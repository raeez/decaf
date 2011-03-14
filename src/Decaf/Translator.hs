module Decaf.Translator where
import Decaf.Data.SymbolTable
import Decaf.IR.Class
import Decaf.IR.AST
import Decaf.IR.LIR
import Decaf.Data.Tree
import Decaf.Data.Zipper

-- TODO
-- handle function pre-call
--        function prologue
--        function epilogue
--        function post-return
--        handle cfg etc.
--        handle relExpr
--        calculate offset address for arrays (negative count + size)

data Namespace = Namespace
    { temp :: Int
    , label :: Int
    , scope :: [Int]
    , blockindex :: [Int]
    }

mkNamespace :: Namespace
mkNamespace = Namespace 0 0 [] [0]

newtype Translator a = Translator
    { runTranslator :: Namespace -> (a, Namespace) }

instance Monad Translator where
    return a = Translator (\s -> (a, s))
    m >>= f = Translator (\s ->
                let (a, ns) = runTranslator m s
                in runTranslator (f a) ns)

translate comp ns = fst $ runTranslator comp ns

getNS :: Translator Namespace
getNS = Translator (\ns -> (ns, ns))

incTemp :: Translator Int
incTemp = Translator (\(Namespace t l s b) -> (t, Namespace (t+1) l s b))

getTemp :: Translator Int
getTemp = Translator (\ns@(Namespace t _ _ _) -> (t, ns))

incLabel :: Translator Int
incLabel = Translator (\(Namespace t l s b) -> (l, Namespace t (l+1) s b))

getLabel :: Translator Int
getLabel = Translator (\ns@(Namespace _ l _ _) -> (l, ns))

getScope :: Translator Int
getScope = Translator (\ns@(Namespace _ _ s _) -> (last s, ns))

getBlock :: Translator Int
getBlock = Translator (\ns@(Namespace _ _ _ b) -> ((last . init) b, ns))

getNesting :: Translator [Int]
getNesting = Translator (\ns@(Namespace _ _ _ b) -> (b, ns))

withScope :: Int -> Translator a -> Translator a
withScope label m = Translator (\(Namespace t l s b) ->
    let (a, Namespace t' l' _ b') = runTranslator m (Namespace t l (s ++ [label]) b)
    in (a, Namespace t' l' s b'))

withBlock :: Translator a -> Translator a
withBlock m = Translator (\(Namespace t l s b) ->
    let b' = init b ++ [last b + 1]
        (a, Namespace t' l' s' _) = runTranslator m (Namespace t l s (b ++ [0]))
    in (a, Namespace t' l' s' b'))

-- | Given a SymbolTree, Translate a DecafProgram into an LIRProgram
translateProgram ::  SymbolTree -> DecafProgram -> Translator LIRProgram
translateProgram st program =
    do ns <- getNS
       return $ LIRProgram (LIRLabel $ "START") (units ns)
  where
    units ns = translate (mapM (translateMethod st) (methods program)) ns

-- | Given a SymbolTree, Translate a DecafMethod into an LIRUnit
translateMethod :: SymbolTree -> DecafMethod -> Translator LIRUnit
translateMethod st method =
    do ns <- getNS
       return (case symLookup (methodID method) (content $ tree st) of
        Just (index, MethodRec _ (label, count)) ->
            LIRUnit (methodlabel count) (translateBody (select (index - firstMethodIndex) st) ns) -- ^ label this method and select the corresponding nested SymbolTree
        _ -> LIRUnit (LIRLabel ("Translator.hs:90 Invalid SymbolTable; could not find '" ++ methodID method ++ "' symbol")) []) -- ^ should not happen
  where
    methodlabel c = if methodname == "main"
                      then LIRLabel (methodname)
                      else LIRLabel (methodLabel methodname c)
      where
        methodname = methodID method
    translateBody st' ns = concat $ translate (mapM (translateStm st') (blockStms $ methodBody method)) ns -- ^ join all translated statements together
    firstMethodIndex = countStatics $ (symbolRecords . content . tree) st
    countStatics [] = 0
    countStatics (MethodRec _ _:xs) = 0
    countStatics (_:xs) = 1 + (countStatics xs)

-- | Given a SymbolTree, Translate a single DecafStatement into [LIRInst]
-- TODO add runtime bounds check on array
translateStm :: SymbolTree -> DecafStm -> Translator [LIRInst]
translateStm st (DecafAssignStm loc op expr _) =
    do (instructions, oper) <- translateExpr st expr
       return (instructions
           ++ [lir oper])
  where
    lir operand = case globalSymLookup (ident loc) st of
                      Just (VarRec _ sr) -> LIRRegAssignInst (SREG (show sr)) (LIROperExpr operand)
                      Just (ArrayRec arr o) -> LIRStoreInst (memaddr arr o) operand
                      _ -> LIRLabelInst (LIRLabel $ "Translator.hs:107 Invalid SymbolTable; could not find '" ++ ident loc ++ "' symbol in\n" ++ show st)

    memaddr (DecafArr ty _ len _) offset = let arrlen = readDecafInteger len
                                               size = (case ty of
                                                      DecafInteger -> 8
                                                      DecafBoolean -> 1
                                                      _ -> error "Translate.hs:112 Array cannot have type void")
                                      in LIRRegOffMemAddr (SREG "gp") (LIRInt offset) (LIRInt size) -- TODO offset must include previous arrays length * size

translateStm st (DecafMethodStm (DecafPureMethodCall mid args _) _) =
    return [LIRCallInst (LIRCall func (SREG "ip"))]
  where
    func = LIRProcLabel (case globalSymLookup mid st of
                            Just (MethodRec _ (label, count)) -> methodLabel mid count
                            _ -> "Translator.hs:120 Invalid SymbolTable; could not find a valid symbol for'" ++ mid ++ "'")

translateStm st (DecafMethodStm (DecafMethodCallout mid args _) _) =
    return [LIRCallInst (LIRCall func (SREG "ip"))]
  where
    func = LIRProcLabel mid
    
translateStm st (DecafIfStm expr block (Just elseblock) _) =
     do l <- incLabel
        trueblock <- translateBlock st block
        elseblock <- translateBlock st elseblock
        let truelabel = LIRLabel $ "LTRUE" ++ show l
            endlabel = LIRLabel $ "LEND" ++ show l
        return ([LIRIfInst (translateRelExpr st expr) truelabel]
            ++ elseblock
            ++ [LIRJumpLabelInst endlabel]
            ++ [LIRLabelInst truelabel]
            ++ trueblock
            ++ [LIRLabelInst endlabel])

translateStm st (DecafForStm ident expr expr' block _) =
    do  l <- incLabel
        (instructions, operand) <- translateExpr st expr
        forblock <- withScope l $ translateBlock st block
        let ivarlabel = (case symLookup ident ((content . tree) st) of
                            Just (_, (VarRec _ label)) -> show label
                            Nothing -> error "Translator.hs:153 Invalid SymbolTable; could not find a valid symbol for'" ++ show ident ++ "'")
        return (instructions
            ++ [LIRRegAssignInst (SREG ivarlabel) (LIROperExpr operand)]
            ++ [LIRLabelInst (loopLabel l)]
            ++ [LIRIfInst (translateRelExpr st expr') (trueLabel l)]
            ++ [LIRJumpLabelInst (endLabel l)]
            ++ [LIRLabelInst (trueLabel l)]
            ++ forblock
            ++ [LIRJumpLabelInst (loopLabel l)]
            ++ [LIRLabelInst (endLabel l)])

translateStm st (DecafIfStm expr block Nothing _) =
    do l <- incLabel
       trueblock <- withScope l $ translateBlock st block
       return ([LIRIfInst (translateRelExpr st expr) (trueLabel l)]
           ++ [LIRJumpLabelInst (endLabel l)]
           ++ [LIRLabelInst (trueLabel l)]
           ++ trueblock
           ++ [LIRLabelInst (endLabel l)])

translateStm st (DecafRetStm (Just expr) _) =
    do (instructions, operand) <- translateExpr st expr
       return (instructions
           ++ [LIRRetOperInst operand])

translateStm st (DecafRetStm Nothing _) =
    return [LIRRetInst]

translateStm st (DecafBreakStm _) =
    do l <- getScope
       return [LIRJumpLabelInst (endLabel l)]

translateStm st (DecafContStm _) =
    do l <- getScope
       return [LIRJumpLabelInst (loopLabel l)]

translateStm st (DecafBlockStm block _) =
    translateBlock st block

translateBlock :: SymbolTree -> DecafBlock -> Translator [LIRInst]
translateBlock st (DecafBlock _ [] _) = return []
translateBlock st block =
    withBlock (do b <- getBlock
                  ns <- getNS
                  return $ translateBlockBody (select b st) ns)
  where
    translateBlockBody st' ns = concat $ translate (mapM (translateStm st') (blockStms block)) ns

translateRelExpr :: SymbolTree -> DecafExpr -> LIRRelExpr
translateRelExpr st expr = LIROperRelExpr $ LIRRegOperand $ RAX

translateExpr :: SymbolTree -> DecafExpr -> Translator ([LIRInst], LIROperand)
translateExpr st expr = return ([], LIRRegOperand RAX)

methodLabel :: String -> Int -> String
methodLabel methodname c = "__func__" ++ show c ++ "__" ++ methodname

loopLabel :: Int -> LIRLabel
loopLabel l = LIRLabel $ "LLOOP" ++ show l

endLabel :: Int -> LIRLabel
endLabel l = LIRLabel $ "LEND" ++ show l

trueLabel :: Int -> LIRLabel
trueLabel l = LIRLabel $ "LTRUE" ++ show l
