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
--        handle expr
--        calculate offset address for arrays

data Namespace = Namespace
    { temp :: Int
    , label :: Int
    , scope :: [Int]
    , blockindex :: [Int]
    }

mkNamespace :: Namespace
mkNamespace = Namespace 0 0 [] []

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

withScope :: [Int] -> Translator a -> Translator a
withScope scope m = Translator (\(Namespace t l s b) ->
    let (a, Namespace t' l' _ b') = runTranslator m (Namespace t l scope b)
    in (a, Namespace t' l' s b'))

getScope :: Translator [Int]
getScope = Translator (\ns@(Namespace _ _ s _) -> (s, ns))

withBlock :: [Int] -> Translator a -> Translator a
withBlock block m = Translator (\(Namespace t l s b) ->
    let (a, Namespace t' l' s' _) = runTranslator m (Namespace t l s block)
    in (a, Namespace t' l' s' b))

getBlock :: Translator [Int]
getBlock = Translator (\ns@(Namespace _ _ _ b) -> (b, ns))

-- | Given a SymbolTree, Translate a DecafProgram into an LIRProgram
translateProgram ::  SymbolTree -> DecafProgram -> Translator LIRProgram
translateProgram st program =
    do i <- incTemp
       ns <- getNS
       return $ LIRProgram (LIRLabel $ "L" ++ show i) (units ns)
  where
    units ns = translate (mapM (translateMethod st) (methods program)) ns

-- | Given a SymbolTree, Translate a DecafMethod into an LIRUnit
translateMethod :: SymbolTree -> DecafMethod -> Translator LIRUnit
translateMethod st method =
    do ns <- getNS
       return (case symLookup (methodID method) (content $ tree st) of
        Just (index, MethodRec _ (label, count)) ->
            LIRUnit (LIRLabel (label ++ show count)) (translateBody (select (rindex index) st) ns) -- ^ label this method and select the corresponding nested SymbolTree
        _ -> LIRUnit (LIRLabel ("Translator.hs:71 Invalid SymbolTable; could not find '" ++ methodID method ++ "' symbol")) []) -- ^ should not happen
  where
    translateBody st' ns = concat $ translate (mapM (translateStm st') (blockStms $ methodBody method)) ns -- ^ join all translated statements together
    rindex index = (length . children . tree) st - index - 1

-- | Given a SymbolTree, Translate a single DecafStatement into [LIRInst]
translateStm :: SymbolTree -> DecafStm -> Translator [LIRInst]
translateStm st (DecafAssignStm loc op expr _) =
    return [lir]
  where
    lir = case globalSymLookup (ident loc) st of
              Just (VarRec _ sr) -> LIRRegAssignInst (SREG (show sr)) (LIROperExpr $ translateExpr st expr)
              Just (ArrayRec _ o) -> LIRStoreInst (LIRRegOffMemAddr (SREG "gp") (LIRInt o) (LIRInt 8)) (translateExpr st expr)
              _ -> LIRLabelInst (LIRLabel $ "Translator.hs:84 Invalid SymbolTable; could not find '" ++ ident loc ++ "' symbol")

translateStm st (DecafMethodStm (DecafPureMethodCall mid args _) _) =
    return [LIRCallInst (LIRCall func (SREG "ip"))]
  where
    func = LIRProcLabel (case globalSymLookup mid st of
                            Just (MethodRec _ (label, count)) -> label ++ show count
                            _ -> "Translator.hs:91 Invalid SymbolTable; could not find a valid symbol for'" ++ mid ++ "'")

translateStm st (DecafMethodStm (DecafMethodCallout mid args _) _) =
    return [LIRCallInst (LIRCall func (SREG "ip"))]
  where
    func = LIRProcLabel mid
    
translateStm st (DecafIfStm expr block (Just elseblock) _) =
     do l <- incLabel
        elseblock <- translateBlock st elseblock
        trueblock <- translateBlock st block
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
        forblock <- translateBlock st block
        let looplabel = LIRLabel $ "LOOP" ++ show l
            truelabel = LIRLabel $ "TRUE" ++ show l
            endlabel = LIRLabel $ "END" ++ show l
        return ([LIRRegAssignInst (SREG "0") (LIROperExpr $ translateExpr st expr)] -- TODO pullout of st
            ++ [LIRLabelInst looplabel]
            ++ [LIRIfInst (translateRelExpr st expr') truelabel]
            ++ [LIRJumpLabelInst endlabel]
            ++ [LIRLabelInst truelabel]
            ++ forblock
            ++ [LIRJumpLabelInst looplabel]
            ++ [LIRLabelInst endlabel])

translateStm st (DecafIfStm expr block Nothing _) =
    do l <- incLabel
       trueblock <- translateBlock st block
       let truelabel = LIRLabel $ "LTRUE" ++ show l
           endlabel = LIRLabel $ "LEND" ++ show l
       return ([LIRIfInst (translateRelExpr st expr) truelabel]
           ++ [LIRJumpLabelInst endlabel]
           ++ [LIRLabelInst truelabel]
           ++ trueblock
           ++ [LIRLabelInst endlabel])

translateStm st (DecafRetStm (Just expr) _) =
    return [LIRRetOperInst (translateExpr st expr)]

translateStm st (DecafRetStm Nothing _) =
    return [LIRRetInst]

translateStm st (DecafBreakStm _) =
    do let endlabel = LIRLabel "TODO__BREAK__STATEMENT"
       return [LIRJumpLabelInst endlabel]

translateStm st (DecafContStm _) =
    do let looplabel = LIRLabel "TODO__CONT__STATEMENT"
       return [LIRJumpLabelInst looplabel]

translateStm st (DecafBlockStm block _) =
    translateBlock st block

translateBlock :: SymbolTree -> DecafBlock -> Translator [LIRInst]
translateBlock st block =
    do ns <- getNS
       return $ translateBody st ns
  where
    translateBody st' ns = concat $ translate (mapM (translateStm st') (blockStms block)) ns

translateRelExpr :: SymbolTree -> DecafExpr -> LIRRelExpr
translateRelExpr st expr = LIROperRelExpr $ LIRRegOperand $ RAX

translateExpr :: SymbolTree -> DecafExpr -> LIROperand
translateExpr st expr = LIRRegOperand $ RAX
