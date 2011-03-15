module Decaf.Translator where
import Data.Char
import Decaf.Data.SymbolTable
import Decaf.IR.Class
import Decaf.IR.AST
import Decaf.IR.LIR
import Decaf.Data.Tree
import Decaf.Data.Zipper

-- TODO
-- handle translateMethod: function pre-call
--        translateMethod: function prologue
--        translateMethod: function epilogue
--        translateMethod: function post-return
--        translateMethod: implement stack wind/unwind
--        translateLiteral: figure out if we want to store booleans in 8-byte integers
--        translateLocation: add runtime bounds check on array
--        translateLocation: offset must include previous arrays length * size
--        ??: runtime check: control falling off edge?

data Namespace = Namespace
    { temp :: Int
    , label :: Int
    , scope :: [Int]
    , blockindex :: [Int]
    }

mkNamespace :: Namespace
mkNamespace = Namespace 4096 0 [] [0] -- ^ need to replace 4096 with ending point of SymbolTable.numberTree

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

--checked
-- | Given a SymbolTree, Translate a DecafProgram into an LIRProgram
translateProgram ::  SymbolTree -> DecafProgram -> Translator CGProgram
translateProgram st program =
    do ns <- getNS
       return $ CGProgram (LIRLabel $ "START") (units ns)
  where
    units ns = translate (mapM (translateMethod st) (methods program)) ns

--checked
-- | Given a SymbolTree, Translate a DecafMethod into an LIRUnit
-- TODO implement stack wind/unwind
translateMethod :: SymbolTree -> DecafMethod -> Translator CGUnit
translateMethod st method =
    do ns <- getNS
       return (case symLookup (methodID method) (content $ tree st) of
        Just (index, MethodRec _ (label, count)) ->
            CGUnit (methodlabel count) (translateBody (select (index - firstMethodIndex) st) ns) -- ^ label this method and select the corresponding nested SymbolTree
        _ -> CGUnit (LIRLabel ("Translator.hs:90 Invalid SymbolTable; could not find '" ++ methodID method ++ "' symbol")) []) -- ^ should not happen
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

--checked
-- | Given a SymbolTree, Translate a single DecafStatement into [LIRInst]
translateStm :: SymbolTree -> DecafStm -> Translator [CGInst]
translateStm st (DecafAssignStm loc op expr _) =
    do (instructions1, LIRRegOperand reg) <- translateLocation st loc
       (instructions2, operand) <- translateExpr st expr
       (instructions3, operand2) <- expr' reg operand
       return (instructions1
           ++ instructions2
           ++ instructions3
           ++ [CGLIRInst $ LIRRegAssignInst reg (LIROperExpr operand2)])
  where
    expr' reg oper = case op of
                         DecafEq _ -> return ([], oper)
                         DecafPlusEq _ -> do t <- incTemp
                                             let bexpr = LIRBinExpr (LIRRegOperand reg) LADD oper
                                                 s = SREG (show t)
                                             return ([CGLIRInst $ LIRRegAssignInst s bexpr], LIRRegOperand $ s)

                         DecafMinusEq _ -> do t <- incTemp
                                              let bexpr = LIRBinExpr (LIRRegOperand reg) LSUB oper
                                                  s = SREG (show t)
                                              return ([CGLIRInst $ LIRRegAssignInst s bexpr], LIRRegOperand $ s)

--checked below
-- | Given a SymbolTree, Translate a single DecafMethodStm into [LIRInst]
translateStm st (DecafMethodStm mc _) =
    do (instructions, operand) <- translateMethodCall st mc
       return instructions

--checked
translateStm st (DecafIfStm expr block melse _) =
     do ns <- getNS
        (instructions, LIROperRelExpr relexpr) <- translateRelExpr st expr ns
        l <- incTemp
        trueblock <- translateBlock st block
        elseblock <- case melse of 
                       Just b -> translateBlock st b
                       Nothing -> return []
        return (instructions
            ++ [CGIf relexpr l -- numbering based on registers so that short-circuit evaluation doesn't clash (SCE takes new if label from expr number)
                trueblock elseblock])

{-                         (trueblock++[CGLIRInst $ LIRLabelInst (endLabel l)]) 
                         (elseblock++[CGLIRInst (LIRJumpLabelInst (endLabel l)),
                                                CGLIRInst (LIRLabelInst (trueLabel l))])]) -}

{-translateStm st (DecafIfStm expr block Nothing _) =
    do ns <- getNS
       (instructions, relexpr) <- translateRelExpr st expr ns
       l <- incLabel
       trueblock <- withScope l $ translateBlock st block
       return (instructions
           ++ [LIRIfInst relexpr (trueLabel l)]
           ++ [LIRJumpLabelInst (endLabel l)]
           ++ [LIRLabelInst (trueLabel l)]
           ++ trueblock
           ++ [LIRLabelInst (endLabel l)])
-}

--checked
translateStm st (DecafForStm ident expr expr' block _) =
    do  l <- incLabel
        (instructions, operand) <- translateExpr st expr
        ns <- getNS
        (instructions2, relexpr) <- translateRelExpr st expr' ns
        forblock <- withScope l $ translateBlock st block
        let ivarlabel = (case symLookup ident ((content . tree) st) of
                            Just (_, (VarRec _ label)) -> show label
                            Nothing -> error "Translator.hs:151 Invalid SymbolTable; could not find a valid symbol for'" ++ show ident ++ "'")
        return (instructions
            ++ [CGLIRInst $ LIRRegAssignInst (SREG ivarlabel) (LIROperExpr operand)]
            ++ [CGLIRInst $ LIRLabelInst (loopLabel l)]
            ++ instructions2
            ++ [CGLIRInst $ LIRIfInst relexpr (trueLabel l)]
            ++ [CGLIRInst $ LIRJumpLabelInst (endLabel l)]
            ++ [CGLIRInst $ LIRLabelInst (trueLabel l)]
            ++ forblock
            ++ [CGLIRInst $ LIRJumpLabelInst (loopLabel l)]
            ++ [CGLIRInst $ LIRLabelInst (endLabel l)])

--checked
translateStm st (DecafRetStm (Just expr) _) =
    do (instructions, operand) <- translateExpr st expr
       return (instructions
           ++ [CGLIRInst $ LIRRetOperInst operand])

translateStm st (DecafRetStm Nothing _) =
    return [CGLIRInst $ LIRRetInst]

translateStm st (DecafBreakStm _) =
    do l <- getScope
       return [CGLIRInst $ LIRJumpLabelInst (endLabel l)]

translateStm st (DecafContStm _) =
    do l <- getScope
       return [CGLIRInst $ LIRJumpLabelInst (loopLabel l)]

translateStm st (DecafBlockStm block _) =
    translateBlock st block

translateBlock :: SymbolTree -> DecafBlock -> Translator [CGInst]
translateBlock st (DecafBlock _ [] _) = return []
translateBlock st block =
    withBlock (do b <- getBlock
                  ns <- getNS
                  return $ translateBlockBody (select b st) ns)
  where
    translateBlockBody st' ns = concat $ translate (mapM (translateStm st') (blockStms block)) ns

--checked
translateRelExpr :: SymbolTree -> DecafExpr -> Namespace -> Translator ([CGInst], LIRRelExpr)
translateRelExpr st expr ns = 
    case translate (translateExpr st expr) ns of
      ([], oper@(LIRRegOperand {})) -> return ([], LIROperRelExpr oper)
      ([], oper@(LIRIntOperand {})) -> return ([], LIROperRelExpr oper)
      (instructions, oper) -> case last instructions of
                                CGLIRInst (LIRRegAssignInst s (LIRBinExpr operand (LIRBinRelOp o) operand')) ->
                                    return (instructions, LIROperRelExpr oper)
                                                            
                                CGLIRInst (LIRRegAssignInst s (LIRUnExpr LNOT operand)) ->
                                    return (instructions, LIROperRelExpr oper)

                                CGLIRInst (LIRRegAssignInst s (LIROperExpr operand)) ->
                                    return (instructions, LIROperRelExpr oper)
                                CGExprInst {} -> 
                                    return (instructions, LIROperRelExpr oper)
                                

                                _ -> return ([CGLIRInst $ LIRLabelInst $ LIRLabel $ "Translator.hs:208 Invalid Expression tree; not of type relExpr"], LIROperRelExpr $ LIRRegOperand RAX)

--checked
translateExpr :: SymbolTree -> DecafExpr -> Translator ([CGInst], LIROperand)
translateExpr st (DecafLocExpr loc _) =
    translateLocation st loc

translateExpr st (DecafMethodExpr mc _) =
    translateMethodCall st mc

translateExpr st (DecafLitExpr lit _) =
    do operand <- translateLiteral st lit
       return ([], operand)

--checked
translateExpr st (DecafBinExpr expr binop expr' _) =
    do (lopis, op1) <- translateExpr st expr
       (ropis, op2) <- translateExpr st expr'
       t <- incTemp
       let s' = SREG (show t)
           s = LIRRegOperand $ s'
           prelude = lopis ++ ropis
           lexp = case lopis of
                    [l@(CGExprInst {})] -> l
                    l@(x:xs) -> CGExprInst (CGFlatExpr l op1)
                    [] -> CGExprInst (CGFlatExpr [] op1)
           rexp = case ropis of
                    [l@(CGExprInst {})] -> l
                    l@(x:xs) -> CGExprInst (CGFlatExpr l op2)
                    [] -> CGExprInst (CGFlatExpr [] op2)
           binexpr = case binop of
                       DecafBinCondOp (DecafAndOp{}) _ -> [CGExprInst (CGLogExpr lexp LAND rexp s')]
                       DecafBinCondOp (DecafOrOp{}) _  -> [CGExprInst (CGLogExpr lexp LOR rexp s')]
                       otherwise -> [mergeFlatExprs2 (mergeFlatExprs lexp rexp s)
                                      [CGLIRInst $ LIRRegAssignInst s' (LIRBinExpr op1 binop' op2)] s]
                                     --s

       return (binexpr, s)
  where
    mergeFlatExprs (CGExprInst (CGFlatExpr insts1 _)) (CGExprInst (CGFlatExpr insts2 _ )) s = 
        CGExprInst (CGFlatExpr (insts1 ++ insts2) s)
    mergeFlatExprs2 (CGExprInst (CGFlatExpr insts1 _)) insts2@(x:xs) s = 
        CGExprInst (CGFlatExpr (insts1 ++ insts2) s)
{-    mergeFlatExprs insts1@(x:xs) [CGExprInst (CGFlatExpr insts2)] s = 
        CGExprInst $ CGFlatExpr (insts1 ++ insts2) s
    mergeFlatExprs insts1@(x:xs) insts2@(y:ys) s = 
        CGExprInst $ CGFlatExpr (insts1 ++ insts2) s
-}    
        
    binop' = case binop of
                 DecafBinArithOp (DecafPlusOp _) _ -> LADD
                 DecafBinArithOp (DecafMinOp _) _ -> LSUB
                 DecafBinArithOp (DecafMulOp _) _ -> LMUL
                 DecafBinArithOp (DecafDivOp _) _ -> LDIV
                 DecafBinArithOp (DecafModOp _) _ -> LMOD
--                 DecafBinCondOp (DecafAndOp _) _ -> LAND
--                 DecafBinCondOp (DecafOrOp _) _ -> LOR  -- shouldn't be needed...
                 DecafBinRelOp (DecafLTOp _) _ -> LIRBinRelOp LLT
                 DecafBinRelOp (DecafGTOp _) _ -> LIRBinRelOp LGT
                 DecafBinRelOp (DecafLTEOp _) _ -> LIRBinRelOp LLTE
                 DecafBinRelOp (DecafGTEOp _) _ -> LIRBinRelOp LGTE
                 DecafBinEqOp (DecafEqOp _) _ -> LIRBinRelOp LEQ
                 DecafBinEqOp (DecafNEqOp _) _ -> LIRBinRelOp LNEQ

translateExpr st (DecafNotExpr expr _) =
    do t <- incTemp
       (instructions, operand) <- translateExpr st expr
       let unexpr = LIRUnExpr LNOT operand
           s = SREG (show t)
       return (instructions ++ [CGLIRInst $ LIRRegAssignInst s unexpr], LIRRegOperand s)

translateExpr st (DecafMinExpr expr _) =
    do t <- incTemp
       (instructions, operand) <- translateExpr st expr
       let unexpr = LIRUnExpr LNEG operand
           s = SREG (show t)
       return (instructions ++ [CGLIRInst $ LIRRegAssignInst s unexpr], LIRRegOperand s)

translateExpr st (DecafParenExpr expr _) =
    do t <- incTemp
       (instructions, operand) <- translateExpr st expr
       let o = LIROperExpr operand 
           s = SREG (show t)
       return (instructions ++ [CGLIRInst $ LIRRegAssignInst s o], LIRRegOperand s)

--checked
translateExpr _ _ =
    return ([CGLIRInst $ LIRLabelInst (LIRLabel $ "Translator.hs:212 Invalid expression tree")], LIRIntOperand $ LIRInt 0)

--checked
translateLiteral :: SymbolTree -> DecafLiteral -> Translator LIROperand
translateLiteral _ (DecafIntLit i _) = return $ LIRIntOperand $ LIRInt (readDecafInteger i)
translateLiteral _ (DecafBoolLit b _) = return $ LIRIntOperand $ LIRInt (if b then 1 else 0) -- ^ TODO figure out if we want to store booleans in 8-byte integers
translateLiteral _ (DecafCharLit c _) = return $ LIRIntOperand $ LIRInt (ord c)

--checked
translateMethodCall :: SymbolTree -> DecafMethodCall -> Translator ([CGInst], LIROperand)
translateMethodCall st meth =
    do t <- incTemp
       return ([CGLIRInst $ LIRCallAssignInst (SREG $ show t) func (SREG "ip")], LIRRegOperand $ SREG $ show t)
  where
    func = case meth of
               (DecafPureMethodCall {}) -> LIRProcLabel (case globalSymLookup (methodCallID meth) st of
                                                       Just (MethodRec _ (label, count)) -> methodLabel (methodCallID meth) count
                                                       _ -> "Translator.hs:120 Invalid SymbolTable; could not find a valid symbol for'" ++ (methodCallID meth) ++ "'")
               (DecafMethodCallout {})-> LIRProcLabel (methodCalloutID meth)

-- TODO add runtime bounds check on array
translateLocation :: SymbolTree -> DecafLoc -> Translator ([CGInst], LIROperand)
translateLocation st loc =
    (case globalSymLookup (ident loc) st of
        Just (VarRec _ sr) -> return ([], LIRRegOperand $ SREG (show sr))
        Just (ArrayRec arr o) -> incTemp >>= \t -> (do (prep, index) <- translateExpr st (arrLocExpr loc)
                                                       return (prep ++ [CGLIRInst $ LIRLoadInst (SREG $ show t) (memaddr arr o index)], LIRRegOperand (SREG $ show t))) -- ^ replace 0 with the array's real index
        _ -> return ([CGLIRInst $ LIRLabelInst (LIRLabel $ "Translator.hs:235 Invalid SymbolTable; could not find '" ++ ident loc ++ "' symbol in\n" ++ show st)], LIRRegOperand $ SREG "--"))
  where
    memaddr (DecafArr ty _ len _) offset (LIRIntOperand (LIRInt index)) =
        let arrlen = readDecafInteger len
            size = (case ty of
                        DecafInteger -> 8 -- ^ size in bytes
                        DecafBoolean -> 8
                        _ -> error "Translate.hs:217 Array cannot have type void")
        in LIRRegOffMemAddr (SREG "gp") (LIRInt (offset + size*index)) (LIRInt size) -- ^ TODO offset must include previous arrays length * size

methodLabel :: String -> Int -> String
methodLabel methodname c = "__func__" ++ show c ++ "__" ++ methodname

loopLabel :: Int -> LIRLabel
loopLabel l = LIRLabel $ "LLOOP" ++ show l

endLabel :: Int -> LIRLabel
endLabel l = LIRLabel $ "LEND" ++ show l

trueLabel :: Int -> LIRLabel
trueLabel l = LIRLabel $ "LTRUE" ++ show l
