module Decaf.Translator where
import Data.Char
import Decaf.IR.Class
import Decaf.IR.AST
import Decaf.IR.LIR
import Decaf.IR.SymbolTable
import Decaf.IR.ControlFlowGraph
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
translateProgram ::  SymbolTree -> DecafProgram -> Translator CFGProgram
translateProgram st program =
    do ns <- getNS
       return $ CFGProgram (LIRLabel $ "START") (units ns)
  where
    units ns = translate (mapM (translateMethod st) (methods program)) ns

--checked
-- | Given a SymbolTree, Translate a DecafMethod into an LIRUnit
-- TODO implement stack wind/unwind
translateMethod :: SymbolTree -> DecafMethod -> Translator CFGUnit
translateMethod st method =
    do ns <- getNS
       return (case symLookup (methodID method) (content $ tree st) of
        Just (index, MethodRec _ (label, count)) ->
            CFGUnit (methodlabel count) (translateBody (select (index - firstMethodIndex) st) ns) -- ^ label this method and select the corresponding nested SymbolTree
        _ -> CFGUnit (LIRLabel ("Translator.hs:90 Invalid SymbolTable; could not find '" ++ methodID method ++ "' symbol")) []) -- ^ should not happen
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
translateStm :: SymbolTree -> DecafStm -> Translator [CFGInst]
translateStm st (DecafAssignStm loc op expr _) =
    do (instructions1, LIRRegOperand reg) <- translateLocation st loc
       (instructions2, operand) <- translateExpr st expr
       (instructions3, operand2) <- expr' reg operand
       return (instructions1
           ++ instructions2
           ++ instructions3
           ++ [CFGLIRInst $ LIRRegAssignInst reg (LIROperExpr operand2)])
  where
    expr' reg oper = case op of
                         DecafEq _ -> return ([], oper)
                         DecafPlusEq _ -> do t <- incTemp
                                             let bexpr = LIRBinExpr (LIRRegOperand reg) LADD oper
                                                 s = SREG (show t)
                                             return ([CFGLIRInst $ LIRRegAssignInst s bexpr], LIRRegOperand $ s)

                         DecafMinusEq _ -> do t <- incTemp
                                              let bexpr = LIRBinExpr (LIRRegOperand reg) LSUB oper
                                                  s = SREG (show t)
                                              return ([CFGLIRInst $ LIRRegAssignInst s bexpr], LIRRegOperand $ s)

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
            ++ [CFGIf relexpr l -- numbering based on registers so that short-circuit evaluation doesn't clash (SCE takes new if label from expr number)
                trueblock elseblock])

{-                         (trueblock++[CFGLIRInst $ LIRLabelInst (endLabel l)]) 
                         (elseblock++[CFGLIRInst (LIRJumpLabelInst (endLabel l)),
                                                CFGLIRInst (LIRLabelInst (trueLabel l))])]) -}

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
            ++ [CFGLIRInst $ LIRRegAssignInst (SREG ivarlabel) (LIROperExpr operand)]
            ++ [CFGLIRInst $ LIRLabelInst (loopLabel l)]
            ++ instructions2
            ++ [CFGLIRInst $ LIRIfInst relexpr (trueLabel l)]
            ++ [CFGLIRInst $ LIRJumpLabelInst (endLabel l)]
            ++ [CFGLIRInst $ LIRLabelInst (trueLabel l)]
            ++ forblock
            ++ [CFGLIRInst $ LIRJumpLabelInst (loopLabel l)]
            ++ [CFGLIRInst $ LIRLabelInst (endLabel l)])

--checked
translateStm st (DecafRetStm (Just expr) _) =
    do (instructions, operand) <- translateExpr st expr
       return (instructions
           ++ [CFGLIRInst $ LIRRetOperInst operand])

translateStm st (DecafRetStm Nothing _) =
    return [CFGLIRInst $ LIRRetInst]

translateStm st (DecafBreakStm _) =
    do l <- getScope
       return [CFGLIRInst $ LIRJumpLabelInst (endLabel l)]

translateStm st (DecafContStm _) =
    do l <- getScope
       return [CFGLIRInst $ LIRJumpLabelInst (loopLabel l)]

translateStm st (DecafBlockStm block _) =
    translateBlock st block

translateBlock :: SymbolTree -> DecafBlock -> Translator [CFGInst]
translateBlock st (DecafBlock _ [] _) = return []
translateBlock st block =
    withBlock (do b <- getBlock
                  ns <- getNS
                  return $ translateBlockBody (select b st) ns)
  where
    translateBlockBody st' ns = concat $ translate (mapM (translateStm st') (blockStms block)) ns

--checked
translateRelExpr :: SymbolTree -> DecafExpr -> Namespace -> Translator ([CFGInst], LIRRelExpr)
translateRelExpr st expr ns = 
    case translate (translateExpr st expr) ns of
      ([], oper@(LIRRegOperand {})) -> return ([], LIROperRelExpr oper)
      ([], oper@(LIRIntOperand {})) -> return ([], LIROperRelExpr oper)
      (instructions, oper) -> case last instructions of
                                CFGLIRInst (LIRRegAssignInst s (LIRBinExpr operand (LIRBinRelOp o) operand')) ->
                                    return (instructions, LIROperRelExpr oper)
                                                            
                                CFGLIRInst (LIRRegAssignInst s (LIRUnExpr LNOT operand)) ->
                                    return (instructions, LIROperRelExpr oper)

                                CFGLIRInst (LIRRegAssignInst s (LIROperExpr operand)) ->
                                    return (instructions, LIROperRelExpr oper)
                                CFGExprInst {} -> 
                                    return (instructions, LIROperRelExpr oper)
                                

                                _ -> return ([CFGLIRInst $ LIRLabelInst $ LIRLabel $ "Translator.hs:208 Invalid Expression tree; not of type relExpr"], LIROperRelExpr $ LIRRegOperand RAX)

--checked
translateExpr :: SymbolTree -> DecafExpr -> Translator ([CFGInst], LIROperand)
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
                    [l@(CFGExprInst {})] -> l
                    l@(x:xs) -> CFGExprInst (CFGFlatExpr l op1)
                    [] -> CFGExprInst (CFGFlatExpr [] op1)
           rexp = case ropis of
                    [l@(CFGExprInst {})] -> l
                    l@(x:xs) -> CFGExprInst (CFGFlatExpr l op2)
                    [] -> CFGExprInst (CFGFlatExpr [] op2)
           binexpr = case binop of
                       DecafBinCondOp (DecafAndOp{}) _ -> [CFGExprInst (CFGLogExpr lexp LAND rexp s')]
                       DecafBinCondOp (DecafOrOp{}) _  -> [CFGExprInst (CFGLogExpr lexp LOR rexp s')]
                       otherwise -> [mergeFlatExprs2 (mergeFlatExprs lexp rexp s)
                                      [CFGLIRInst $ LIRRegAssignInst s' (LIRBinExpr op1 binop' op2)] s]
                                     --s

       return (binexpr, s)
  where
    mergeFlatExprs (CFGExprInst (CFGFlatExpr insts1 _)) (CFGExprInst (CFGFlatExpr insts2 _ )) s = 
        CFGExprInst (CFGFlatExpr (insts1 ++ insts2) s)
    mergeFlatExprs2 (CFGExprInst (CFGFlatExpr insts1 _)) insts2@(x:xs) s = 
        CFGExprInst (CFGFlatExpr (insts1 ++ insts2) s)
{-    mergeFlatExprs insts1@(x:xs) [CFGExprInst (CFGFlatExpr insts2)] s = 
        CFGExprInst $ CFGFlatExpr (insts1 ++ insts2) s
    mergeFlatExprs insts1@(x:xs) insts2@(y:ys) s = 
        CFGExprInst $ CFGFlatExpr (insts1 ++ insts2) s
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
       return (instructions ++ [CFGLIRInst $ LIRRegAssignInst s unexpr], LIRRegOperand s)

translateExpr st (DecafMinExpr expr _) =
    do t <- incTemp
       (instructions, operand) <- translateExpr st expr
       let unexpr = LIRUnExpr LNEG operand
           s = SREG (show t)
       return (instructions ++ [CFGLIRInst $ LIRRegAssignInst s unexpr], LIRRegOperand s)

translateExpr st (DecafParenExpr expr _) =
    do t <- incTemp
       (instructions, operand) <- translateExpr st expr
       let o = LIROperExpr operand 
           s = SREG (show t)
       return (instructions ++ [CFGLIRInst $ LIRRegAssignInst s o], LIRRegOperand s)

--checked
translateExpr _ _ =
    return ([CFGLIRInst $ LIRLabelInst (LIRLabel $ "Translator.hs:212 Invalid expression tree")], LIRIntOperand $ LIRInt 0)

--checked
translateLiteral :: SymbolTree -> DecafLiteral -> Translator LIROperand
translateLiteral _ (DecafIntLit i _) = return $ LIRIntOperand $ LIRInt (readDecafInteger i)
translateLiteral _ (DecafBoolLit b _) = return $ LIRIntOperand $ LIRInt (if b then 1 else 0) -- ^ TODO figure out if we want to store booleans in 8-byte integers
translateLiteral _ (DecafCharLit c _) = return $ LIRIntOperand $ LIRInt (ord c)

--checked
translateMethodCall :: SymbolTree -> DecafMethodCall -> Translator ([CFGInst], LIROperand)
translateMethodCall st meth =
    do t <- incTemp
       return ([CFGLIRInst $ LIRCallAssignInst (SREG $ show t) func (SREG "ip")], LIRRegOperand $ SREG $ show t)
  where
    func = case meth of
               (DecafPureMethodCall {}) -> LIRProcLabel (case globalSymLookup (methodCallID meth) st of
                                                       Just (MethodRec _ (label, count)) -> methodLabel (methodCallID meth) count
                                                       _ -> "Translator.hs:120 Invalid SymbolTable; could not find a valid symbol for'" ++ (methodCallID meth) ++ "'")
               (DecafMethodCallout {})-> LIRProcLabel (methodCalloutID meth)

-- TODO add runtime bounds check on array
translateLocation :: SymbolTree -> DecafLoc -> Translator ([CFGInst], LIROperand)
translateLocation st loc =
    (case globalSymLookup (ident loc) st of
        Just (VarRec _ sr) -> return ([], LIRRegOperand $ SREG (show sr))
        Just (ArrayRec arr o) -> incTemp >>= \t -> (do (prep, index) <- translateExpr st (arrLocExpr loc)
                                                       return (prep ++ [CFGLIRInst $ LIRLoadInst (SREG $ show t) (memaddr arr o index)], LIRRegOperand (SREG $ show t))) -- ^ replace 0 with the array's real index
        _ -> return ([CFGLIRInst $ LIRLabelInst (LIRLabel $ "Translator.hs:235 Invalid SymbolTable; could not find '" ++ ident loc ++ "' symbol in\n" ++ show st)], LIRRegOperand $ SREG "--"))
  where
    memaddr (DecafArr ty _ len _) offset (LIRIntOperand (LIRInt index)) =
        let arrlen = readDecafInteger len
            size = (case ty of
                        DecafInteger -> 8 -- ^ size in bytes
                        DecafBoolean -> 8
                        _ -> error "Translate.hs:217 Array cannot have type void")
        in LIRRegOffMemAddr (SREG "gp") (LIRInt (offset + size*index)) (LIRInt size) -- ^ TODO offset must include previous arrays length * size
