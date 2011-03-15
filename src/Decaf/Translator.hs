module Decaf.Translator where
import Data.Char
import Data.List
import Decaf.IR.Class
import Decaf.IR.AST
import Decaf.IR.LIR
import Decaf.IR.SymbolTable
import Decaf.IR.ControlFlowGraph
import Decaf.Data.Tree
import Decaf.Data.Zipper

data Namespace = Namespace
    { temp :: Int
    , label :: Int
    , scope :: [Int]
    , blockindex :: [Int]
    }

mkNamespace :: Int -> Namespace
mkNamespace lastTemp= Namespace lastTemp 0 [] [0]

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
translateProgram ::  SymbolTree -> DecafProgram -> Translator CFGProgram
translateProgram st program =
    do ns <- getNS
       declarations <- mapM translateField (fields program)
       units <- mapM (translateMethod st (concat declarations)) (methods program)
       eUnits <- mapM ($st) exceptionHandlers
       return $ CFGProgram (LIRLabel $ "START") ( units
                                                ++ [CFGUnit exceptionHeader (concat eUnits)])
  where
    translateField (DecafVarField var _) = translateVarDeclaration st var
    translateField (DecafArrField arr _) = return [] -- translateArrDeclaration st arr

-- | Given a SymbolTree, Translate a DecafMethod into an LIRUnit
translateMethod :: SymbolTree -> [CFGInst] -> DecafMethod -> Translator CFGUnit
translateMethod st declarations method =
    do ns <- getNS
       (case symLookup (methodID method) (table st) of
        Just (index, MethodRec _ (label, count)) ->
            do body <- translateBlock (st) (methodBody method)
               prologue <- translateMethodPrologue (st' index) method
               postcall <- translateMethodPostcall st method
               let decs = if (methodID method) == "main" then declarations else []
               return $ CFGUnit (methodlabel count) ([CFGLIRInst LIRTempEnterInst]
                                                  ++ prologue
                                                  ++ decs
                                                  ++ body
                                                  ++ postcall)
        _ -> return $ CFGUnit (LIRLabel ("Translator.hs:translateMethod Invalid SymbolTable; could not find '" ++ methodID method ++ "' symbol")) [])
  where
    st' index = select (index - firstMethodIndex) st
    firstMethodIndex = countStatics $ (symbolRecords . table) st
    countStatics [] = 0
    countStatics (MethodRec _ _:xs) = 0
    countStatics (_:xs) = 1 + (countStatics xs)

    methodlabel c = if methodname == "main"
                      then LIRLabel methodname
                      else LIRLabel (methodLabel methodname c)
      where
        methodname = methodID method

translateMethodPrologue :: SymbolTree -> DecafMethod -> Translator [CFGInst]
translateMethodPrologue st (DecafMethod _ ident args _ _) =
    do let numRegVars = min (length args) 6
           regvars = map genRegVar (zip [RDI, RSI, RDX, RCX, R8, R9] args)
       stackvars <- mapM genStackVar (zip [1..] (drop numRegVars args))
       return (regvars ++ stackvars)
  where
    genRegVar (reg, arg) = CFGLIRInst $ LIRRegAssignInst (symVar arg st) (LIROperExpr $ LIRRegOperand reg)
    genStackVar (index, arg) = do let mem = LIRRegOffMemAddr RBP (LIRInt ((index + 1) * 8)) qword -- ^ [rbp] = old rbp; [rbp + 8] = ret address; [rbp + 16] = first stack param
                                  return $ CFGLIRInst $ LIRLoadInst (symVar arg st) mem

-- | Given a SymbolTree, Translate a single DecafStatement into [CFGInst]
translateStm :: SymbolTree -> DecafStm -> Translator [CFGInst]
translateStm st (DecafAssignStm loc op expr _) =
    do (instructions1, LIRRegOperand reg) <- translateLocation st loc
       (instructions2, operand) <- translateExpr st expr
       --(instructions3, operand2) <- expr' reg operand
       let arrayStore = genArrayStore instructions1
       return (instructions1
           ++ instructions2
           -- ++ instructions3
           ++ [CFGLIRInst $ LIRRegAssignInst reg (LIROperExpr operand)]
           ++ arrayStore)
  where
    genArrayStore [] = []
    genArrayStore instructions = (case last instructions of
                                     CFGLIRInst (LIRLoadInst s memaddr) -> [CFGLIRInst $ LIRStoreInst memaddr (LIRRegOperand s)]
                                     _ -> [])

    expr' reg oper = case op of
                         DecafEq _ -> return ([], oper)
                         DecafPlusEq _ -> do t <- incTemp
                                             let bexpr = LIRBinExpr (LIRRegOperand reg) LADD oper
                                                 s = SREG t
                                             return ([CFGLIRInst $ LIRRegAssignInst s bexpr], LIRRegOperand $ s)

                         DecafMinusEq _ -> do t <- incTemp
                                              let bexpr = LIRBinExpr (LIRRegOperand reg) LSUB oper
                                                  s = SREG t
                                              return ([CFGLIRInst $ LIRRegAssignInst s bexpr], LIRRegOperand $ s)

-- | Given a SymbolTree, Translate a single DecafMethodStm into [CFGInst]
translateStm st (DecafMethodStm mc _) =
    do (instructions, operand) <- translateMethodCall st mc
       return instructions

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

translateStm st (DecafForStm ident expr expr' block _) =
    do  l <- incLabel
        (instructions, operand) <- translateExpr st expr
        ns <- getNS
        (instructions2, relexpr) <- translateRelExpr st expr' ns
        forblock <- withScope l $ translateBlock st block
        let ivarlabel = (case symLookup ident (table st) of
                            Just (_, (VarRec _ label)) -> (show label) -- stupid!
                            Nothing -> error "Translator.hs:translateStm Invalid SymbolTable; could not find a valid symbol for'" ++ show ident ++ "'")
        return (instructions
            ++ [CFGLIRInst $ LIRRegAssignInst (SREG (read ivarlabel :: Int)) (LIROperExpr operand)]
            ++ [CFGLIRInst $ LIRLabelInst (loopLabel l)]
            ++ instructions2
            ++ [CFGLIRInst $ LIRIfInst relexpr (trueLabel l)]
            ++ [CFGLIRInst $ LIRJumpLabelInst (endLabel l)]
            ++ [CFGLIRInst $ LIRLabelInst (trueLabel l)]
            ++ forblock
            ++ [CFGLIRInst $ LIRJumpLabelInst (loopLabel l)]
            ++ [CFGLIRInst $ LIRLabelInst (endLabel l)])

translateStm st (DecafRetStm (Just expr) _) =
    do (instructions, LIRRegOperand reg) <- translateExpr st expr
       return (instructions
           ++ [CFGLIRInst $ LIRRegAssignInst reg (LIROperExpr $ LIRRegOperand RAX)]
           ++ [CFGLIRInst $ LIRRetInst])

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
translateBlock st (DecafBlock _ [] _) = withBlock (return [])
translateBlock st block =
    withBlock (do b <- getBlock
                  ns <- getNS
                  let st' = select b st
                  declarations <- mapM (translateVarDeclaration st') (blockVars block)
                  statements <- mapM (translateStm st') (blockStms block)
                  return (concat declarations ++ concat statements))

translateVarDeclaration :: SymbolTree -> DecafVar -> Translator [CFGInst]
translateVarDeclaration st var =
    return [CFGLIRInst $ LIRRegAssignInst (symVar var st) (LIROperExpr $ LIRIntOperand $ LIRInt 0)]

translateArrDeclaration :: SymbolTree -> DecafArr -> Translator [CFGInst]
translateArrDeclaration st (DecafArr ty ident len _) =
    case symLookup ident (table st) of
        Just (index, ArrayRec arr o) -> mapM (\i -> (arrayMemaddr arr o (LIRIntOperand (LIRInt i))) >>= \(instructions, mem) -> return $ CFGLIRInst $ LIRStoreInst mem (LIRIntOperand $ LIRInt 0)) [1..readDecafInteger len]
        _ -> error $ "Translator.hs:translateArrDeclaration Invalid SymbolTable; could not find a valid symbol for'" ++ ident ++ "'"

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

                                _ -> return ([CFGLIRInst $ LIRLabelInst $ LIRLabel $ "Translator.hs:translateRelExpr Invalid Expression tree; not of type relExpr"], LIROperRelExpr $ LIRRegOperand RAX)

translateExpr :: SymbolTree -> DecafExpr -> Translator ([CFGInst], LIROperand)
translateExpr st (DecafLocExpr loc _) =
    translateLocation st loc

translateExpr st (DecafMethodExpr mc _) =
    translateMethodCall st mc

translateExpr st (DecafLitExpr lit _) =
    do operand <- translateLiteral st lit
       return ([], operand)

translateExpr st (DecafBinExpr expr binop expr' _) =
    do (lopis, op1) <- translateExpr st expr
       (ropis, op2) <- translateExpr st expr'
       t <- incTemp
       let s' = SREG t
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

    binop' = case binop of
                 DecafBinArithOp (DecafPlusOp _) _ -> LADD
                 DecafBinArithOp (DecafMinOp _) _ -> LSUB
                 DecafBinArithOp (DecafMulOp _) _ -> LMUL
                 DecafBinArithOp (DecafDivOp _) _ -> LDIV
                 DecafBinArithOp (DecafModOp _) _ -> LMOD
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
           s = SREG t
       return (instructions ++ [CFGLIRInst $ LIRRegAssignInst s unexpr], LIRRegOperand s)

translateExpr st (DecafMinExpr expr _) =
    do t <- incTemp
       (instructions, operand) <- translateExpr st expr
       let unexpr = LIRUnExpr LNEG operand
           s = SREG t
       return (instructions ++ [CFGLIRInst $ LIRRegAssignInst s unexpr], LIRRegOperand s)

translateExpr st (DecafParenExpr expr _) =
    do t <- incTemp
       (instructions, operand) <- translateExpr st expr
       let o = LIROperExpr operand 
           s = SREG t
       return (instructions ++ [CFGLIRInst $ LIRRegAssignInst s o], LIRRegOperand s)

translateExpr _ _ =
    return ([CFGLIRInst $ LIRLabelInst (LIRLabel $ "Translator.hs:translateExpr Invalid expression tree")], LIRIntOperand $ LIRInt 0)

translateLiteral :: SymbolTree -> DecafLiteral -> Translator LIROperand
translateLiteral _ (DecafIntLit i _) = return $ LIRIntOperand $ LIRInt (readDecafInteger i)
translateLiteral _ (DecafBoolLit b _) = return $ LIRIntOperand $ LIRInt (if b then 1 else 0)
translateLiteral _ (DecafCharLit c _) = return $ LIRIntOperand $ LIRInt (ord c)

translateMethodPostcall :: SymbolTree -> DecafMethod -> Translator [CFGInst]
translateMethodPostcall st (DecafMethod ty ident _ _ _) =
    if mustRet
      then return $ throwException missingRet
      else return [CFGLIRInst $ LIRRetInst]
  where
    mustRet = case ty of
                  DecafVoid -> False
                  _ -> True

translateMethodPrecall :: SymbolTree -> DecafMethodCall -> Translator [CFGInst]
translateMethodPrecall st (DecafPureMethodCall ident exprs _) =
    do let numRegArgs = min 6 (length exprs)
       regargs <- mapM handleRegArg (zip [RDI, RSI, RDX, RCX, R8, R9] exprs)
       stackargs <- mapM handleStackArg (reverse $ drop numRegArgs exprs)
       return (concat regargs ++ concat stackargs)
  where
    handleRegArg (reg, expr) = do (instructions, operand) <- translateExpr st expr
                                  return $ instructions
                                        ++ [CFGLIRInst $ LIRRegAssignInst reg (LIROperExpr operand)]

    handleStackArg expr = do (instructions, operand) <- translateExpr st expr
                             return $ instructions
                                   ++ [CFGLIRInst $ LIRRegAssignInst RSP $ LIRBinExpr (LIRRegOperand RSP) LSUB (LIRIntOperand qword)] -- ^ the next two instructions form a push
                                   ++ [CFGLIRInst $ LIRRegAssignInst RSP (LIROperExpr operand)]

translateMethodPrecall st (DecafMethodCallout ident exprs _) =
    do let numRegArgs = min 6 (length exprs)
       regargs <- mapM handleRegArg (zip [RDI, RSI, RDX, RCX, R8, R9] exprs)
       stackargs <- mapM handleStackArg (reverse $ drop numRegArgs exprs)
       return (concat regargs ++ concat stackargs)
  where
    translateCalloutArg (DecafCalloutArgExpr expr _) = translateExpr st expr
    translateCalloutArg (DecafCalloutArgStr str _) = translateString st str

    handleRegArg (reg, expr) = do (instructions, operand) <- translateCalloutArg expr
                                  return $ instructions
                                        ++ [CFGLIRInst $ LIRRegAssignInst reg (LIROperExpr operand)]

    handleStackArg expr = do (instructions, operand) <- translateCalloutArg expr
                             return $ instructions
                                   ++ [CFGLIRInst $ LIRRegAssignInst RSP (LIRBinExpr (LIRRegOperand RSP) LSUB (LIRIntOperand qword))] -- ^ the next two instructions form a push
                                   ++ [CFGLIRInst $ LIRRegAssignInst RSP (LIROperExpr operand)]


translateMethodCall :: SymbolTree -> DecafMethodCall -> Translator ([CFGInst], LIROperand)
translateMethodCall st mc =
    do precall <- translateMethodPrecall st mc
       t <- incTemp
       return (precall
            ++ [CFGLIRInst $ LIRCallInst func]
            ++ [CFGLIRInst $ LIRRegAssignInst (SREG t) (LIROperExpr $ LIRRegOperand $ RAX)]
            , LIRRegOperand $ SREG $ t)
  where
    func = case mc of
               (DecafPureMethodCall {}) -> LIRProcLabel (case globalSymLookup (methodCallID mc) st of
                                                       Just (MethodRec _ (label, count)) -> methodLabel (methodCallID mc) count
                                                       _ -> "Translator.hs:translateMethodCall Invalid SymbolTable; could not find a valid symbol for'" ++ (methodCallID mc) ++ "'")
               (DecafMethodCallout {})-> LIRProcLabel (methodCalloutID mc)

translateString :: SymbolTree -> DecafString -> Translator ([CFGInst], LIROperand)
translateString st string =
    (case symLookup ('.':string) (table st) of
         Just (_, StringRec _ label) ->
             do t <- incTemp
                return ([CFGLIRInst $ LIRRegAssignInst (SREG t) (LIROperExpr $ LIRStringOperand $ stringLabel label)], LIRRegOperand $ SREG t)
         _ -> return ([CFGLIRInst $ LIRLabelInst $ LIRLabel $ "Translator.hs:translateString Invalid SymbolTable; could not find '" ++ '.':string ++ "' symbol"], LIRRegOperand RBP))

translateLocation :: SymbolTree -> DecafLoc -> Translator ([CFGInst], LIROperand)
translateLocation st loc =
    (case globalSymLookup (ident loc) st of
        Just (VarRec _ sr) -> return ([], LIRRegOperand $ SREG sr)
        Just (ArrayRec arr o) -> incTemp >>= \t -> (do (prep, index) <- translateExpr st (arrLocExpr loc)
                                                       checkCode <- arrayBoundsCheck st arr index
                                                       (instructions, mem) <- arrayMemaddr arr o index
                                                       return (prep
                                                            ++ checkCode
                                                            ++ (map CFGLIRInst instructions)
                                                            ++ [CFGLIRInst $ LIRLoadInst (SREG t) mem], LIRRegOperand (SREG t)))
        _ -> return ([CFGLIRInst $ LIRLabelInst (LIRLabel $ "Translator.hs:translateLocation Invalid SymbolTable; could not find '" ++ ident loc ++ "' symbol in\n" ++ show st)], LIRRegOperand $ SREG (-1)))

arrayBoundsCheck :: SymbolTree -> DecafArr -> LIROperand -> Translator [CFGInst]
arrayBoundsCheck st (DecafArr _ _ len _) indexOperand =
    do l <- incLabel
       return ([CFGLIRInst $ LIRIfInst (LIRBinRelExpr indexOperand LLT (LIRIntOperand $ LIRInt $ readDecafInteger len)) (boundsLabel l)]
           ++ throwException outOfBounds
           ++ [CFGLIRInst $ LIRIfInst (LIRBinRelExpr indexOperand LGT (LIRIntOperand $ LIRInt $ 0)) (boundsLabel l)]
           ++ throwException outOfBounds
           ++ [CFGLIRInst $ LIRLabelInst $ boundsLabel l])

symVar :: DecafVar -> SymbolTree -> LIRReg
symVar var st =
    SREG (case symLookup (varID var) (table st) of
                    Just (_, VarRec _ label) -> label
                    _ -> error $ "Translator.hs:translateBlock.symVar Invalid SymbolTable; could not find a valid symbol for'" ++ (show $ varID var) ++ "'")

arrayMemaddr :: DecafArr -> Int -> (LIROperand) -> Translator ([LIRInst], LIRMemAddr)
arrayMemaddr (DecafArr ty _ len _) offset operand =
    case operand of
        (LIRIntOperand (LIRInt index))->
            return ([], LIRRegOffMemAddr GP (LIRInt (offset + size*index)) (LIRInt size)) -- ^ TODO offset must include previous arrays length * size
        _ -> do t1 <- incTemp
                t2 <- incTemp
                let sr1 = SREG t1
                    sr2 = SREG t2
                return ([LIRRegAssignInst sr1 (LIRBinExpr operand LMUL (LIRIntOperand (LIRInt size)))]
                    ++ [LIRRegAssignInst sr2 (LIRBinExpr (LIRRegOperand sr1) LADD (LIRIntOperand (LIRInt offset)))]
                    , LIRRegMemAddr sr2 (LIRInt size))
  where
    arrlen = readDecafInteger len
    size = (case ty of
                DecafInteger -> 8 -- ^ size in bytes
                DecafBoolean -> 8
                _ -> error "Translate.hs:arrayMemaddr Array cannot have type void")

-- |'throwException' generates code for throwing an exception (our convention is to jump to the label __exception 
throwException :: Int -> [CFGInst]
throwException code = 
    [CFGLIRInst $ LIRJumpLabelInst $ exceptionLabel code]

exceptionHandlers = [missingRetHandler, outOfBoundsHandler]

missingRetHandler :: SymbolTree ->Translator [CFGInst]
missingRetHandler st =
    do (instructions, operand) <- translateString st missingRetMessage
       return $ [CFGLIRInst $ LIRLabelInst $ exceptionLabel 0]
           ++ instructions
           ++ [CFGLIRInst $ LIRRegAssignInst RAX (LIROperExpr operand)]
           ++ [CFGLIRInst $ LIRCallInst (LIRProcLabel "printf")]

outOfBoundsHandler :: SymbolTree -> Translator [CFGInst]
outOfBoundsHandler st =
    do (instructions, operand) <- translateString st outOfBoundsMessage
       return $ [CFGLIRInst $ LIRLabelInst $ exceptionLabel 1]
            ++ instructions
            ++ [CFGLIRInst $ LIRRegAssignInst RAX (LIROperExpr operand)]
            ++ [CFGLIRInst $ LIRCallInst (LIRProcLabel "printf")]
