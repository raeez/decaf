module Decaf.Translator where
import Data.Char
import Data.Int
import Data.List
import Decaf.IR.Class
import Decaf.IR.AST
import Decaf.IR.LIR
import Decaf.IR.SymbolTable
--import Decaf.IR.ControlFlowGraph
import Decaf.Data.Tree
import Decaf.Data.Zipper

data Namespace = Namespace
    { temp       :: Int
    , labels     :: Int
    , scope      :: [(LIRLabel, LIRLabel)]
    , encMethod  :: String
    , blockindex :: [Int]
    }

mkNamespace :: Int -> Namespace
mkNamespace lastTemp= Namespace lastTemp 0 [] "null" [0]

newtype Translator a = Translator
    { runTranslator :: Namespace -> (a, Namespace) }

instance Monad Translator where
    return a = Translator (\s -> (a, s))
    m >>= f = Translator (\s ->
        let (a, s') = runTranslator m s
        in runTranslator (f a) s')

translate :: Translator a -> Namespace -> a
translate comp ns = fst $ runTranslator comp ns

incTemp :: Translator Int
--incTemp = Translator (\(Namespace t l s m b) -> (t, Namespace (t+1) l s m b))
incTemp = incLabel -- changed so that every label has a unique numeric id 

incLabel :: Translator Int
incLabel = Translator (\(Namespace t l s m b) -> (l, Namespace t (l+1) s m b))

getScope :: Translator (LIRLabel, LIRLabel)
getScope = Translator (\ns@(Namespace _ _ s _ _) -> (last s, ns))

getBlock :: Translator Int
getBlock = Translator (\ns@(Namespace _ _ _ _ b) -> ((last . init) b, ns))

getMethod :: Translator String
getMethod = Translator (\ns -> (encMethod ns, ns))

setMethod :: String -> Translator ()
setMethod newMethod = Translator (\ns -> ((), ns{ encMethod = newMethod }))

getNesting :: Translator [Int]
getNesting = Translator (\ns@(Namespace _ _ _ _ b) -> (b, ns))

withScope :: LIRLabel -> LIRLabel -> Translator a -> Translator a
withScope looplabel endlabel m 
  = Translator 
    (\(Namespace t l s me b) ->
       let (a, Namespace t' l' _ me' b') = runTranslator m (Namespace t l [(looplabel, endlabel)] me b)
       in (a, Namespace t' l' s me' b'))

withBlock :: Translator a -> Translator a
withBlock m = Translator (\(Namespace t l s me b) ->
    let b' = init b ++ [last b + 1]
        (a, Namespace t' l' s' me' _) = runTranslator m (Namespace t l s me (b ++ [0]))
    in (a, Namespace t' l' s' me' b'))

-- | Given a SymbolTree, Translate a DecafProgram into an LIRProgram
translateProgram :: SymbolTree -> DecafProgram -> Translator CFGProgram
translateProgram st program =
  do units <- mapM (translateMethod st) (methods program)
     eUnits <- mapM ($st) exceptionHandlers
     last <- incLabel
     return $ CFGProgram (LIRLabel "START" 0) 
                         (units ++ [CFGUnit exceptionHeader (concat eUnits)])
                         last
                         

-- | Given a SymbolTree, Translate a DecafMethod into an LIRUnit
translateMethod :: SymbolTree -> DecafMethod -> Translator CFGUnit
translateMethod st method =
    do (case symLookup (methodID method) (table st) of
        Just (index, MethodRec _ (label, count)) ->
            do lab <- incLabel
               oldMethod <- getMethod
               setMethod (methodID method)
               body <- translateBlock (st) (methodBody method)
               prologue <- translateMethodPrologue (st' index) method
               postcall <- translateMethodPostcall st method
               setMethod oldMethod
               return $ CFGUnit (methodlabel lab) (prologue -- changed ml to take new lab instead of count
                                                  ++ body
                                                  ++ postcall)
        _ -> return $ CFGUnit (LIRLabel ("Translator.hs:translateMethod Invalid SymbolTable; could not find '" ++ methodID method ++ "' symbol") 0) [])
  where
    st' index = select (index - firstMethodIndex) st
    firstMethodIndex = countStatics $ (symbolRecords . table) st
    countStatics [] = 0
    countStatics (MethodRec _ _:xs) = 0
    countStatics (_:xs) = 1 + (countStatics xs)

    methodlabel c = if methodname == "main"
                      then LIRLabel methodname (-1) -- minus 1 so that not displayed
                      else LIRLabel ("__" ++ methodname ++ "__proc") c
--                      else LIRLabel (methodLabel methodname c) c
      where
        methodname = methodID method

translateMethodPrologue :: SymbolTree -> DecafMethod -> Translator [CFGInst]
translateMethodPrologue st (DecafMethod _ ident args _ _) =
    do let numRegVars = min (length args) 6
           regvars = map genRegVar (zip [LRDI, LRSI, LRDX, LRCX, LR8, LR9] args)
       stackvars <- mapM genStackVar (zip [1..] (drop numRegVars args))
       return (regvars ++ stackvars)
  where
    genRegVar (reg, arg) = CFGLIRInst $ LIRRegAssignInst (symVar arg st) (LIROperExpr $ LIRRegOperand reg)
    genStackVar (index, arg) = do let mem = LIRMemAddr LRBP Nothing ((index + 1) * 8) qword -- ^ [rbp] = old rbp; [rbp + 8] = ret address; [rbp + 16] = first stack param
                                  return $ CFGLIRInst $ LIRLoadInst (symVar arg st) mem

-- | Given a SymbolTree, Translate a single DecafStatement into [CFGInst]
translateStm :: SymbolTree -> DecafStm -> Translator [CFGInst]
translateStm st (DecafAssignStm loc op expr _) =
    do (instructions1, LIRRegOperand reg) <- translateLocation st loc
       (instructions2, operand) <- translateExpr st expr
       (instructions3, operand2) <- expr' reg operand
       let arrayStore = genArrayStore instructions1 loc
       return (instructions1
           ++ instructions2
           ++ instructions3
           ++ [CFGLIRInst $ LIRRegAssignInst reg (LIROperExpr operand2)]
           ++ arrayStore)
  where
    genArrayStore [] _ = []
    genArrayStore instructions (DecafArrLoc{arrLocIdent=id})
        = case last instructions of
            CFGLIRInst (LIRLoadInst s (LIRMemAddr reg (Just reg') 0 size)) -> 
                [CFGLIRInst $ LIRRegOffAssignInst reg reg' size (LIRRegOperand s)]
            CFGLIRInst (LIRLoadInst s (LIRMemAddr reg Nothing off _)) -> 
                [CFGLIRInst $ LIRRegOffAssignInst reg (MEM $ show off) 0 (LIRRegOperand s)]
            _ -> []

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

{-translateStm st (DecafIfStm expr block melse _) =
     do (instructions, LIROperRelExpr relexpr) <- translateRelExpr st expr
        l <- incTemp
        trueblock <- translateBlock st block
        elseblock <- case melse of 
                       Just b -> translateBlock st b
                       Nothing -> return []
        return (instructions
            ++ [CFGIf relexpr l -- numbering based on registers so that short-circuit evaluation doesn't clash (SCE takes new if label from expr number)
                trueblock elseblock])
-}

translateStm st (DecafIfStm expr block melse _) = 
  do (instructions, rexpr@(LIROperRelExpr relexpr)) <- translateRelExpr st expr
     trueblock <- translateBlock st block
     elseblock <- case melse of 
                    Just b -> translateBlock st b
                    Nothing -> return []
     newif <- makeIf relexpr elseblock trueblock
     return (instructions ++ newif)

translateStm st (DecafForStm ident expr expr' block _) =
    do  falselabel <- incLabel >>= return.falseLabel -- false
        truelabel <- incLabel >>= return.trueLabel -- true
        looplabel <- incLabel >>= return.loopLabel -- loop
        endlabel <- incLabel >>= return.endLabel -- end label for while AND if; should work okay
        (instructions, operand) <- translateExpr st expr
        (instructions2, (LIROperRelExpr terminateoperand)) <- translateRelExpr st expr'
        bs <- getNesting -- can't use getBlock, it assumes you're in a new block
        let st' = select (last bs) st -- needed to lookup index variable correctly
        forblock <- withScope looplabel endlabel 
                    $ translateBlock st block
        let ivarlabel = (case symLookup ident (table st') of
                            Just (_, (VarRec _ label)) -> (show label) -- stupid!
                            Nothing -> error ("Translator.hs:translateStm Invalid SymbolTable; could not find a valid symbol for'" ++ show ident ++ "'") )
            ivarlabelreg = SREG (read ivarlabel :: Int)
            lessThan = LIRBinRelExpr (LIRRegOperand ivarlabelreg) LLT terminateoperand
        return $ instructions
            ++ [CFGLIRInst $ LIRRegAssignInst ivarlabelreg (LIROperExpr operand)]
            ++ [CFGLIRInst $ LIRJumpLabelInst looplabel] -- necessary for hoopl I think
            ++ [CFGLIRInst $ LIRLabelInst looplabel]
            ++ instructions2
            ++ [CFGLIRInst $ LIRIfInst lessThan falselabel truelabel]
            ++ [CFGLIRInst $ LIRLabelInst falselabel]
            ++ [CFGLIRInst $ LIRJumpLabelInst endlabel]
            ++ [CFGLIRInst $ LIRLabelInst truelabel]
            ++ forblock
            ++ [CFGLIRInst $ LIRRegAssignInst ivarlabelreg (LIRBinExpr (LIRRegOperand ivarlabelreg) LADD (LIRIntOperand 1))]
            ++ [CFGLIRInst $ LIRJumpLabelInst looplabel]
            ++ [CFGLIRInst $ LIRLabelInst endlabel]

translateStm st (DecafRetStm (Just expr) _) =
    do (instructions, operand) <- translateExpr st expr
       (case operand of
          LIRIntOperand int -> return (instructions
                                   ++ [CFGLIRInst $ LIRRegAssignInst LRAX (LIROperExpr $ LIRIntOperand int) ]
                                   ++ [CFGLIRInst $ LIRRetInst])
          LIRRegOperand reg -> return (instructions
                                   ++ [CFGLIRInst $ LIRRegAssignInst LRAX (LIROperExpr $ LIRRegOperand reg)]
                                   ++ [CFGLIRInst $ LIRRetInst]))

translateStm st (DecafRetStm Nothing _) =
    return [CFGLIRInst $ LIRRetInst]

translateStm st (DecafBreakStm _) =
    do (_,el) <- getScope
       return [CFGLIRInst $ LIRJumpLabelInst el]

translateStm st (DecafContStm _) =
    do (ll,_) <- getScope
       return [CFGLIRInst $ LIRJumpLabelInst ll]

translateStm st (DecafBlockStm block _) =
    translateBlock st block

translateBlock :: SymbolTree -> DecafBlock -> Translator [CFGInst]
translateBlock st (DecafBlock _ [] _) = withBlock (return [])
translateBlock st block =
    withBlock (do b <- getBlock
                  let st' = select b st
                  declarations <- mapM (translateVarDeclaration st') (blockVars block)
                  statements <- mapM (translateStm st') (blockStms block)
                  return (concat declarations ++ concat statements))

translateVarDeclaration :: SymbolTree -> DecafVar -> Translator [CFGInst]
translateVarDeclaration st var =
    return [CFGLIRInst $ LIRRegAssignInst (symVar var st) (LIROperExpr $ LIRIntOperand 0)]

translateArrDeclaration :: SymbolTree -> DecafArr -> Translator [CFGInst]
translateArrDeclaration st (DecafArr ty ident len _) =
    case symLookup ident (table st) of
        Just (index, ar@(ArrayRec arr o)) ->
            mapM (\i -> (arrayMemaddr arr ar (LIRIntOperand i)) >>=
                \(instructions, mem) ->
                    return $ CFGLIRInst $ LIRStoreInst mem (LIRIntOperand 0)) [1..readDecafInteger len]
        _ -> error $ "Translator.hs:translateArrDeclaration Invalid SymbolTable; could not find a valid symbol for'" ++ ident ++ "'"

translateRelExpr :: SymbolTree -> DecafExpr -> Translator ([CFGInst], LIRRelExpr)
translateRelExpr st expr = 
    do res<- (translateExpr st expr)
       (case res of
           ([], oper@(LIRRegOperand {})) -> return ([], LIROperRelExpr oper)
           ([], oper@(LIRIntOperand {})) -> return ([], LIROperRelExpr oper)
           (instructions, oper) -> case last instructions of
                                     CFGLIRInst (LIRRegAssignInst s e@(LIRBinExpr {})) ->
                                         return (instructions, LIROperRelExpr oper)
     
                                     CFGLIRInst (LIRRegAssignInst s (LIRUnExpr LNOT operand)) ->
                                         return (instructions, LIROperRelExpr oper)
     
                                     CFGLIRInst (LIRRegAssignInst s (LIROperExpr operand)) ->
                                         return (instructions, LIROperRelExpr oper)
     
                                     CFGExprInst {} -> 
                                         return (instructions, LIROperRelExpr oper)
     
                                     _ -> return ([CFGLIRInst $ LIRLabelInst $ LIRLabel "Translator.hs:translateRelExpr Invalid Expression tree; not of type relExpr" (-1)], LIROperRelExpr $ LIRRegOperand LRAX))

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
       cmpLabel <- incTemp
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
                                      [CFGLIRInst $ LIRRegAssignInst s' (LIRBinExpr op1 (binop' (compareLabel cmpLabel)) op2)] s]

       return (binexpr, s)
  where
    mergeFlatExprs (CFGExprInst (CFGFlatExpr insts1 _)) (CFGExprInst (CFGFlatExpr insts2 _ )) s = 
        CFGExprInst (CFGFlatExpr (insts1 ++ insts2) s)
    mergeFlatExprs2 (CFGExprInst (CFGFlatExpr insts1 _)) insts2@(x:xs) s = 
        CFGExprInst (CFGFlatExpr (insts1 ++ insts2) s)

    binop' l = case binop of
                 DecafBinArithOp (DecafPlusOp _) _ -> LADD
                 DecafBinArithOp (DecafMinOp _) _ -> LSUB
                 DecafBinArithOp (DecafMulOp _) _ -> LMUL
                 DecafBinArithOp (DecafDivOp _) _ -> LDIV
                 DecafBinArithOp (DecafModOp _) _ -> LMOD
                 DecafBinRelOp (DecafLTOp _) _ -> LIRBinRelOp LLT l
                 DecafBinRelOp (DecafGTOp _) _ -> LIRBinRelOp LGT l
                 DecafBinRelOp (DecafLTEOp _) _ -> LIRBinRelOp LLTE l
                 DecafBinRelOp (DecafGTEOp _) _ -> LIRBinRelOp LGTE l
                 DecafBinEqOp (DecafEqOp _) _ -> LIRBinRelOp LEQ l
                 DecafBinEqOp (DecafNEqOp _) _ -> LIRBinRelOp LNEQ l

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
    return ([CFGLIRInst $ LIRLabelInst (LIRLabel "Translator.hs:translateExpr Invalid expression tree" (-1))], LIRIntOperand 0)

translateLiteral :: SymbolTree -> DecafLiteral -> Translator LIROperand
translateLiteral _ (DecafIntLit i _) = return $ LIRIntOperand (readDecafInteger i)
translateLiteral _ (DecafBoolLit b _) = return $ LIRIntOperand (if b then asmTrue else asmFalse)
translateLiteral _ (DecafCharLit c _) = return $ LIRIntOperand (fromIntegral $ ord c :: LIRInt)

translateMethodPostcall :: SymbolTree -> DecafMethod -> Translator [CFGInst]
translateMethodPostcall st (DecafMethod ty ident _ _ _) =
    do method <- getMethod
       return (if mustRet
                then throwException missingRet st method
                else [CFGLIRInst $ LIRRetInst])
  where
    mustRet = case ty of
                  DecafVoid -> False
                  _ -> True

translateMethodPrecall :: SymbolTree -> DecafMethodCall -> Translator ([CFGInst], [CFGInst])
translateMethodPrecall st (DecafPureMethodCall ident exprs _) =
    do let numRegArgs = min 6 (length exprs)
       regargtuples <- mapM handleRegArg (zip [LRDI, LRSI, LRDX, LRCX, LR8, LR9] exprs)
       stackargtuples <- mapM handleStackArg (reverse $ drop numRegArgs exprs)
       let (tails, heads) = unzip (regargtuples ++ stackargtuples)
       return (concat tails, concat heads)
  where
    handleRegArg (reg, expr) = do (instructions, operand) <- translateExpr st expr
                                  return $ (instructions,
                                           [CFGLIRInst $ LIRRegAssignInst reg (LIROperExpr operand)])

    handleStackArg expr = do (instructions, operand) <- translateExpr st expr
                             return $ (instructions,
                                      [CFGLIRInst $ LIRRegAssignInst LRSP $ LIRBinExpr (LIRRegOperand LRSP) LSUB (LIRIntOperand qword)] -- ^ the next two instructions form a push
                                   ++ [CFGLIRInst $ LIRRegAssignInst LRSP (LIROperExpr operand)])

translateMethodPrecall st (DecafMethodCallout ident exprs _) =
    do let numRegArgs = min 6 (length exprs)
       regargtuples <- mapM handleRegArg (zip [LRDI, LRSI, LRDX, LRCX, LR8, LR9] exprs)
       stackargtuples <- mapM handleStackArg (reverse $ drop numRegArgs exprs)
       let (tails, heads) = unzip (regargtuples ++ stackargtuples)
       return (concat tails, concat heads)
  where
    translateCalloutArg (DecafCalloutArgExpr expr _) = translateExpr st expr
    translateCalloutArg (DecafCalloutArgStr str _) = translateString st str

    handleRegArg (reg, expr) = do (instructions, operand) <- translateCalloutArg expr
                                  return $ (instructions, [CFGLIRInst $ LIRRegAssignInst reg (LIROperExpr operand)])

    handleStackArg expr = do (instructions, operand) <- translateCalloutArg expr
                             return $ (instructions,
                                      [CFGLIRInst $ LIRRegAssignInst LRSP (LIRBinExpr (LIRRegOperand LRSP) LSUB (LIRIntOperand qword))] -- ^ the next two instructions form a push
                                   ++ [CFGLIRInst $ LIRRegAssignInst LRSP (LIROperExpr operand)])


translateMethodCall :: SymbolTree -> DecafMethodCall -> Translator ([CFGInst], LIROperand)
translateMethodCall st mc =
    do (preinstructions, precall) <- translateMethodPrecall st mc
       t <- incTemp
       l <- incLabel -- for method call return
       return (preinstructions
            ++ precall
            ++ [CFGLIRInst $ LIRRegAssignInst LRAX (LIROperExpr $ LIRIntOperand 0)
               , CFGLIRInst func
--               , CFGLIRInst $ LIRCallInst func
               , CFGLIRInst $ LIRLabelInst $ LIRLabel "METHODDONE" l
               , CFGLIRInst $ LIRRegAssignInst (SREG t) (LIROperExpr $ LIRRegOperand $ LRAX)]
            , LIRRegOperand $ SREG $ t)
  where
    func = case mc of
               DecafPureMethodCall {} -> 
                 LIRCallInst $ 
                   (case globalSymLookup (methodCallID mc) st of
                      Just (MethodRec _ (label, count)) -> methodLabel (methodCallID mc) count
                      _ -> methodLabel ("Translator.hs:translateMethodCall Invalid SymbolTable; could not find a valid symbol for'" ++ (methodCallID mc) ++ "'") (-1))
               DecafMethodCallout {} ->  LIRCalloutInst (methodCalloutID mc)

translateString :: SymbolTree -> DecafString -> Translator ([CFGInst], LIROperand)
translateString st string =
    (case globalSymLookup ('.':string) st of
         Just (StringRec _ label) ->
             do t <- incTemp
                return ([CFGLIRInst $ LIRRegAssignInst (SREG t) (LIROperExpr $ LIRStrOperand $ stringLabel label)], LIRRegOperand $ SREG t)
         _ -> return ([CFGLIRInst $ LIRLabelInst $ LIRLabel ("Translator.hs:translateString Invalid SymbolTable; could not find '" ++ string ++ "' symbol") (-1)], LIRRegOperand LRBP))

translateLocation :: SymbolTree -> DecafLoc -> Translator ([CFGInst], LIROperand)
translateLocation st loc =
    (case globalSymLookup (ident loc) st of
        Just (VarRec _ sr) -> return ([], LIRRegOperand $ SREG sr)
        Just ar@(ArrayRec arr o) -> 
            do t <- incTemp
               (prep, index) <- translateExpr st (arrLocExpr loc)
               checkCode <- arrayBoundsCheck st arr index
               (instructions, mem) <- arrayMemaddr arr ar index
               return (prep
                       ++ checkCode
                       ++ (map CFGLIRInst instructions)
                       ++ [CFGLIRInst $ LIRLoadInst (SREG t) mem], LIRRegOperand (SREG t))

        _ -> return ([CFGLIRInst $ LIRLabelInst (LIRLabel ("Translator.hs:translateLocation Invalid SymbolTable; could not find '" ++ ident loc ++ "' symbol in\n" ++ show st) (-1))], LIRRegOperand $ SREG (-1)))

arrayBoundsCheck :: SymbolTree -> DecafArr -> LIROperand -> Translator [CFGInst]
arrayBoundsCheck st (DecafArr _ _ len _) indexOperand =
    do method <- getMethod
       l1 <- incTemp -- false 1
       l2 <- incTemp -- false 2
       l3 <- incTemp -- true 1
       l4 <- incTemp -- true 2
             -- no end needed
       
       return ([CFGLIRInst $ LIRIfInst (LIRBinRelExpr indexOperand LLT (LIRIntOperand $ readDecafInteger len)) (boundsLabel False l1) (boundsLabel True l3) ] -- TODO check this
           ++ [CFGLIRInst $ LIRLabelInst $ (boundsLabel False l1)]
           ++ (throwException outOfBounds st method) -- has a jump at the end
           ++ [CFGLIRInst $ LIRLabelInst $ (boundsLabel True l3)]
           ++ [CFGLIRInst $ LIRIfInst (LIRBinRelExpr indexOperand LGTE (LIRIntOperand 0)) (boundsLabel False l2) (boundsLabel True l4)] -- TODO check this
           ++ [CFGLIRInst $ LIRLabelInst $ (boundsLabel False l2)]
           ++ (throwException outOfBounds st method) -- has a jump at the end
           ++ [CFGLIRInst $ LIRLabelInst $ boundsLabel True l4])

symVar :: DecafVar -> SymbolTree -> LIRReg
symVar var st =
    SREG (case symLookup (varID var) (table st) of
                    Just (_, VarRec _ label) -> label
                    _ -> error $ "Translator.hs:symVar Invalid SymbolTable; could not find a valid symbol for'" ++ (show $ varID var) ++ "'")

arrayMemaddr :: DecafArr -> SymbolRecord -> (LIROperand) -> Translator ([LIRInst], LIRMemAddr)
arrayMemaddr (DecafArr ty _ len _) (ArrayRec _ l) operand =
    case operand of
        (LIRIntOperand index)->
            return ([], LIRMemAddr (MEM $ arrayLabel l) Nothing (size * index) size)
        _ -> do t1 <- incTemp
                t2 <- incTemp
                let sr1 = SREG t1
                    sr2 = SREG t2
                return ([LIRRegAssignInst sr1 (LIRBinExpr operand LMUL (LIRIntOperand size))]
                    , LIRMemAddr (MEM $ arrayLabel l) (Just sr1) 0 size)
  where
    arrlen = readDecafInteger len
    size = (case ty of
                DecafInteger -> 8 -- ^ size in bytes
                DecafBoolean -> 8
                _ -> error "Translate.hs:arrayMemaddr Array cannot have type void")

-- |'throwException' generates code for throwing an exception (our convention is to jump to the label __exception 
throwException :: Int -> SymbolTree -> String -> [CFGInst]
throwException code st method = 
    case globalSymLookup ('.':method) st of
            Just (StringRec _ label) ->
                [CFGLIRInst $ LIRRegAssignInst LRSI (LIROperExpr (LIRStrOperand $ stringLabel label))]
             ++ [CFGLIRInst $ LIRJumpLabelInst $ exceptionLabel st (exception code)]
            other -> error $ "Translate.hs:throwException could not find symbol for :" ++ method

exceptionHandlers = [missingRetHandler, outOfBoundsHandler]

missingRetHandler :: SymbolTree ->Translator [CFGInst]
missingRetHandler st =
       do let var = case globalSymLookup ('.':missingRetMessage) st of
                      Just (StringRec _ l) ->  l
                      _ -> error $ "Translate.hs:missingRetHandler could not find symbol for :" ++ missingRetMessage
          return $ [CFGLIRInst $ LIRLabelInst $ exceptionLabel st missingRetMessage]
                ++ [CFGLIRInst $ LIRRegAssignInst LRDI (LIROperExpr (LIRRegOperand $ MEM $ exceptionString st missingRetMessage))]
                ++ [CFGLIRInst $ LIRRegAssignInst LRAX (LIROperExpr (LIRIntOperand 0))]
                ++ [CFGLIRInst $ LIRCalloutInst "printf"]
                ++ [CFGLIRInst LIRRetInst]

outOfBoundsHandler :: SymbolTree -> Translator [CFGInst]
outOfBoundsHandler st =
       do let var = case globalSymLookup ('.':outOfBoundsMessage) st of
                      Just (StringRec _ l) -> l
                      _ -> error $ "Translate.hs:outOfBoundsHandler could not find symbol for :" ++ outOfBoundsMessage
          return $ [CFGLIRInst $ LIRLabelInst $ exceptionLabel st outOfBoundsMessage]
               ++ [CFGLIRInst $ LIRRegAssignInst LRDI (LIROperExpr (LIRRegOperand $ MEM $ exceptionString st outOfBoundsMessage))]
               ++ [CFGLIRInst $ LIRRegAssignInst LRAX (LIROperExpr (LIRIntOperand 0))]
               ++ [CFGLIRInst $ LIRCalloutInst "printf"]
               ++ [CFGLIRInst LIRRetInst]


makeIf :: LIROperand -> [CFGInst] -> [CFGInst] -> Translator [CFGInst]
makeIf expr falseblock trueblock = 
  do falselabel <- incLabel >>= (return . falseLabel)
     truelabel  <- incLabel >>= (return . trueLabel)
     endlabel   <- incLabel >>= (return . endLabel)

     return $
      [ CFGLIRInst $ LIRIfInst (LIROperRelExpr expr) falselabel truelabel
      , CFGLIRInst $ LIRLabelInst falselabel]
      ++ falseblock ++
      [ CFGLIRInst $ LIRJumpLabelInst endlabel
      , CFGLIRInst $ LIRLabelInst truelabel]
      ++ trueblock ++
      [ CFGLIRInst $ LIRLabelInst endlabel]
