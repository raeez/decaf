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

getNS :: Translator Namespace
getNS = Translator (\ns -> (ns, ns))

incTemp :: Translator ()
incTemp = Translator (\(Namespace t l s b) -> ((), Namespace (t+1) l s b))

getTemp :: Translator Int
getTemp = Translator (\ns@(Namespace t _ _ _) -> (t, ns))

incLabel :: Translator ()
incLabel = Translator (\(Namespace t l s b) -> ((), Namespace t (l+1) s b))

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
    do i <- getTemp
       incTemp
       let prog = LIRProgram (LIRLabel $ "L" ++ show i) units
       return prog
  where
    units = map (translateMethod st) (methods program)

-- | Given a SymbolTree, Translate a DecafMethod into an LIRUnit
translateMethod :: SymbolTree -> DecafMethod -> LIRUnit
translateMethod st method =
    case symLookup (methodID method) (getTable st) of
        Just (index, MethodRec _ (label, count)) ->
            LIRUnit (LIRLabel (label ++ show count)) (translateBody (select (rindex index) st)) -- ^ label this method and select the corresponding nested SymbolTree
        _ -> LIRUnit (LIRLabel ("Translator.hs:71 Invalid SymbolTable; could not find '" ++ methodID method ++ "' symbol")) [] -- ^ should not happen
  where
    translateBody st' = concatMap (translateStm st') (blockStms $ methodBody method) -- ^ join all translated statements together
    rindex index = (length . children . tree) st - index - 1

-- | Given a SymbolTree, Translate a single DecafStatement into [LIRInst]
translateStm :: SymbolTree -> DecafStm -> [LIRInst]
translateStm st (DecafAssignStm loc op expr _) =
    [lir]
  where
    lir = case globalSymLookup (ident loc) st of
              Just (VarRec _ sr) -> LIRRegAssignInst (SREG (show sr)) (LIROperExpr $ translateExpr st expr)
              Just (ArrayRec _ o) -> LIRStoreInst (LIRRegOffMemAddr (SREG "gp") (LIRInt o) (LIRInt 8)) (translateExpr st expr)
              _ -> LIRLabelInst (LIRLabel $ "Translator.hs:84 Invalid SymbolTable; could not find '" ++ ident loc ++ "' symbol")

translateStm st (DecafMethodStm (DecafPureMethodCall mid args _) _) =
    [LIRCallInst (LIRCall func (SREG "ip"))]
  where
    func = LIRProcLabel (case globalSymLookup mid st of
                            Just (MethodRec _ (label, count)) -> label ++ show count
                            _ -> "Translator.hs:91 Invalid SymbolTable; could not find a valid symbol for'" ++ mid ++ "'")

translateStm st (DecafMethodStm (DecafMethodCallout mid args _) _) =
    [LIRCallInst (LIRCall func (SREG "ip"))]
  where
    func = LIRProcLabel mid
    
translateStm st (DecafIfStm expr block (Just elseblock) _) =
    [LIRIfInst (translateRelExpr st expr) truelabel]
        ++ translateBlock st elseblock
        ++ [LIRJumpLabelInst endlabel]
        ++ [LIRLabelInst truelabel]
        ++ translateBlock st block
        ++ [LIRLabelInst endlabel]
  where
    truelabel = LIRLabel "TRUE"
    endlabel = LIRLabel "END"

translateStm st (DecafForStm ident expr expr' block _) =
    [LIRRegAssignInst (SREG "0") (LIROperExpr $ translateExpr st expr)] -- TODO pullout of st
        ++ [LIRLabelInst looplabel]
        ++ [LIRIfInst (translateRelExpr st expr') truelabel]
        ++ [LIRJumpLabelInst endlabel]
        ++ [LIRLabelInst truelabel]
        ++ translateBlock st block
        ++ [LIRJumpLabelInst looplabel]
        ++ [LIRLabelInst endlabel]
  where
    looplabel = LIRLabel "LOOP"
    truelabel = LIRLabel "TRUE"
    endlabel = LIRLabel "END"
    
translateStm st (DecafIfStm expr block Nothing _) =
    [LIRIfInst (translateRelExpr st expr) truelabel]
        ++ [LIRJumpLabelInst endlabel]
        ++ [LIRLabelInst truelabel]
        ++ translateBlock st block
        ++ [LIRLabelInst endlabel]
  where
    truelabel = LIRLabel "TRUE"
    endlabel = LIRLabel "END"

translateStm st (DecafRetStm (Just expr) _) =
    [LIRRetOperInst (translateExpr st expr)]

translateStm st (DecafRetStm Nothing _) =
    [LIRRetInst]

translateStm st (DecafBreakStm _) =
    [LIRJumpLabelInst endlabel]
  where
    endlabel = LIRLabel "END"

translateStm st (DecafContStm _) =
    [LIRJumpLabelInst startlabel]
  where
    startlabel = LIRLabel "LOOP"

translateStm st (DecafBlockStm block _) =
    translateBlock st block

translateBlock :: SymbolTree -> DecafBlock -> [LIRInst]
translateBlock st block =
    translateBody st
  where
    translateBody st' = concatMap (translateStm st') (blockStms block)
    rindex index = (length . children . tree) st - index - 1

translateRelExpr :: SymbolTree -> DecafExpr -> LIRRelExpr
translateRelExpr st expr = LIROperRelExpr $ LIRRegOperand $ RAX

translateExpr :: SymbolTree -> DecafExpr -> LIROperand
translateExpr st expr = LIRRegOperand $ RAX

getTable :: SymbolTree -> SymbolTable
getTable = (content . tree)
