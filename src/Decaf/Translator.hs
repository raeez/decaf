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
--
--        control flow graph generation
--        control flow graph ss conditional expansion
--        control flow graph noop removal

data Namespace = Namespace
    { temp :: Int
    , label :: Int
    , scope :: String
    }

newtype Translator a = Translator
    { runTranslator :: Namespace -> (a, Namespace) }

instance Monad Translator where
    return a = Translator (\s -> (a, s))
    m >>= f = Translator (\s ->
                let (a, ns) = runTranslator m s
                in runTranslator (f a) ns)

getNS :: Translator Namespace
getNS = Translator (\ns -> (ns, ns))

incTemp :: Translator a -> Translator a
incTemp m = Translator (\(Namespace t l s) ->
        runTranslator m (Namespace (t+1) l s))

getTemp :: Translator a -> Translator Int
getTemp m = Translator (\ns@(Namespace t _ _) -> (t, ns))

incLabel :: Translator a -> Translator a
incLabel m = Translator (\(Namespace t l s) ->
        runTranslator m (Namespace t (l+1) s))

getLabel :: Translator a -> Translator Int
getLabel m = Translator (\ns@(Namespace _ l _) -> (l, ns))

setScope :: String -> Translator a -> Translator a
setScope scope m = Translator (\(Namespace t l s) ->
        let (a, Namespace t' l' _) = runTranslator m (Namespace t l scope)
        in (a, (Namespace t' l' s)))

getScope :: Translator a -> Translator String
getScope m = Translator (\ns@(Namespace _ _ s) -> (s, ns))

-- | Given a SymbolTree, Translate a DecafProgram into an LIRProgram
translateProgram ::  SymbolTree -> DecafProgram -> LIRProgram
translateProgram st program = LIRProgram programlabel units
  where
    programlabel = LIRLabel "PROG"
    units = map (translateMethod st) (methods program)

-- | Given a SymbolTree, Translate a DecafMethod into an LIRUnit
translateMethod :: SymbolTree -> DecafMethod -> LIRUnit
translateMethod st method =
    case symLookup (methodID method) (getTable st) of
        Just (index, MethodRec _ (label, _)) -> LIRUnit (LIRLabel label) (translateBody (select (rindex index) st)) -- ^ label this method and select the corresponding nested SymbolTree
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
    [LIRCallInst (LIRCall func (SREG "this"))]
  where
    func = LIRProcLabel (case globalSymLookup mid st of
                            Just (MethodRec _ (label, _)) -> label
                            _ -> "Translator.hs:91 Invalid SymbolTable; could not find a valid symbol for'" ++ mid ++ "'")

translateStm st (DecafMethodStm (DecafMethodCallout mid args _) _) =
    [LIRCallInst (LIRCall func (SREG "ip"))]
  where
    func = LIRProcLabel  mid
    
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
    -- TODO dive into scope
    concatMap (translateStm st) (blockStms block)

translateRelExpr :: SymbolTree -> DecafExpr -> LIRRelExpr
translateRelExpr st expr = LIROperRelExpr $ LIRRegOperand $ RAX

translateExpr :: SymbolTree -> DecafExpr -> LIROperand
translateExpr st expr = LIRRegOperand $ RAX

getTable :: SymbolTree -> SymbolTable
getTable = (content . tree)
