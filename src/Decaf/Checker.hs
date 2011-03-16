module Decaf.Checker where
import Decaf.Data.Zipper
import Decaf.Util.Report
import Decaf.IR.Class
import Decaf.IR.AST
import Decaf.IR.SymbolTable
import Decaf.IR.LIR -- just for exception messages
import Decaf.Parser

-- | A single instance of a semantic error
data SemanticError = SemanticError
    { message :: String
    , position :: DecafPosition
    } deriving (Eq)

data CheckerState = CST
    { cstErrors :: [SemanticError]
    , cstTable :: SymbolTree
    }

mkCheckerState = CST [] mkSymbolTree

-- | Semantic checking monad
newtype Checker a = Checker
    { runChecker :: CheckerState -> (a, CheckerState) }

instance Monad Checker where
    return a = Checker (\s -> (a, s))
    m >>= f = Checker (\s ->
                let (a, s') = runChecker m s
                in runChecker (f a) s')

instance Show SemanticError where
    show (SemanticError message (l, c)) = show l ++ ":" ++ show c ++ ": " ++ message

-- Monad manipulation functions
pushError :: DecafPosition -> String -> Checker Bool
pushError (l,c) str = Checker (\s@(CST{cstErrors = e}) -> (False, s{cstErrors = e++[err]}))
  where
    err = SemanticError str (l, c)

addSymbol :: SymbolRecord -> Checker Bool
addSymbol sr = Checker (\s@(CST{cstTable = t}) -> (True, s{cstTable = modifyContent g t}))
  where
    g (SymbolTable rs bt) = SymbolTable (rs ++ [sr]) bt -- this order of adding symbols is important!

local :: BlockType -> Checker a -> Checker a
local tp m = Checker (\s@(CST{cstTable = t}) ->
                          let (a, s') = runChecker m s{cstTable = addChild (SymbolTable [] tp) t}
                          in (a, s'{cstTable = setContext (context t) (cstTable s')}))
getST :: Checker SymbolTree
getST  = Checker (\s@(CST{cstTable = t}) -> (t, s))

get :: Checker CheckerState
get = Checker (\s -> (s, s))

setCheckerContext :: SymbolTreeContext -> Checker()
setCheckerContext c = Checker(\s@(CST{cstTable = t}) -> ((), s{cstTable = setContext c t}))

-- | Search the current nesting level for the symbol 'id'
lookNear :: DecafIdentifier -> Checker (Maybe SymbolRecord)
lookNear id = do
                st <- getST
                let table = getContent $ st
                return $ (case symLookup id table of
                              Just (_, a) -> Just a -- ^ we don't care about the index
                              Nothing -> Nothing)

lookFar :: DecafIdentifier -> Checker (Maybe SymbolRecord)
lookFar id = do
               st <- getST
               return $ exists st
  where exists st = let table = getContent $ st
                    in case symLookup id table of
                        Nothing -> if isRoot st
                                     then Nothing
                                     else exists $ parent st
                        Just (_, a) -> Just a

inFor :: Checker Bool
inFor = do st <- getST
           return $ infor st
  where infor st
              | (blockType . getContent $ st) == ForBlock = True
              | isRoot st = False
              | otherwise = infor $ parent st

inMethod :: Checker (Maybe DecafType)
inMethod = do st <- getST
              return $ inmethod st
  where inmethod st = 
              case blockType.getContent $ st of
                MethodBlock t -> Just t
                _ -> if isRoot st
                       then Nothing
                       else inmethod $ parent st

addRet :: Checker Bool
addRet = do st <- getST
            goFun st
            setCheckerContext (context st) -- goFun modifies context
            return True
  where goFun st = 
              case blockType.getContent $ st of
                MethodBlock _ -> 
                    do setCheckerContext (context st)
                       addSymbol $ VarRec (DecafVar DecafVoid "return" (0,0)) (32)
                _ -> goFun $ parent st

-- Counter management for register/label assignment

{- Statement-like Semantic checkers, bottom-up order -}
checkStm :: DecafStm -> Checker Bool
checkStm (DecafMethodStm (DecafMethodCallout _ args _) _) = 
          foldl (>>) (return False) (map checkCalloutArg args) >> return True

checkStm (DecafMethodStm m _) =
              checkMethodArgs m >> return False

checkStm (DecafAssignStm loc op expr p) =
          do lhs <- lookFar (ident loc)
             case lhs of 
               Nothing -> pushError (pos loc) ("Undeclared variable " ++ ident loc)
               Just lhs -> 
                   do case loc of
                        arr@(DecafArrLoc {}) ->
                            checkExpr (DecafLocExpr arr p) >> return True
{-                            do indT <- checkExpr ind
                               if indT == DecafInteger
                                 then return True
                                 else pushError (pos ind) "Array index must be an integer"-}
                        _ -> return True

                      t2 <- checkExpr expr
                      let t1 = symType lhs
                      case op of
                        DecafEq {} -> if t1 == t2
                                       then return True
                                       else pushError p "Mismatched types in assignment statement"
                                            >> return False
                        _ -> if t1 == DecafInteger && t2 == DecafInteger
                               then return True
                               else pushError p "Mismatched types in arithmetic assignment statement"

checkStm (DecafIfStm expr block melse pos) =
    do
      dt <- checkExpr expr
      if dt == DecafBoolean
        then return True
        else pushError pos "If conditional must have type Boolean"

      case melse of
          Just e -> checkBlock block IfBlock >> checkBlock e IfBlock
          Nothing -> checkBlock block IfBlock

checkStm (DecafForStm id expr1 expr2 block pos') =
          do
            addSymbol $ VarRec (DecafVar DecafInteger id pos') (32)
            t1 <- checkExpr expr1
            t2 <- checkExpr expr2
            if t1 /= DecafInteger
              then pushError (pos expr1) "Start expression in for loop must have type Integer"
              else return True
            if t2 /= DecafInteger
              then pushError (pos expr2) "End expression in for loop must have type Integer"
              else return True
            checkBlock block ForBlock

checkStm (DecafRetStm rv pos) =
          do t <- inMethod -- returns method type, if there is one
             case t of
               Nothing -> pushError pos "return may only be used inside a method definition"
               Just DecafVoid -> 
                   case rv of
                     Nothing -> return True
                     _ -> pushError pos "Function declared void cannot return a value"
               Just x ->
                   case rv of
                     Nothing -> pushError pos "Function declared to return a value cannot return void"
                     Just expr -> do rt <- checkExpr expr
                                     if rt == x
                                       then return True
                                       else pushError pos ("Function declared to return " ++ show x ++ " cannot return " ++ show rt)
             case t of
               Nothing -> return False
               _ -> addRet -- hack, so that checkMethod knows whether a return statement occurred in the body; no legal var can have ID="return"

checkStm (DecafBreakStm pos) =
          guardFor pos

checkStm (DecafContStm  pos) =
          guardFor pos

checkStm (DecafBlockStm block _) =
          checkBlock block TrivialBlock

guardFor :: DecafPosition -> Checker Bool
guardFor pos = do
                b <- inFor
                if b
                  then return True
                  else pushError pos "break and continue may only be used inside for loops"


checkBlock :: DecafBlock -> BlockType -> Checker Bool
checkBlock (DecafBlock vars stmts pos) btype
    = local btype (do (foldl (>>) (return False) (map checkVarDec vars))
                      foldl (>>) (return False) (map checkStm stmts)
                      case btype of
                        MethodBlock t ->
                            do mrec <- lookNear "return"
                               case mrec of
                                 Nothing -> if t == DecafVoid
                                            then return True
                                            else pushError pos "Missing return statement" -- line number of block
                                 _ -> return True
                        _ -> return True)

checkCalloutArg :: DecafCalloutArg -> Checker Bool
checkCalloutArg carg =
    case carg of
      DecafCalloutArgExpr expr _ -> checkExpr expr >> return True
      DecafCalloutArgStr str _ -> addSymbol $ StringRec str 0

checkVarDec :: DecafVar -> Checker Bool
checkVarDec (DecafVar t id pos') = 
    do rec <- lookNear id
       case rec of 
         Nothing -> addSymbol $ VarRec (DecafVar t id pos') (32)
         Just (VarRec vrec _) -> pushError pos' ("Variable " ++ id ++ " already defined at line number "++ show (fst (pos vrec)))
         Just _ -> error "Checker.hs:200 lookNear returned declaration of incorrect type; should be VarRec" -- TODO better error message

checkFieldDec :: DecafField -> Checker Bool
checkFieldDec (DecafVarField var _) = checkVarDec var
checkFieldDec (DecafArrField arr@(DecafArr _ id len _) pos') =
    do rec <- lookNear id
       case rec of
         Nothing -> do checkExpr (DecafLitExpr (DecafIntLit len pos') pos')
                       let l = readDecafInteger len
                       if l <= 0 -- TODO needs to be fixed once I make checkLiteral
                        then pushError pos' "Arrays must have positive length"
                        else addSymbol $ ArrayRec arr (32)
         Just (ArrayRec arec _) -> pushError pos' ("Array " ++ id ++ " already defined at line number " ++ show (fst (pos arec)))
         Just _ -> error "Checker.hs:209 lookNear returned declaration of incorrect type; should be ArrayRec" -- TODO better error message

checkMethodDec :: DecafMethod -> Checker Bool
checkMethodDec meth@(DecafMethod t id args body pos') = 
    do rec <- lookNear id
       case rec of
         Nothing -> do addSymbol $ MethodRec meth ("", 32)
                       checkBlock (DecafBlock (args ++ (blockVars body)) (blockStms body) (blockPos body)) (MethodBlock t)

         Just (MethodRec meth _) -> pushError pos' ("Function " ++ id ++ " already defined at line number " ++ show(fst(pos meth)))
         Just _ -> pushError pos' ("Variable with identifier "++ id ++ " already defined")


checkProgram :: DecafProgram -> Checker Bool
checkProgram (DecafProgram fields methods) =
    do addSymbol $ StringRec missingRetMessage  0
       addSymbol $ StringRec outOfBoundsMessage 0  -- needed for translator
       foldl (>>) (return False) (map checkFieldDec fields)
       foldl (>>) (return False) (map checkMethodDec methods)
       b <- lookNear "main"
       case b of
         Just (MethodRec (DecafMethod _ _ args _ p) _) ->
            if null args
            then return True
            else pushError p "Method \"main\" must have empty parameter list"
         _ -> pushError (0, 0) "Must define method main"

-- Type checking

checkExpr :: DecafExpr -> Checker DecafType
checkExpr (DecafLocExpr (DecafVarLoc id _) pos) =
            do mrec <- lookFar id
               case mrec of
                 Nothing -> pushError pos ("Undefined variable "++id) >> return DecafVoid
                 Just rec -> 
                     case rec of
                       ArrayRec _ _ -> pushError pos "Must specify array index" >> return DecafVoid
                       _ -> return $ symType rec

checkExpr (DecafLocExpr (DecafArrLoc id ind _) pos) =
            do arrT <- lookupID id
               indT <- checkExpr ind
               if indT == DecafInteger
                then return True
                else pushError pos "Array index must be an integer"
               return arrT
  where lookupID id =
            do mrec <- lookFar id
               case mrec of
                 Nothing -> pushError pos ("Undefined variable "++id) >> return DecafVoid
                 Just (ArrayRec arr _) -> return $ arrayType arr
                 Just (VarRec var _) -> pushError pos (id++ " is not an array") >> return DecafVoid

checkExpr (Expr _ _ _) = error "AST has not had expression tree rewrite; concrete tree still present"

checkExpr (DecafMethodExpr (DecafMethodCallout _ args _) _) =
            do foldl (>>) (return False) (map checkCalloutArg args)
               return DecafInteger

checkExpr (DecafMethodExpr m _) =
            checkMethodArgs m

checkExpr (DecafLitExpr lit pos) =
            case lit of
              DecafIntLit int _ -> 
                  checkInt int pos
              DecafBoolLit _ _-> return DecafBoolean
              DecafCharLit char _ -> return DecafInteger
              _ -> do pushError pos "THIS SHOULDN'T HAPPEN" -- TODO put a sensible error message here
                      return DecafVoid
  where
      checkInt :: DecafInteger -> DecafPosition -> Checker DecafType
      checkInt int pos = 
              let val = readDecafInteger int
              in if val > 2^(31::Integer) - 1 || val < -2^(31 :: Integer)
                 then pushError pos "Integer literal too large" >> return DecafInteger -- maybe should be void
                 else return DecafInteger

checkExpr (DecafBinExpr expr1 op expr2 pos') =
            do t1 <- checkExpr expr1
               t2 <- checkExpr expr2
               case op of 
                 DecafBinArithOp {} -> 
                     do
                       if t1 /= DecafInteger
                        then pushError (pos expr1)"Argument of arithmetic operator must be integral"
                        else return True -- doesn't matter
                       if t2 /= DecafInteger
                        then pushError (pos expr2)"Argument of arithmetic operator must be integral"
                        else return True -- doesn't matter
                       return DecafInteger
                 DecafBinRelOp {} ->
                     do
                       if t1 /= DecafInteger
                        then pushError (pos expr1)"Argument of relative operator must be integral"
                        else return True
                       if t2 /= DecafInteger
                        then pushError (pos expr2)"Argument of relative operator must be integral"
                        else return True
                       return DecafBoolean
                 DecafBinEqOp {} -> 
                       do if t1 /= t2
                           then pushError pos' "Arguments of \"==\" must be of the same type"
                           else return True
                          if t1 == DecafVoid || t2 == DecafVoid
                            then pushError pos' "Arguments of \"==\" must be integral or boolean"
                            else return True
                          return DecafBoolean
                 DecafBinCondOp {} ->
                     do
                       if t1 /= DecafBoolean
                        then pushError (pos expr1)"Argument of logical operator must be boolean"
                        else return True
                       if t2 /= DecafBoolean
                        then pushError (pos expr2)"Argument of logical operator must be boolean"
                        else return True
                       return DecafBoolean

checkExpr (DecafNotExpr expr pos) =
            do t <- checkExpr expr
               if t /= DecafBoolean
                then pushError pos "Argument of logical not must be boolean"
                else return True
               return DecafBoolean

checkExpr (DecafMinExpr expr pos) =
            do t <- checkExpr expr
               if t /= DecafInteger
                then pushError pos "Can only negate an integral expression"
                else return True
               return DecafInteger

checkExpr (DecafParenExpr expr _) = checkExpr expr

checkMethodArgs :: DecafMethodCall -> Checker DecafType
checkMethodArgs (DecafPureMethodCall dID args pos) =
            do ex <- lookFar dID
               case ex of 
                 Just (MethodRec decafMethod _) ->
                   do types <- bindCat [] (map checkExpr args)
                      if all (== True) (zipWith (==) types (map varType (methodArg decafMethod)))
                         && (length types == length (methodArg decafMethod))
                        then return False
                        else pushError pos "Incorrect argument type"
                      return (methodType decafMethod)
                 Just _ -> do pushError pos (dID ++ " is not a method")
                              return DecafVoid
                 Nothing -> pushError pos ("Undeclared method: " ++ dID) >> return DecafVoid
  where
    bindCat :: [DecafType] -> [Checker DecafType] -> Checker [DecafType]
    bindCat res args =
        case args of
            [] -> return res
            _ -> do
                   t <- (head args)
                   bindCat (res++[t]) (tail args)
checkMethodArgs _ = return DecafInteger

-- | The 'checker' function returns 'True' on a semantically valid decaf program input, 'False' otherwise; utilized in testsuite
checker :: String -> Bool
checker input =
    case ps program input of
        RSuccess prog ->
            let (_, s) =  runChecker (checkProgram prog) mkCheckerState
                errors = cstErrors s
            in length errors <= 0
        RError s -> error s -- should be a valid program


-- | The 'checkFile' function returns a tuple of (String, String) representing
checkFile :: String -> String -> Report ([SemanticError], SymbolTree, String, String)
checkFile str file =
    case ps program str of
        RSuccess prog -> let (_,s) = runChecker (checkProgram prog) mkCheckerState
                             e = cstErrors s
                             t = cstTable s
                         in RSuccess (e, t, displayDebug (prog, t), displayErrors e)
        RError str -> RError str
  where
    addHeader a = file ++ ": " ++ a
    displayErrors e = unlines (map (addHeader . show) e)
    displayDebug (prog, t) = show prog ++ "\n\n" ++ show t

-- | The 'checkFile' function returns a tuple of (String, String) representing
check :: String -> String -> Report ([SemanticError], DecafProgram, SymbolTree, String, String)
check str file =
    case ps program str of
        RSuccess prog -> let (_,s) = runChecker (checkProgram prog) mkCheckerState
                             e = cstErrors s
                             t = cstTable s
                         in RSuccess (e, prog, t, displayDebug (prog, t), displayErrors e)
        RError str -> RError str
  where
    addHeader a = file ++ ": " ++ a
    displayErrors e = unlines (map (addHeader . show) e)
    displayDebug (prog, t) = show prog ++ "\n\n" ++ show t
