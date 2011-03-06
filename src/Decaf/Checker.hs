module Decaf.Checker where
import Decaf.AST
import Decaf.Parser
import Decaf.Data.SymbolTable
import Decaf.Data.ContextTree
import Decaf.Util

data SemanticError = SemanticError {
  message :: String,
  position :: DecafPosition
  }

-- | Semantic checking monad
newtype Checker a = Checker {
  runChecker :: ([SemanticError], SymbolTree) -> (a, ([SemanticError], SymbolTree))
  }

instance Monad Checker where
  return a = Checker (\s -> (a, s))
  m >>= f = Checker (\s ->
                let (a, (e,t)) = runChecker m s
                in runChecker (f a) (e,t))

instance Show SemanticError where
  show (SemanticError message (l, c)) = show l ++ ":" ++ show c ++ ": " ++ message

-- Monad manipulation functions
pushError :: DecafPosition -> String -> Checker Bool
pushError (l,c) str = Checker (\(e,t) -> (False, (e ++ [err], t)))
    where
      err = SemanticError str (l, c)

addSymbol :: SymbolRecord -> Checker Bool
addSymbol sr = Checker (\(e,t) -> (True, (e, modifyContextTreeContent g t)))
    where g (SymbolTable rs bt) = SymbolTable (sr : rs) bt

local :: BlockType -> Checker a -> Checker a
local tp m = Checker (\(e,t)-> let (a,(e', t')) = runChecker m (e, addChild (SymbolTable [] tp) t)
                               in (a, (e', setContext (context t) t')))
getST :: Checker SymbolTree
getST  = Checker (\(e, t) -> (t,(e, t)))

get :: Checker ([SemanticError], SymbolTree)
get = Checker (\(e,t) -> ((e, t),(e, t)))

setContextTree :: SymbolTree -> Checker ()
setContextTree t = Checker(\(e, _) -> ((), (e,t )))

setCheckerContext :: Context -> Checker()
setCheckerContext c = Checker(\(e, t)-> ((), (e, setContext c t)))

-- symbol table access functions
lookNear :: DecafIdentifier -> Checker (Maybe SymbolRecord)
lookNear id = do st <- getST
                 let recs = symbolRecords.getContent $ st
                 return $ lookup id $ zip (map symID recs) recs

lookFar :: DecafIdentifier -> Checker (Maybe SymbolRecord)
lookFar id = do st <- getST
                return $ exists st
    where exists st = let recs = symbolRecords.getContent $ st in
                      case lookup id (zip (map symID recs) recs) of
                        Nothing -> if isRoot st
                                   then Nothing
                                   else exists $ parent st
                        other -> other

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
                       addSymbol $ VarRec $ DecafVar DecafVoid "return" (0,0)
                _ -> goFun $ parent st

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
                        DecafArrLoc _ ind _ ->
                            do indT <- checkExpr ind
                               if indT == DecafInteger
                                then return True
                                else pushError (pos ind) "Array index must be an integer"
                        _ -> return True

                      t2 <- checkExpr expr
                      let t1 = symType lhs
                      case op of
                        DecafEq _ -> if t1 == t2
                                   then return True
                                   else pushError p "Mismatched types in assignment statement"
                                            >> return False
                        _ -> if t1 == DecafInteger && t2 == DecafInteger
                                 then return True
                                 else pushError p "Mismatched types in arithmetic assignment statement"

checkStm (DecafIfStm expr block melse pos) = 
          do dt <- checkExpr expr
             if dt == DecafBoolean
              then return True
              else pushError pos "If conditional must have type Boolean"

             case melse of
               Just e -> checkBlock block IfBlock >> checkBlock e IfBlock
               Nothing -> checkBlock block IfBlock

checkStm (DecafForStm id expr1 expr2 block pos') = 
          do addSymbol $ VarRec $ DecafVar DecafInteger id pos'
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
      DecafCalloutArgStr _ _ -> return True

checkVarDec :: DecafVar -> Checker Bool
checkVarDec (DecafVar t id pos') = 
    do rec <- lookNear id
       case rec of 
         Nothing -> addSymbol $ VarRec $ DecafVar t id pos'
         Just (VarRec vrec) -> pushError pos' ("Variable " ++ id ++ " already defined at line number "++ show (fst (pos vrec)))
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
                        else addSymbol $ ArrayRec arr
         Just (ArrayRec arec) -> pushError pos' ("Array " ++ id ++ " already defined at line number " ++ show (fst (pos arec)))
         Just _ -> error "Checker.hs:209 lookNear returned declaration of incorrect type; should be ArrayRec" -- TODO better error message

checkMethodDec :: DecafMethod -> Checker Bool
checkMethodDec meth@(DecafMethod t id args body@(DecafBlock _ _ _) pos') = 
    do rec <- lookNear id
       case rec of
         Nothing -> do foldl (>>) (return False) (map (addSymbol.VarRec) args)
                       addSymbol $ MethodRec meth
                       checkBlock body (MethodBlock t) 


         Just (MethodRec meth) -> pushError pos' ("Function " ++ id ++ " already defined at line number " ++ show(fst(pos meth)))
         Just _ -> error "Checker.hs:221 lookNear returned declaration of incorrect type; should be MethodRec" -- TODO better error message


checkProgram :: DecafProgram -> Checker Bool
checkProgram (DecafProgram fields methods p) = 
    do foldl (>>) (return False) (map checkFieldDec fields)
       foldl (>>) (return False) (map checkMethodDec methods)
       b <- lookNear "main"
       case b of
         Just (MethodRec (DecafMethod _ _ args _ p)) ->
            if null args
            then return True
            else pushError p "Method \"main\" must have empty parameter list"
         _ -> pushError p "Must define method main"

-- Type checking

checkExpr :: DecafExpr -> Checker DecafType
checkExpr (DecafLocExpr (DecafVarLoc id _) pos) =
            do mrec <- lookFar id
               case mrec of
                 Nothing -> pushError pos ("Undefined variable "++id) >> return DecafVoid
                 Just rec -> 
                     case rec of
                       ArrayRec _ -> pushError pos "Must specify array index" >> return DecafVoid
                       _ -> return $ symType rec

checkExpr (DecafLocExpr (DecafArrLoc id ind _) pos) =
            do arrT <- lookupID id pos 
               indT <- checkExpr ind
               if indT == DecafInteger
                then return True
                else pushError pos "Array index must be an integer"
               return arrT
    where lookupID id pos =
            do mrec <- lookFar id
               case mrec of
                 Nothing -> pushError pos("Undefined variable "++id) >> return DecafVoid
                 Just rec -> return $ symType rec

checkExpr (DecafExpr _ _ _) = error "AST has not had expression tree rewrite; concrete tree still present"

checkExpr (DecafMethodExpr (DecafMethodCallout _ args _) _) =
            do foldl (>>) (return False) (map checkCalloutArg args)
               return DecafInteger

checkExpr (DecafMethodExpr m _) =
            checkMethodArgs m

checkExpr (DecafLitExpr lit _) =
            case lit of
              DecafIntLit int pos-> 
                  checkInt int pos
              DecafBoolLit _ _-> return DecafBoolean
              _ -> error "THIS SHOULDN'T HAPPEN" -- TODO put a sensible error message here
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
                 DecafBinArithOp _ _-> 
                     do
                       if t1 /= DecafInteger
                        then pushError (pos expr1)"Argument of arithmetic operator must be integral"
                        else return True -- doesn't matter
                       if t2 /= DecafInteger
                        then pushError (pos expr2)"Argument of arithmetic operator must be integral"
                        else return True -- doesn't matter
                       return DecafInteger
                 DecafBinRelOp _ _ ->
                     do
                       if t1 /= DecafInteger
                        then pushError (pos expr1)"Argument of relative operator must be integral"
                        else return True
                       if t2 /= DecafInteger
                        then pushError (pos expr2)"Argument of relative operator must be integral"
                        else return True
                       return DecafBoolean
                 DecafBinEqOp _ _-> 
                       do if t1 /= t2
                           then pushError pos' "Arguments of \"==\" must be of the same type"
                           else return True
                          return DecafBoolean
                 DecafBinCondOp _ _->
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
                 Just (MethodRec decafMethod) -> 
                   do types <- bindCat [] (map checkExpr args)
                      if (all (== True) $ zipWith (==) types (map varType (methodArg decafMethod)))
                         && (length types == length (methodArg decafMethod))
                        then return False
                        else pushError pos "Incorrect argument type"
                      return (methodType decafMethod)
                 Just _ -> error "Checker.hs:347 lookFar returned declaration of incorrect type; should be methodRec" -- TODO better error message
                 Nothing -> pushError pos ("Undeclared method: " ++ dID) >> return DecafVoid
          where
            bindCat :: [DecafType] -> [Checker DecafType] -> Checker [DecafType]
            bindCat res args =
                case args of
                  [] -> return res
                  _ -> do t <- (head args)
                          bindCat (res++[t]) (tail args)
checkMethodArgs _ = return DecafInteger

-- | The 'checker' function returns 'True' on a semantically valid decaf program input, 'False' otherwise; utilized in testsuite
checker :: String -> Bool
checker input =
    case ps program input of
      RSuccess prog ->
          let (_, (errors, _)) =  runChecker (checkProgram prog) ([], mkContextTree $ SymbolTable [] GlobalBlock)
          in length errors <= 0
      RError s -> error s -- should be a valid program

-- | The 'checkFile' function returns a tuple of (String, String) representing
checkFile :: String -> String -> (String, String)
checkFile str file =
    case ps program str of
      RSuccess prog -> 
          let (_,(e,t)) = runChecker (checkProgram prog) ([], mkContextTree $ SymbolTable [] GlobalBlock)
          in (show prog ++ "\n\n" ++ show t, unlines (map (addHeader . show) e))
      RError str -> (str,str)
      where
        safeinit [] = []
        safeinit list = init list
        addHeader a = file ++ ": " ++ a
