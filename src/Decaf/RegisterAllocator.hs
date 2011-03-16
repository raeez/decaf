{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}


module Decaf.RegisterAllocator where
import Decaf.IR.SymbolTable
import Decaf.IR.Class
import Decaf.IR.AST
import Decaf.IR.LIR
import Decaf.Translator
import Decaf.Data.Zipper

import qualified Data.Map as Map
import Data.Typeable
import Data.Data

{- horrible Data instance declarations -}
instance Data LIRProgram where 
    gmapT f (LIRProgram lab units) = LIRProgram (f lab) (f units)
    gmapM f (LIRProgram lab units) = do lab' <- f lab
                                        units' <- f units
                                        return $ LIRProgram lab' units'

instance Data LIRLabel where
    gmapM f l = return l

instance Data LIRUnit where
    gmapM f (LIRUnit l uns) = do l' <- f l
                                 uns' <- f uns
                                 return $LIRUnit l' uns'

instance Data LIRExpr where
 gmapM f (LIRBinExpr a1 a2 a3) =
  do a'1 <- f a1
     a'2 <- f a2
     a'3 <- f a3
     
     return (LIRBinExpr a'1 a'2 a'3)
 gmapM f (LIRUnExpr a1 a2) =
  do a'1 <- f a1
     a'2 <- f a2
     
     return (LIRUnExpr a'1 a'2)
 gmapM f (LIROperExpr a1) =
  do a'1 <- f a1

     return (LIROperExpr a'1)

instance Data LIRMemAddr where
 gmapM f (LIRRegMemAddr a1 a2) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     return (LIRRegMemAddr a'1 a'2)
 gmapM f (LIRRegPlusMemAddr a1 a2 a3) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     a'3 <- f a3
     return (LIRRegPlusMemAddr a'1 a'2 a'3)
 gmapM f (LIRRegOffMemAddr a1 a2 a3) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     a'3 <- f a3
     return (LIRRegOffMemAddr a'1 a'2 a'3)

instance Data LIRInt where
 gmapM f (LIRInt a1) =
  do 
     a'1 <- f a1
     return (LIRInt a'1)

instance Data LIRRelExpr where
 gmapM f (LIRBinRelExpr a1 a2 a3) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     a'3 <- f a3
     return (LIRBinRelExpr a'1 a'2 a'3)
 gmapM f (LIRNotRelExpr a1) =
  do 
     a'1 <- f a1
     return (LIRNotRelExpr a'1)
 gmapM f (LIROperRelExpr a1) =
  do 
     a'1 <- f a1
     return (LIROperRelExpr a'1)


instance Data LIRInst where
 gmapT f (LIRRegAssignInst a1 a2) = LIRRegAssignInst (f a1) (f a2)
 gmapT f (LIRRegOffAssignInst a1 a2 a3 a4) = LIRRegOffAssignInst (f a1) (f a2) (f a3) (f a4)
 gmapT f (LIRCondAssignInst a1 a2 a3) = LIRCondAssignInst (f a1) (f a2) (f a3)
 gmapT f (LIRStoreInst a1 a2) = LIRStoreInst (f a1) (f a2)
 gmapT f (LIRLoadInst a1 a2) = LIRLoadInst (f a1) (f a2)
 gmapT f (LIRJumpRegInst a1 a2) = LIRJumpRegInst (f a1) (f a2)
 gmapT f (LIRJumpLabelInst a1) = LIRJumpLabelInst (f a1)
 gmapT f (LIRIfInst a1 a2) = LIRIfInst (f a1) (f a2)
 gmapT f (LIRRetInst) = LIRRetInst
 gmapT f (LIRLabelInst a1) = LIRLabelInst (f a1)

 gmapM f (LIRRegAssignInst a1 a2) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     return (LIRRegAssignInst a'1 a'2)
 gmapM f (LIRRegCmpAssignInst a1 a2 a3) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     a'3 <- f a3
     return (LIRRegCmpAssignInst a'1 a'2 a'3)
 gmapM f (LIRRegOffAssignInst a1 a2 a3 a4) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     a'3 <- f a3
     a'4 <- f a4
     return (LIRRegOffAssignInst a'1 a'2 a'3 a'4)
 gmapM f (LIRCondAssignInst a1 a2 a3) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     a'3 <- f a3
     return (LIRCondAssignInst a'1 a'2 a'3)
 gmapM f (LIRStoreInst a1 a2) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     return (LIRStoreInst a'1 a'2)
 gmapM f (LIRLoadInst a1 a2) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     return (LIRLoadInst a'1 a'2)
 gmapM f (LIRTempEnterInst a1) = 
  do 
     a'1 <- f a1
     return (LIRTempEnterInst a'1)
 gmapM f (LIRJumpRegInst a1 a2) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     return (LIRJumpRegInst a'1 a'2)
 gmapM f (LIRJumpLabelInst a1) =
  do 
     a'1 <- f a1
     return (LIRJumpLabelInst a'1)
 gmapM f (LIRIfInst a1 a2) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     return (LIRIfInst a'1 a'2)
 gmapM f (LIRCallInst a1) =
  do 
     a'1 <- f a1
     return (LIRCallInst a'1)
 gmapM f (LIRRetInst) =
  do 
     return (LIRRetInst)
 gmapM f (LIRLabelInst a1) =
  do 
     a'1 <- f a1
     return (LIRLabelInst a'1)

instance Data LIRBinOp where
 gmapM f (LADD) =
  do 
     return (LADD)
 gmapM f (LSUB) =
  do 
     return (LSUB)
 gmapM f (LMUL) =
  do 
     return (LMUL)
 gmapM f (LDIV) =
  do 
     return (LDIV)
 gmapM f (LMOD) =
  do 
     return (LMOD)
 gmapM f (LAND) =
  do 
     return (LAND)
 gmapM f (LOR) =
  do 
     return (LOR)
 gmapM f (LXOR) =
  do 
     return (LXOR)
 gmapM f (LSHL) =
  do 
     return (LSHL)
 gmapM f (LSHR) =
  do 
     return (LSHR)
 gmapM f (LSHRA) =
  do 
     return (LSHRA)
 gmapM f (LIRBinRelOp a1 a2) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     return (LIRBinRelOp a'1 a'2)

instance Data LIRUnOp where
 gmapM f (LNEG) =
  do 
     return (LNEG)
 gmapM f (LNOT) =
  do 
     return (LNOT)

instance Data LIRRelOp where
 gmapM f (LEQ) =
  do 
     return (LEQ)
 gmapM f (LNEQ) =
  do 
     return (LNEQ)
 gmapM f (LGT) =
  do 
     return (LGT)
 gmapM f (LGTE) =
  do 
     return (LGTE)
 gmapM f (LLT) =
  do 
     return (LLT)
 gmapM f (LLTE) =
  do 
     return (LLTE)

instance Data LIRProc where
 gmapM f (LIRProcLabel a1) =
  do 
     a'1 <- f a1
     return (LIRProcLabel a'1)
 gmapM f (LIRProcReg a1) =
  do 
     a'1 <- f a1
     return (LIRProcReg a'1)
instance Data LIRReg where
 gmapM f (RAX) =
  do 
     return (RAX)
 gmapM f (RBX) =
  do 
     return (RBX)
 gmapM f (RCX) =
  do 
     return (RCX)
 gmapM f (RDX) =
  do 
     return (RDX)
 gmapM f (RBP) =
  do 
     return (RBP)
 gmapM f (RSP) =
  do 
     return (RSP)
 gmapM f (RSI) =
  do 
     return (RSI)
 gmapM f (RDI) =
  do 
     return (RDI)
 gmapM f (R8) =
  do 
     return (R8)
 gmapM f (R9) =
  do 
     return (R9)
 gmapM f (R10) =
  do 
     return (R10)
 gmapM f (R11) =
  do 
     return (R11)
 gmapM f (R12) =
  do 
     return (R12)
 gmapM f (R13) =
  do 
     return (R13)
 gmapM f (R14) =
  do 
     return (R14)
 gmapM f (R15) =
  do 
     return (R15)
 gmapM f (GI a1) =
  do 
     a'1 <- f a1
     return (GI a'1)
 gmapM f (SREG a1) =
  do 
     a'1 <- f a1
     return (SREG a'1)
 gmapM f (MEM a1) =
  do 
     a'1 <- f a1
     return (MEM a'1)

instance Data LIROperand where
 gmapM f (LIRRegOperand a1) =
  do 
     a'1 <- f a1
     return (LIRRegOperand a'1)
 gmapM f (LIRIntOperand a1) =
  do 
     a'1 <- f a1
     return (LIRIntOperand a'1)
 gmapM f (LIRStringOperand a1) =
  do 
     a'1 <- f a1
     return (LIRStringOperand a'1)


{- END DECLARATIONS -}


mkT :: (Typeable a, Typeable b) => 
       (b->b) -> a -> a
mkT f = case cast f of
          Just g -> g
          Nothing -> id

mkM :: (Typeable a, Typeable b, Typeable (m a), Typeable (m b), Monad m) =>
       (b -> m b) -> a -> m a
mkM f = case cast f of 
          Just g -> g
          Nothing -> return

extM :: (Typeable a, Typeable b, Typeable (m a), Typeable (m b), Monad m) =>
        (a->m a) -> (b->m a) -> (a->m a)
extM f g a = case cast g of
                 Just g -> g a
                 Nothing -> f a
               

everywhere :: Data a => (forall b. Data b => b -> b) -> a -> a
everywhere f x = f (gmapT (everywhere f) x)

everywhereM :: (Monad m, Data a) => (forall b. Data b => b -> m b) -> a -> m a
everywhereM f x = do x' <- gmapM (everywhereM f) x
                     f x'

data RealRegister = Register LIRReg
                  | StackOffset Int


data RegCounterState
    = RCState 
      { rcDict :: Map.Map Int Int
      , rcCount :: Int
      , glDict :: Map.Map Int Int
      , glCount :: Int
--      , rcTable :: SymbolTree
      }
    deriving (Show, Eq)

mkRCState t = RCState (Map.empty) 0 (Map.empty) 0 -- t

data RegAllocator a = RegAllocator { runAllocator :: RegCounterState -> (a, RegCounterState) }
                    deriving Typeable

instance Monad RegAllocator where
    return a = RegAllocator (\s -> (a,s))
    m >>= f  = RegAllocator (\s -> let (a,s') = runAllocator m s
                                   in runAllocator (f a) s')

getST :: RegAllocator RegCounterState
getST = RegAllocator (\s -> (s,s))

setST :: RegCounterState -> RegAllocator ()
setST st = RegAllocator (\s -> ((), st))

{-isGlobal :: Int -> RegAllocator Bool
isGlobal num = 
    do st <- getST
       let srs = map getSR $symbolRecords.rcTable $ st

       if num `elem` srs
         then return True
         else return False
  where getSR (VarRec _ sr) = sr
        getSR _ = -1
-}
updateCounter :: LIRReg -> RegAllocator LIRReg
updateCounter reg@(SREG num)
    = do st <- getST
         let c = rcCount st
             dict = rcDict st

         if num < 0
           then do setST st{glCount = (glCount st)+1, glDict = (Map.insert num 0 (glDict st))}
                   return $ GI (-num-1)
           else
             case Map.lookup num dict of
               Just lab -> return $ SREG lab
               Nothing  -> do setST st{rcCount = c+1, rcDict = (Map.insert num c dict)} 
                              return $ SREG c
                          
updateCounter s = return s

fixStackOffset :: Int -> LIRReg -> RegAllocator LIRReg
fixStackOffset i reg@(SREG num) = 
    return $ SREG (num+i)
fixStackOffset i x = return x           
{-updateCounter reg@(GI num)
    = do st <- getST
         let c = glCount st
             dict = glDict st
         case Map.lookup num dict of
              Just lab -> return $ GI lab
              Nothing -> do setST st{glCount = c+1, glDict = (Map.insert num c dict)} 
                            return $ GI c
-}



testCounter :: String -> RegAllocator String
testCounter s = 
    do st <- getST
       let c = rcCount st
       setST st{rcCount = c+1}
       return s

--fun1 = ((mkM testCounter) `extM` (mkM updateCounter))
--fun2 = ((mkM updateCounter) `extM` (mkM testCounter))

getMethods :: SymbolTree -> [DecafMethod]
getMethods st = 
    let recs = symbolRecords.getContent $ st
        pullMethod (MethodRec m _) = [m]
        pullMethod _ = []
    in 
      concatMap pullMethod recs


allocateRegisters :: SymbolTree -> LIRProgram -> (LIRProgram, Int, [RegCounterState])
allocateRegisters st prog = 
    let units = lirProgUnits prog 
        allocUnit unit = runAllocator (everywhereM (mkM updateCounter) unit) (mkRCState st)
        unitcounts' :: [(LIRUnit, RegCounterState)]
        unitcounts' = map allocUnit units
        unitcounts = map appendEnter  $ zip (map (rcCount.snd) unitcounts') (map fst unitcounts')
        methods = getMethods st
        units' = (map fixOffset $ zip methods (init unitcounts)) ++ [last $ unitcounts]

        


        appendEnter :: (Int, LIRUnit) -> LIRUnit
        appendEnter (i, u) = 
            let insts = lirUnitInstructions u
            in
              u{lirUnitInstructions = (LIRTempEnterInst i):insts}


        fixOffset :: (DecafMethod, LIRUnit) -> LIRUnit
        fixOffset (m,u) = (fst $ runAllocator (everywhereM (mkM (fixStackOffset nargs')) u) (mkRCState st)) 
                 where nargs' = max 0 (length (methodArg m)-6)

        countGlobals st = 
            help 0 $ symbolRecords $ getContent st
          where help c [] = c
                help c (rec:rs) = 
                    case rec of 
                      VarRec {} -> help (c+1) rs
                      ArrayRec r@(DecafArr{}) _ -> help (c+(readDecafInteger $ arrayLength r)) rs
                      otherwise -> help c rs
                    

    in
      if (length methods) /= (length unitcounts - 1)

        then error ("number of methods does not equal number of units" ++ show (length methods) ++ show (length unitcounts))
        else
          ((LIRProgram (lirProgLabel prog) units'), countGlobals st, (map snd unitcounts'))
        
