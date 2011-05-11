{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}


module Decaf.RegisterAllocator where
import Decaf.IR.SymbolTable
import Decaf.IR.IRNode
import Decaf.IR.AST
import Decaf.IR.LIR
import Decaf.Data.Zipper
import Decaf.LIRTranslator

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
 gmapM f (LIRMemAddr a1 a2 a3 a4) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     a'3 <- f a3
     a'4 <- f a4
     return (LIRMemAddr a'1 a'2 a'3 a'4)

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
 gmapT f (LIRStoreInst a1 a2) = LIRStoreInst (f a1) (f a2)
 gmapT f (LIRLoadInst a1 a2) = LIRLoadInst (f a1) (f a2)
 gmapT f (LIRJumpLabelInst a1) = LIRJumpLabelInst (f a1)
 gmapT f (LIRIfInst a1 a2 a3) = LIRIfInst (f a1) (f a2) (f a3)
 gmapT f (LIRRetInst a1 a2) = LIRRetInst (f a1) (f a2)
 gmapT f (LIRLabelInst a1) = LIRLabelInst (f a1)

 gmapM f (LIRRegAssignInst a1 a2) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     return (LIRRegAssignInst a'1 a'2)
 gmapM f (LIRRegOffAssignInst a1 a2 a3 a4) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     a'3 <- f a3
     a'4 <- f a4
     return (LIRRegOffAssignInst a'1 a'2 a'3 a'4)
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
 gmapM f (LIREnterInst a1) = 
  do 
     a'1 <- f a1
     return (LIREnterInst a'1)
 gmapM f (LIRJumpLabelInst a1) =
  do 
     a'1 <- f a1
     return (LIRJumpLabelInst a'1)
 gmapM f (LIRIfInst a1 a2 a3) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     a'3 <- f a3
     return (LIRIfInst a'1 a'2 a'3)
 gmapM f (LIRCallInst a1 a2) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     return (LIRCallInst a'1 a'2)
 gmapM f (LIRCalloutInst a1) =
  do 
     a'1 <- f a1
     return (LIRCalloutInst a'1)
 gmapM f (LIRRetInst a1 a2) =
  do 
     a'1 <- f a1
     a'2 <- f a2
     return (LIRRetInst a'1 a'2)
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
 gmapM f (LRAX) =
  do 
     return (LRAX)
 gmapM f (LRBX) =
  do 
     return (LRBX)
 gmapM f (LRCX) =
  do 
     return (LRCX)
 gmapM f (LRDX) =
  do 
     return (LRDX)
 gmapM f (LRBP) =
  do 
     return (LRBP)
 gmapM f (LRSP) =
  do 
     return (LRSP)
 gmapM f (LRSI) =
  do 
     return (LRSI)
 gmapM f (LRDI) =
  do 
     return (LRDI)
 gmapM f (LR8) =
  do 
     return (LR8)
 gmapM f (LR9) =
  do 
     return (LR9)
 gmapM f (LR10) =
  do 
     return (LR10)
 gmapM f (LR11) =
  do 
     return (LR11)
 gmapM f (LR12) =
  do 
     return (LR12)
 gmapM f (LR13) =
  do 
     return (LR13)
 gmapM f (LR14) =
  do 
     return (LR14)
 gmapM f (LR15) =
  do 
     return (LR15)
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
 gmapM f (LIRStrOperand a1) =
  do 
     a'1 <- f a1
     return (LIRStrOperand a'1)


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
    let initUnits = lirProgUnits prog 

        allocUnit :: LIRUnit -> (LIRUnit, RegCounterState)
        allocUnit unit = runAllocator (everywhereM (mkM updateCounter) unit) (mkRCState st)
        enhUnits = map allocUnit initUnits

        units = map fst enhUnits
        counters = map snd enhUnits

        methods = getMethods st

        units' = (map appendEnter $ zip (map rcCount counters)
                                        (map fixOffset $ zip methods units))
                 ++ [last units, last . init $ units]

        appendEnter :: (Int, LIRUnit) -> LIRUnit
        appendEnter (i, u) = 
            let insts = lirUnitInstructions u
            in
              u{lirUnitInstructions = (head insts) : ((LIREnterInst lirsize):(tail insts))}
          where
            lirsize = fromIntegral i :: LIRInt

        fixOffset :: (DecafMethod, LIRUnit) -> LIRUnit
        fixOffset (m,u) = (fst $ runAllocator (everywhereM (mkM (fixStackOffset nargs')) u) (mkRCState st)) 
                 where nargs' = max 0 (length (methodArg m)-6)

        countGlobals st = 
            help 0 $ symbolRecords $ getContent st
          where help c [] = c
                help c (rec:rs) = 
                    case rec of 
                      VarRec {} -> help (c+1) rs
                      ArrayRec r@(DecafArr{}) _ -> help (c+(fromIntegral $ readDecafInteger $ arrayLength r :: Int)) rs
                      otherwise -> help c rs
                    

    in
      if (length methods) /= (length units - 2) -- 2 extra units for exceptions

        then error ("number of methods does not equal number of units" 
                    ++ show (length methods) ++ show (length units))
        else
          ((LIRProgram (lirProgLabel prog) units'), countGlobals st, counters)
        
