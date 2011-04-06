module Decaf.TransRewrite where
import Compiler.Hoopl hiding (Top)
import Data.Map as Map
import Decaf.IR.LIR
import Control.Monad


----------------------------------------------------
-- this file contains CSE transfer/rewrite functions
-- currently it is local rewrite,  
-- would need to expand to global rewrite later



-- Note: 
-- currently only support
-- x = y binop z   -->  x = w 
-- not  x = y relop z, or x = uniop y



data CSEKey = CSEKey LIROperand LIRBinOp LIROperand
type CSEData = LIRReg
type CSELattice = Map CSEKey CSEData



-- join two CSE lattices : fix me!     (right now, the joined results is always empty, CSE is completely local to basic block)
joinCSELattice :: JoinFun CSELattice
joinCSELattice _ (OldFact m) (NewFact n) = 
  (NoChange, empty);
        

  

-- define const lattice
constLattice :: DataflowLattice Lattice
constLattice = DataflowLattice
  { fact_name   = "CSElattice"
  , fact_bot    = empty        
  , fact_join   = joinCSELattice }





