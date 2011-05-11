{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Decaf.SSATranslator
where
import Data.Maybe
import Loligoptl
import Loligoptl.Label
import Decaf.LIRNodes
import Decaf.SSANodes
import Decaf.IR.LIR
import qualified Data.Map as M

data SSATranslateState = SSATranslateState
    { hasAlready   :: LabelMap Int
    , work         :: LabelMap Int
    , varMap       :: M.Map LIRReg Int
    }

mkTranslateState :: SSATranslateState
mkTranslateState = SSATranslateState mapEmpty mapEmpty M.empty

newtype SSATranslator a = SSATranslator
    { runSSA :: SSATranslateState -> (a, SSATranslateState) }

instance Monad SSATranslator where
    return a = SSATranslator (\s -> (a, s))
    m >>= f = SSATranslator (\s ->
        let (a, s') = runSSA m s
        in runSSA (f a) s')

-- translateGraph :: LIRGraph C C -> SSAGraph C C
