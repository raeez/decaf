{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Decaf.SSATranslator
where
import Data.Maybe
import Loligoptl
import Decaf.LIRNodes
import Decaf.SSANodes
import Decaf.IR.LIR
import qualified Data.Map as M

data TranslateState = Translate
    { hasAlready   :: LabelMap Int
    , work         :: LabelMap Int
    , varMap       :: M.Map LIRReg Int
    }

mkTranslateState :: Int -> Namespace
mkTranslateState = TranslateState emptyMap emptyMap M.empty

newtype SSATranslator a = SSATranslator
    { runSSA :: TranslateState -> (a, TranslateState) }

instance Monad TranslateState where
    return a = SSATranslator (\s -> (a, s))
    m >>= f = SSATranslator (\s ->
        let (a, s') = runSSA m s
        in runSSA (f a) s')


--translateGraph :: LIRGraph C C -> SSAGraph C C

