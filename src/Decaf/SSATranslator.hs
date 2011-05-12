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
    { workList   :: [Label]
    , hasAlready :: LabelMap Int
    , work       :: LabelMap Int
    , varMap     :: M.Map LIRReg Int
    }

--mkTranslateState :: LIRGraph C C -> SSATranslateState
--mkTranslateState g = SSATranslateState [] mapEmpty mapEmpty (initVarsG g)

newtype SSATranslator a = SSATranslator
    { runSSA :: SSATranslateState -> (a, SSATranslateState) }

instance Monad SSATranslator where
    return a = SSATranslator (\s -> (a, s))
    m >>= f = SSATranslator (\s ->
        let (a, s') = runSSA m s
        in runSSA (f a) s')

{- addSuccessor :: String -> LIRLabel -> SSATranslator ()
addSuccessor meth label =
    SSATranslator (\ns@(Namespace{ successorMap = sm }) ->
        let updatedSuccessors = case Map.lookup meth sm of
                                  Just a  -> (label:a)
                                  Nothing -> [label] -- create the key/val
        in ((), ns{ successorMap = Map.insert meth updatedSuccessors sm }))

translate :: SSATranslator a -> Namespace -> a
translate comp ns = fst $ runLIR comp ns

incTemp :: SSATranslator Int
--incTemp = SSATranslator (\(Namespace t l s m b) -> (t, Namespace (t+1) l s m b))
incTemp = incLabel -- changed so that every label has a unique numeric id 

incLabel :: SSATranslator Int
incLabel = SSATranslator (\ns@(Namespace{ labels = l }) -> (l, ns{ labels = (l+1) }))

getScope :: SSATranslator (LIRLabel, LIRLabel)
getScope = SSATranslator (\ns@(Namespace{ scope = s }) -> (last s, ns))

getBlock :: SSATranslator Int
getBlock = SSATranslator (\ns@(Namespace{ blockindex = b }) -> ((last . init) b, ns))

getMethod :: SSATranslator String
getMethod = SSATranslator (\ns -> (encMethod ns, ns))

setMethod :: String -> SSATranslator ()
setMethod newMethod = SSATranslator (\ns -> ((), ns{ encMethod = newMethod }))

getNesting :: SSATranslator [Int]
getNesting = SSATranslator (\ns@(Namespace{ blockindex = b }) -> (b, ns))

withScope :: LIRLabel -> LIRLabel -> SSATranslator a -> SSATranslator a
withScope looplabel endlabel m 
  = SSATranslator 
    (\(Namespace t l s me b sm) ->
       let (a, Namespace t' l' _ me' b' sm') = runLIR m (Namespace t l [(looplabel, endlabel)] me b sm)
       in (a, Namespace t' l' s me' b' sm'))

withBlock :: SSATranslator a -> SSATranslator a
withBlock m = SSATranslator (\(Namespace t l s me b sm) ->
    let b' = init b ++ [last b + 1]
        (a, Namespace t' l' s' me' _ sm') = runLIR m (Namespace t l s me (b ++ [0]) sm)

-}
-- translateGraph :: LIRGraph C C -> SSAGraph C C
--

