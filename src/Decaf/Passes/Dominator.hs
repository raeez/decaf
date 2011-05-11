{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Decaf.Passes.Dominator
  ( DomFact, DPath(..), domPath, domEntry, domLattice, extendDom
  , DominatorNode(..), DominatorTree(..), dtree
  , immediateDominators
  , DominanceFrontiers, mkDF, mkIteratedDF, mkDFSet
  , domPass
  )
where
import Data.Maybe
import Data.List
import Loligoptl
import Decaf.LIRNodes
import Debug.Trace

type DomFact = WithBot DPath
-- ^ List of labels, extended with a standard bottom element

-- | The fact that goes into the entry of a dominator analysis: the first node
-- is dominated only by the entry point, which is represented by the empty list
-- of labels.
domEntry :: DomFact
domEntry = PElem (DPath [])

newtype DPath = DPath [Label]
  -- ^ represents part of the domination relation: each label
  -- in a list is dominated by all its successors.  This is a newtype only so
  -- we can give it a fancy Show instance.

instance Show DPath where
  show (DPath ls) = concat (foldr (\l path -> show l : " -> " : path) ["entry"] ls)

domPath :: DomFact -> [Label]
domPath Bot = [] -- lies: an unreachable node appears to be dominated by the entry
domPath (PElem (DPath ls)) = ls

extendDom :: Label -> DPath -> DPath
extendDom l (DPath ls) = trace ("found label " ++ show l ++ ", adding it to dpath: " ++ show ls) DPath (l:ls)

domLattice :: DataflowLattice DomFact
domLattice = addPoints "dominators" extend

extend :: JoinFun DPath
extend _ (OldFact (DPath l)) (NewFact (DPath l')) =
                                (changeIf (l `lengthDiffers` j), DPath j)
    where j = lcs l l'
          lcs :: [Label] -> [Label] -> [Label] -- longest common suffix
          lcs l l' | length l > length l' = lcs (drop (length l - length l') l) l'
                   | length l < length l' = lcs l' l
                   | otherwise = dropUnlike l l' l
          dropUnlike [] [] maybe_like = maybe_like
          dropUnlike (x:xs) (y:ys) maybe_like =
              dropUnlike xs ys (if x == y then maybe_like else xs)
          dropUnlike _ _ _ = error "this can't happen"

          lengthDiffers [] [] = False
          lengthDiffers (_:xs) (_:ys) = lengthDiffers xs ys
          lengthDiffers [] (_:_) = True
          lengthDiffers (_:_) [] = True



-- | Dominator pass
domPass :: (NonLocal n, Monad m) => FwdPass m n DomFact
domPass = FwdPass
  { fp_lattice = domLattice
  , fp_transfer = (mkFTransfer3 first (const id) distributeFact)
  , fp_rewrite = noFwdRewrite
  }
  where first n = fmap (extendDom $ entryLabel n)

----------------------------------------------------------------

data DominatorNode = Entry | Labelled Label
data DominatorTree = Dominates DominatorNode [DominatorTree]
-- ^ This data structure is a *rose tree* in which each node may have
--  arbitrarily many children.  Each node dominates all its descendants.

-- | Map from a FactBase for dominator lists into a
-- dominator tree.  
dtree :: [(Label, DomFact)] -> DominatorTree
dtree facts = Dominates Entry $ merge $ map reverse $ map mkList facts
   -- This code has been lightly tested.  The key insight is this: to
   -- find lists that all have the same head, convert from a list of
   -- lists to a finite map, in 'children'.  Then, to convert from the
   -- finite map to list of dominator trees, use the invariant that
   -- each key dominates all the lists of values.
   --
  where merge lists = mapTree $ children $ filter (not . null) lists
        children = foldl addList noFacts
        addList :: FactBase [[Label]] -> [Label] -> FactBase [[Label]]
        addList map (x:xs) = mapInsert x (xs:existing) map
            where existing = fromMaybe [] $ lookupFact x map
        addList _ [] = error "this can't happen"
        mapTree :: FactBase [[Label]] -> [DominatorTree]
        mapTree map = [Dominates (Labelled x) (merge lists) |
                                                    (x, lists) <- mapToList map]
        mkList (l, doms) = l : domPath doms


-- |'dominatorWalk' performs a post-order walk of the dominator tree
dominatorWalk :: DominatorTree -> [Label]
dominatorWalk (Dominates Entry [])            = []
dominatorWalk (Dominates Entry children)      = concatMap dominatorWalk children
dominatorWalk (Dominates (Labelled l) [])     = [l]
dominatorWalk (Dominates (Labelled l) children) = concatMap dominatorWalk children ++ [l]

mkImDom :: FactBase DomFact -> LabelMap [Label]
mkImDom f = mapFromList
                      $ map (foldl (\(a,b) (c,d) -> (c, d:b)) (undefined, []))
                      $ groupBy (\(a, b) (c, d) -> a == c) . sort
                      $ map (\(a,b) -> (b,a))
                            $ mapToList (immediateDominators f)

graphToMap :: LIRGraph C C -> LabelMap [Label]
graphToMap (GMany entry labels exit) = mapMap successors labels

type DominanceFrontiers = LabelMap [Label]

mkDF :: LIRGraph C C -> FactBase DomFact -> (DominanceFrontiers, DominatorTree)
mkDF g facts =
    let domList = (mapToList facts)
        imDomFact = trace ("IMDOMS: " ++ show (mkImDom facts)) (mkImDom facts)
        dominatorTree = dtree domList
        postOrder = dominatorWalk dominatorTree
        program = graphToMap g
        dominanceFrontier :: LabelMap [Label]
        dominanceFrontier = mapUnion localDF upDF
          where
            localDF :: LabelMap [Label]
            localDF = mapMapWithKey localDominanceFrontier program

            upDF :: LabelMap [Label]
            upDF = mapMapWithKey upDominanceFrontier program

            localDominanceFrontier node successors =
                filter (\y -> y `notElem` iDom) successors
              where
                iDom :: [Label]
                iDom = case (mapLookup node imDomFact) of
                              Just res -> res
                              Nothing -> []

            upDominanceFrontier node successors =
                concatMap zf iDom
              where
                zf :: Label -> [Label]
                zf z = filter (\y -> y `notElem` iDom) (queryDF z)

                iDom :: [Label]
                iDom = case (mapLookup node imDomFact) of
                                Just res -> res
                                Nothing -> []

        queryDF z = case (mapLookup z dominanceFrontier) of
                        Just res -> res
                        Nothing -> []
    in (dominanceFrontier, dominatorTree)

mkDFSet :: DominanceFrontiers -> [Label] -> [Label]
mkDFSet df labels =
    concatMap ($df) (map lookup' labels)
  where
    lookup' :: Label -> DominanceFrontiers -> [Label]
    lookup' key map = case mapLookup key map of
                        Just res -> res
                        Nothing -> []

mkIteratedDF :: DominanceFrontiers -> [Label]  -> [Label]
mkIteratedDF df labels =
    loop (mkDFSet df labels)
  where
    loop dfp = let dfp' = mkDFSet df (union dfp labels)
               in if dfp /= dfp'
                    then loop dfp'
                    else dfp

instance Show DominatorTree where
  show = tree2dot

-- | Given a dominator tree, produce a string representation, in the
-- input language of dot, that will enable dot to produce a
-- visualization of the tree.  For more info about dot see
-- http://www.graphviz.org.

tree2dot :: DominatorTree -> String
tree2dot t = concat $ "digraph {\n" : dot t ["}\n"]
  where
    dot :: DominatorTree -> [String] -> [String]
    dot (Dominates root trees) = 
                   (dotnode root :) . outedges trees . flip (foldl subtree) trees
      where outedges [] = id
            outedges (Dominates n _ : ts) =
                \s -> "  " : show root : " -> " : show n : "\n" : outedges ts s
            dotnode Entry = "  entryNode [shape=plaintext, label=\"entry\"]\n"
            dotnode (Labelled l) = "  " ++ show l ++ "\n"
            subtree = flip dot

instance Show DominatorNode where
  show Entry = "entryNode"
  show (Labelled l) = show l

----------------------------------------------------------------

-- | Takes FactBase from dominator analysis and returns a map from each 
-- label to its immediate dominator, if any
immediateDominators :: FactBase DomFact -> LabelMap Label
immediateDominators = mapFoldWithKey add mapEmpty
    where add l (PElem (DPath (idom:_))) = mapInsert l idom 
          add _ _ = id

-- | This utility function handles a common case in which a transfer function
-- for a last node takes the incoming fact unchanged and simply distributes
-- that fact over the outgoing edges.
