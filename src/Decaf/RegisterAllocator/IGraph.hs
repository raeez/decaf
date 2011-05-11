{-# LANGUAGE GADTs, ScopedTypeVariables, TypeSynonymInstances #-}

-- | Inteferance graph representation
module Decaf.RegisterAllocator.IGraph
  ( LabelGraph(..)
  , mkLabelGraph
  , addEdge, addEdges
  , colorGraph
  --, manipulation  functions
  )
where
import Decaf.IR.ASM
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List hiding (delete)
import Data.Array hiding (index)
import Data.Maybe

-- CHANGE THIS
data LabelGraph a dat 
  = LabelGraph (M.Map a Int) 
               (M.Map Int (Maybe dat))
               (Array (Int, Int) Bool)
  deriving (Show, Eq)

-- graph with no edges, vertices entered into map
mkLabelGraph :: (Ord a) => [a] -> LabelGraph a dat
mkLabelGraph things =
  LabelGraph (M.fromList $ zip things [1..n])
             (M.fromList $ zip [1..n] $ replicate n $ Nothing)
             (array ((1,1),(n,n)) [((i,j), False) | i <- [1..n], j <- [1..n]])
  where 
    fromList i [] = M.empty
    fromList i (x:xs) = M.insert x i (fromList (i+1) xs)

    n = length things

index :: (Ord a) => a -> M.Map a Int -> Int
index a m =
 case M.lookup a m of 
   Just x -> x
   Nothing -> error "improper index into LabelGraph"

addEdge a b (LabelGraph map labels array) = 
  let i1 = index a map
      i2 = index b map
  in
    LabelGraph map labels (array // [((i1,i2), True), ((i2,i1), True)])

addEdges :: (Ord a) => [a] -> LabelGraph a dat -> LabelGraph a dat
addEdges newset (LabelGraph map labels array) =
  let k = length newset
      newinds :: [Int]
      newinds = Prelude.map (fromJust . (\x -> M.lookup x map)) newset
      tuples :: [(Int,Int)]
      tuples = zip (concatMap (replicate k) newinds) (concat $ replicate k newinds)
      goodtuples = filter notdiag tuples
        where notdiag (a,b) = if a == b then False else True
  in LabelGraph map labels (array // (zip goodtuples (replicate (length goodtuples) True)))

delete :: (Ord a) => a -> LabelGraph a dat -> LabelGraph a dat
delete a (LabelGraph map labels array) = 
  let k = index a map
      n = M.size map in
  LabelGraph (M.delete a map)
             (M.delete k labels)
             (array // ([((k,i), False) | i <- [1..n]] ++ [((i,k),False) | i <- [1..n]]))
deletes as g = foldr delete g as
    

-- neighbor indices
neighbors :: (Ord a) => a -> LabelGraph a dat -> [Int]
neighbors a (LabelGraph m _ array) = 
  map (snd . fst) $ filter adj $ assocs array
  where ind = index a m
        adj ((i,j), True) | i == ind = True -- okay, since (i,i) is never True
        adj _ = False

numNeighbors a g = length $ neighbors a g

adjacent :: a -> a -> LabelGraph a dat -> Bool
adjacent a b g@(LabelGraph m _ array) = index a m `elem` neighbors b g

label a lab (LabelGraph map labels array) = 
  LabelGraph map (M.insert (index a map) (Just lab) labels) array

labelWithKey :: (k -> Maybe dat) -> (LabelGraph k dat) -> (LabelGraph a dat)
labelWithKey f (LabelGraph m labels array) = 
  LabelGraph m (M.mapWithKey (\k _ -> f k) labels) array

getLabel :: a -> LabelGraph a dat -> Maybe dat
getLabel a (LabelGraph map labels _) = M.lookup (M.lookup a map) labels

labels (LabelGraph _ l _) = l

keys (LabelGraph m _ _) = M.keys m

-- get set of (non Nothing) adjacent labels (relevant when labels are
-- colors, and we're graph coloring)
neighborLabels :: (Ord a, Ord dat) => a -> LabelGraph a dat -> S.Set dat
neighborLabels a g@(LabelGraph m labels array) = 
  S.fromList $ map fromJust $ filter isJust $ map (fromJust . (flip M.lookup) labels) $ neighbors a g

neighborColors :: (Ord a) => a -> IGraph a -> S.Set Color
neighborColors a g = S.filter isReg $ neighborLabels a g
  where isReg (Reg {}) = True
        isReg _ = False


-- assumes at most one is already labelled
merge :: a -> a -> LabelGraph a dat -> LabelGraph a dat
merge a b g@(LabelGraph map labels array) =
  let inda = index a map
      indb = index b map in
  LabelGraph (M.insert b (fromJust $ M.lookup a map)) -- make b point to a's index
             (let lb = getLabel b g in
              if isJust lb then -- in case b is pre-labelled with register
                M.insert inda lb labels
              else labels)
             (array // [((inda,i),True) | i <- neighbors b g] ++ -- add b's neighbors to a
                       [((i,inda),True) | i <- neighbors b g])
             
             
             

-- isJust :: Maybe a -> Bool
-- isJust (Just a) = True
-- isJust Nothing = False


x = addEdge 'a' 'b' $ mkLabelGraph ['a', 'b', 'c']




data Color = Reg ASMReg
           | Spill
  deriving (Show, Eq, Ord)



-- CHANGE make coalesce first
-- :: color set -> interferance graph
-- -> coalescing/spill cost graph (labels are costs, edges are mov codes)
-- -> colored interferance graph
colorGraph :: forall a. (Ord a) => S.Set Color  -- colors
           -> (LabelGraph a Color, LabelGraph a Int) -- IGraph, CGraph
           -> Either [a] (LabelGraph a Color) -- either list of spills or a colored graph
colorGraph colors (g, costs) = help g []
  where 
    n = S.size colors
    help :: IGraph a -> [a] -> IGraph a
    help (LabelGraph map labels array) stack | M.size map == 0 = color g stack -- graph empty; color
    help g@(LabelGraph map labels array) stack = 
      case findCoalesce g of -- try coalescing
        Just (n1,n2) -> help (merge n1 n2 g) stack
        Nothing      ->  -- try to kill small valence nodes
          let keys = M.keys map
              goodnodes = filter ((< n) . ((flip numNeighbors) g)) keys in
          if length goodnodes > 0 then
            help (deletes goodnodes g) (goodnodes ++ stack)
          else -- otherwise (potentially) spill something
            -- runtime error if something isn't labelled with spillcost
            let spill = argmin $ map (mapSnd fromJust)
                        $ zip keys (Prelude.map spillCost keys) 
            in  help (delete spill g) (spill : stack)

    findCoalesce gr = find coalOkay $ [(i,j) | i <- k, j <- k]
      where 
        k = keys gr
        -- nodes must have combined valence less than n so as not to induce spills
        -- and must be joined by a mov in the cost graph
        coalOkay :: (a,a) -> Bool
        coalOkay (n1, n2) 
          = (size $ neighbors n1 `union` neighbors n2) < n 
          && not (adjacent n1 n2 gr)  -- don't conflict
          && adjacent n1 n2 costs     -- are connected by a mov
          

    color :: LabelGraph a Color -> [a] -> Either [a] (LabelGraph a Color)
    color g [] = 
      let spills = filter (== Spill) (labels g) in
      if size spills == 0 then
        Right g
      else Left $ keys spills

    color g (node:ns) = 
      let adjcolors = neighborLabels node g in
        color (label node (newColor adjcolors) g) ns

    spillCost x = getLabel x costs

    newColor :: S.Set Color -> Color -- might be Spill
    newColor used = let left = colors `S.difference` used in
                    if S.size left == 0 then
                      Spill
                    else
                      S.findMin left -- take the smallest one I guess

    argmin :: (Ord b) => [(k,b)] -> k
    argmin tuples = fst $ foldl min (head tuples) (tail tuples)
      where min (a,b) (a',b') = if b < b' then (a,b) else (a',b')

    mapSnd f (a,b) = (a, f b)


g = addEdges [1,2,3] $ addEdge 3 4 $ mkIGraph [1,2,3,4]
h = addEdges [1,2,3,4] $ mkIGraph [1,2,3,4,5]
colors = S.fromList $ Data.List.map Reg [RAX, RDX, R10, R11]