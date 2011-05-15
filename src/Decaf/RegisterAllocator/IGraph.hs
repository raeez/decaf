{-# LANGUAGE GADTs, ScopedTypeVariables, TypeSynonymInstances, PatternGuards #-}

-- maybe bug: how are precolored registers spilled?

-- | Inteferance graph representation
module Decaf.RegisterAllocator.IGraph
  ( LabelGraph(..)
  , mkLabelGraph
  , addEdge, addEdges
  , colorGraph
  , Color(..)
  , getLabel
  , labelWithKey
  , keys
  --, manipulation  functions
  )
where
import Decaf.IR.ASM
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List hiding (delete)
import Data.Array hiding (index)
import Data.Maybe hiding (fromJust)

import Debug.Trace

-- CHANGE THIS
data LabelGraph a dat 
  = LabelGraph (M.Map a Int) 
               (M.Map Int dat)
               (Array (Int, Int) Bool)
  deriving (Show, Eq)


fromJust (Just x) = x
fromJust (Nothing) = error "from just error IGraph"


-- graph with no edges, vertices entered into map
mkLabelGraph :: (Ord a) => [a] -> LabelGraph a dat
mkLabelGraph things =
  LabelGraph (M.fromList $ zip things [1..n])
             (M.empty)
--             (M.fromList $ zip [1..n] $ replicate n $ Nothing)
             (array ((1,1),(n,n)) [((i,j), False) | i <- [1..n], j <- [1..n]])
  where 
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

adjacent :: (Ord a) => a -> a -> LabelGraph a dat -> Bool
adjacent a b g@(LabelGraph m _ array) = index a m `elem` neighbors b g

label a lab (LabelGraph map labels array) = 
  LabelGraph map (M.insert (index a map) lab labels) array

labelWithKey :: (k -> Maybe dat) -> (LabelGraph k dat) -> (LabelGraph k dat)
labelWithKey f (LabelGraph m labels array) = 
  let kas = map (\(x,y) -> (y,f x)) (M.assocs m) -- apply map to k's
  in LabelGraph m (foldr update labels kas) array
    where
      update (i, l) labs = case l of
                           Just l' -> M.insert i l' labs
                           Nothing -> labs

getLabel :: (Ord a) => a -> LabelGraph a dat -> Maybe dat
getLabel a (LabelGraph map labels _) 
  | Just k <- (M.lookup a map) -- extra safety check (not really)
  = M.lookup k labels
--  | otherwise = Nothing

allLabels :: (Ord a) => LabelGraph a dat -> [(a, Maybe dat)]
allLabels g@(LabelGraph m l _) = 
  let ks = M.keys m
  in zip ks (map (\k -> getLabel k g) ks)

keys (LabelGraph m _ _) = M.keys m

-- get set of (non Nothing) adjacent labels (relevant when labels are
-- colors, and we're graph coloring)
neighborLabels :: (Ord a, Ord dat) => a -> LabelGraph a dat -> S.Set dat
neighborLabels a g@(LabelGraph m labels array) = 
--  S.fromList $ map fromJust $ filter isJust $ map (fromJust . (flip M.lookup) labels) $ neighbors a g
  S.fromList $ map fromJust $ filter isJust $ map ((flip M.lookup) labels) $ neighbors a g

{-neighborColors :: (Ord a) => a -> IGraph -> S.Set Color
neighborColors a g = S.filter isReg $ neighborLabels a g
  where isReg (Reg {}) = True
        isReg _ = False
-}

-- assumes at most one is already labelled
merge ::(Ord a) => a -> a -> LabelGraph a dat -> LabelGraph a dat
merge a b g@(LabelGraph map labels array) =
  let inda = index a map
      indb = index b map in
  LabelGraph (M.insert b inda map) -- make b point to a's index
             (let lb = getLabel b g in
              case lb of 
                Just lb' -> M.insert inda lb' labels
                Nothing -> labels)
             (array // ( [((inda,i),True) | i <- neighbors b g] ++ -- add b's neighbors to a
                         [((i,inda),True) | i <- neighbors b g]))
             
             
             

-- isJust :: Maybe a -> Bool
-- isJust (Just a) = True
-- isJust Nothing = False


x = addEdge 'a' 'b' $ mkLabelGraph ['a', 'b', 'c']


{-data ASMReg = RAX | RDX | R10 | R11
            deriving (Show, Eq, Ord)
-}
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
    help :: LabelGraph a Color -> [a] -> Either [a] (LabelGraph a Color)
    help (LabelGraph m labels array) stack | M.size m == 0 = color g stack -- graph empty; color
    help g@(LabelGraph m labels array) stack = 
      case findCoalesce g of -- try coalescing
        Just (n1,n2) -> help (merge n1 n2 g) stack
        Nothing      ->  -- try to kill small valence nodes
          let keys = M.keys m
              goodnodes = filter ((> 0) . S.size . (\x -> colors `S.difference` x) .
                                  ((flip neighborLabels) g)) keys in
          if length goodnodes > 0 then
            help (deletes goodnodes g) (goodnodes ++ stack)
          else -- otherwise (potentially) spill something
            -- runtime error if something isn't labelled with spillcost
            let 
              argmin :: (Ord b) => [(k,b)] -> k
              argmin tuples = fst $ foldl min (head tuples) (tail tuples)
                where min (a,b) (a',b') = if b < b' then (a,b) else (a',b')

              mapSnd :: (d -> b) -> ((c,d) -> (c,b))
              mapSnd f (a,b) = (a, f b)

              spill = (argmin $ map (mapSnd fromJust) (allLabels costs))

--(zip keys (map spillCost keys)
            in  help (delete spill g) ([spill] ++ stack)

    findCoalesce gr = find coalOkay $ [(i,j) | i <- k, j <- k]
      where 
        k = keys gr
        -- nodes must have combined valence less than n so as not to induce spills
        -- and must be joined by a mov in the cost graph
        coalOkay :: (a,a) -> Bool
        coalOkay (n1, n2) 
          = (length $ neighbors n1 gr `union` neighbors n2 gr) < n 
          && not (adjacent n1 n2 gr)  -- don't conflict
          && adjacent n1 n2 costs     -- are connected by a mov
          

    color :: LabelGraph a Color -> [a] -> Either [a] (LabelGraph a Color)
    color g [] = 
      let spills = filter ((== Just Spill).snd) (allLabels g) in
      if length spills == 0 then
        Right g
      else Left $ map fst spills

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



--g = addEdges [1,2,3] $ addEdge 3 4 $ mkIGraph [1,2,3,4]
--h = addEdges [1,2,3,4] $ mkIGraph [1,2,3,4,5]
--colors = S.fromList $ Data.List.map Reg [RAX, RDX, R10, R11]