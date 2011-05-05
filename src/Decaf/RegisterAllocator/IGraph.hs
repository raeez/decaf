{-# LANGUAGE GADTs, ScopedTypeVariables, TypeSynonymInstances #-}

-- | Inteferance graph representation
module Decaf.RegisterAllocator.IGraph
  ( LabelGraph(..)
  , IGraph
  , mkIGraph
  , Colorable
  --, manipulation  functions
  )
where
--import Decaf.IR.LIR
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List hiding (delete)
import Data.Array hiding (index)
import Data.Maybe

-- CHANGE THIS
data LabelGraph a dat 
  = LabelGraph (M.Map a Int) 
               (M.Map Int dat)
               (Array (Int, Int) Bool)
  deriving (Show, Eq)

-- graph with no edges, vertices entered into map
mkLabelGraph :: (Ord a) => [a] -> dat -> LabelGraph a dat
mkLabelGraph things init =
  LabelGraph (M.fromList $ zip things [1..n])
             (M.fromList $ zip [1..n] $ replicate n init)
             (array ((1,1),(n,n)) [((i,j), False) | i <- [1..n], j <- [1..n]])
  where 
    fromList i [] = M.empty
    fromList i (x:xs) = M.insert x i (fromList (i+1) xs)

    n = length things

index :: (Ord a) => a -> M.Map a Int -> Int
index a m = 
  fromJust $ M.lookup a m

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
        adj ((i,j), True) | i == ind = True
        adj _ = False

numNeighbors a g = length $ neighbors a g

label a lab (LabelGraph map labels array) = 
  LabelGraph map (M.insert (index a map)  lab labels) array

labels (LabelGraph _ l _) = l

-- get set of adjacent labels (relevant when labels are colors, and we're graph coloring)
neighborLabels :: (Ord a, Ord dat) => a -> LabelGraph a dat -> S.Set dat
neighborLabels a g@(LabelGraph m labels array) = 
  S.fromList $ map (fromJust . (flip M.lookup) labels) $ neighbors a g

neighborColors :: (Ord a) => a -> IGraph a -> S.Set Color
neighborColors a g = S.filter isReg $ neighborLabels a g
  where isReg (Reg {}) = True
        isReg _ = False




x = addEdge 'a' 'b' $ mkLabelGraph ['a', 'b', 'c'] 0



-- | Interference Graph
-- change String to LIRReg
type IGraph a = LabelGraph a Color
mkIGraph regs = mkLabelGraph regs Null

-- need more?
class Colorable a where
  spillCost :: a -> Int

instance Colorable String where
  spillCost s = 0
instance Colorable Integer where
  spillCost s = 0

data Color = Reg Int -- change to LIRReg
           | Spill
           | Null -- used for init
  deriving (Show, Eq, Ord)

-- this needs to change
newColor :: S.Set Color -> Color
newColor colors = fromJust $ find (not . (flip S.member) colors) (map Reg [1..])




colorIGraph :: forall a. (Colorable a, Ord a) => Int -> IGraph a -> IGraph a
colorIGraph n g = help g []
  where 
    help :: IGraph a -> [a] -> IGraph a
    help (LabelGraph map labels array) stack | M.size map == 0 = color g stack
    help g@(LabelGraph map labels array) stack = 
      let keys = M.keys map
          goodnodes = filter ((< n) . ((flip numNeighbors) g)) keys in
      if not $ null goodnodes then
        help (deletes goodnodes g) (goodnodes ++ stack)
      else
--        help (delete (keys !! 0) g) ((keys !! 0) : stack)
        let spill = argmin $ zip keys (Prelude.map spillCost keys) 
        in  help (delete spill g) (spill : stack)

    argmin :: (Ord b) => [(k,b)] -> k
    argmin tuples = fst $ foldl min (head tuples) (tail tuples)
      where min (a,b) (a',b') = if b < b' then (a,b) else (a',b')

    color :: IGraph a -> [a] -> IGraph a
    color g [] = g
    color g (node:ns) = 
      let adjcolors = neighborColors node g in
      if S.size adjcolors < n then
        color (label node (newColor adjcolors) g) ns
      else color (label node Spill g) ns
      


g = addEdges [1,2,3] $ addEdge 3 4 $ mkIGraph [1,2,3,4]
h = addEdges [1,2,3,4] $ mkIGraph [1,2,3,4,5]