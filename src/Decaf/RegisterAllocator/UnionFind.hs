module Decaf.RegisterAllocator.UnionFind
  ( UnionSet
  , emptyUnion
  , join
  , getParent
  , listPartitions
  , assocFind
  )
where
import Data.Maybe

-- The list !! i = positive, then 
data  UnionSet a = UnionSet [(a, Int)] [Int] 
                   deriving Show
emptyUnion = UnionSet [] []

{-instance Show (UnionSet a) where
  show (UnionSet m l) = show l
-}

insert :: Eq a => a -> UnionSet a -> UnionSet a
insert a (UnionSet m l) = 
  let size = length m in
  UnionSet (m++[(a, size)]) (l++[-1])

-- returns list element corresponding to a
ulookup :: Eq a => a -> UnionSet a -> Maybe Int
ulookup a (UnionSet m l) = 
  case lookup a m of
    Nothing -> Nothing
    Just i -> Just $ l !! i

modify :: Int -> Int -> UnionSet a -> UnionSet a
modify i num (UnionSet m l) = 
  UnionSet m (setbang i num l)
  where
    setbang i num l = 
      (take i l) ++ (num : (drop (i+1) l)) -- remember 0 indexing


-- finds parent and size
find :: Eq a => a -> UnionSet a -> (Int,Int)
find a (UnionSet m l) = 
  help (fromJust $ lookup a m) l
  where 
    help i l = if l !! i < 0 then (i, -(l !! i)) else help (l !! i) l

-- joins b to a in us
join :: Eq a => a -> a -> UnionSet a -> UnionSet a
join a b u | a == b = 
  if ulookup a u == Nothing then insert a u else u
                      
join a b u = 
  let u' = 
        let ma = ulookup a u
            mb = ulookup b u 
        in
          if ma == Nothing then
            insert a $
                   if mb == Nothing then
                     insert b u
                   else
                     u
          else
            if mb == Nothing then
              insert b u
            else
              u
      -- parent _, size _
      (pa, sa) = find a u'
      (pb, sb) = find b u'
  in
    if sa > sb then
      -- modify web pb to have parent pa, and change pa's size to be the sum of the sizes
      modify pb pa $ modify pa (-(sa+sb)) u'
    else
      modify pa pb $ modify pb (-(sa+sb)) u'

  

-- m  is a list of ((reg,node),int), for instance
assocFind :: Eq a => a -> UnionSet (a,b) -> (a,b)
assocFind key (UnionSet m l) = (key, fromJust $ lookup key (map fst m))


-- gets parent node
getParent :: Eq a => a -> UnionSet a -> a
getParent a us@(UnionSet m l) = 
  let (p, _) = find a us 
  in fst $ m !! p
--lists the parent of each web
listPartitions :: Eq a => UnionSet a -> [a]
listPartitions us@(UnionSet m _) = foldl appendnew [] m
  where
--    appendnew :: [a] -> (a,Int) -> [a]
    appendnew seen (a,_) = if (getParent a us) `elem` seen then seen else a:seen

      