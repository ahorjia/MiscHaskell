-- Disjoint set data type (weighted and using path compression).
-- O((M+N)lg*N + 2MlogN) worst-case union time (practically O(1))
-- For M union operations on a set of N elements.
-- O((M+N)lg*N) worst-case find time (practically O(1))
-- For M connected(find) operations on a set of N elements.
data DisjointSet = DisjointSet
     { count :: Int, ids :: (Seq Int), sizes :: (Seq Int) }
     deriving (Read,  Show)

-- Return id of root object
findRoot :: DisjointSet -> Int -> Int
findRoot set p | p == parent = p
               | otherwise   = findRoot set parent
               where
                parent = index (ids set) (p - 1)

-- Are objects P and Q connected ?
connected :: DisjointSet -> Int -> Int -> Bool
connected set p q = (findRoot set p) == (findRoot set q)

-- Replace sets containing P and Q with their union
quickUnion :: DisjointSet -> Int -> Int -> DisjointSet
quickUnion set p q | i == j = set
                   | otherwise = DisjointSet cnt rids rsizes
                     where
                        (i, j)   = (findRoot set p, findRoot set q)
                        (i1, j1) = (index (sizes set) (i - 1), index (sizes set) (j - 1))
                        (cnt, psmaller, size) = (count set - 1, i1 < j1, i1 + j1)
                        -- Always make smaller root point to the larger one
                        (rids, rsizes) = if psmaller
                                         then (update (i - 1) j (ids set), update (j - 1) size (sizes set))
                                         else (update (j - 1) i (ids set), update (i - 1) size (sizes set))
										 
