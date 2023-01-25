-- Task 5.1 
-- i
turnAround :: Direction -> Direction
data Direction = North | East | South | West deriving (Eq, Show)
turnAround North = South
turnAround South = North
turnAround West = East
turnAround East = West

-- ii 
turnRight :: Direction -> Direction
turnRight North = East 
turnRight East = South
turnRight South = West 
turnRight West = North

turnLeft :: Direction -> Direction 
turnLeft North = East 
turnLeft East = South
turnLeft South = West 
turnLeft West = North

-- Task 5.2 
--i
safetailMaybe :: [a] -> Maybe [a]
safetailMaybe [] = Nothing
safetailMaybe (_ : x) = Just x

--ii
takeMaybe :: Int -> [a] -> Maybe [a]
takeMaybe x xs = if x > length xs then Nothing else Just xs

--Task 5.3
data Btree a = Leaf a | Unary (Btree a) a | Binary (Btree a) a (Btree a)
ex1 :: Btree Integer
ex1 = Unary (Unary (Unary (Binary (Leaf 0) 1 (Leaf 2)) 3) 4) 5
ex2 :: Btree Integer
ex2 = Binary (Binary (Leaf 0) 1 (Leaf 2)) 3 (Binary (Leaf 4) 5 (Leaf 6))
--i 
depth :: Btree a -> Int
depth (Leaf x) = 0 
depth (Unary x y) = depth x + 1
depth (Binary x y z) = if depth x > depth z then depth x + 1 else depth z + 1

--ii
mapBtree :: (a -> b) -> Btree a -> Btree b
mapBtree f (Leaf x) = Leaf (f x)
mapBtree f (Unary x y) = Unary(mapBtree f x) (f y)
mapBtree f (Binary x y z) = Binary(mapBtree f x) (f y) (mapBtree f z)





