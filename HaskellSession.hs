fun1 x = x + x
fun1' = \x -> x + x

data MyEither a b
    = MyLeft a
    | MyRight b

data SumCharInt
    = L Char
    | R Int
    deriving (Show, Eq)

data SumType
    = DataConstr1
    | DataConstr2
    | DataConstr3
    deriving (Show, Eq)

data Prod a b
    = Prod a b
    deriving(Show)

projLeft :: Prod a b -> a
projLeft (Prod a _) = a

projRight :: Prod a b -> b
projRight (Prod _ a) = a

data MyList a 
    = Nil
    | Cons a (MyList a)
    deriving(Show, Eq)

data Tree a
    = Node (Tree a) (Tree a)
    | Leaf a
    deriving(Show)

myLast :: [a] -> a
myLast [] = error "boom!"
myLast [a] = a
myLast (_:xs) = myLast xs
    
safeMyLast :: [a] -> Maybe a
safeMyLast [] = Nothing
safeMyLast [a] = Just a
safeMyLast (_:xs) = safeMyLast xs

myLastBefore :: [a] -> Maybe a
myLastBefore [] = Nothing
myLastBefore [a] = Nothing
myLastBefore (a:_:[]) = Just a
myLastBefore (_:xs) = myLastBefore xs

elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt (x:xs) i
    | i < 0 = Nothing
    | i == 0 = Just x
    | otherwise = elementAt xs (pred i)

len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs
