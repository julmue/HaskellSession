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

-- Problem 5
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

rev' :: [a] -> [a]
rev' list = go list []
    where
      go [] acc = acc
      go (x:xs) acc = go xs (x:acc)
      
-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = list == rev' list

class MyEq a where 
    eq :: a -> a -> Bool
    eq x y = not (neq x y)
    neq :: a -> a -> Bool
    neq x y = not (eq x y)

instance MyEq Bool where
    eq True True = True
    eq False False = True
    eq _ _ = False

data Id a = Id a

instance Show a => Show (Id a) where
    show (Id a) = "Id " ++ show a
    
instance (Show p, Show q) => Show (Prod p q) where
    show (Prod p q) = "Prod " ++ show p ++ " " ++ show q
    
instance (Ord a) => Ord (Id a) where
    (Id x) <= (Id y) = x <= y
    
instance (Eq a) => Eq (Id a) where
    (Id x) == (Id y) = x == y 
    
instance Functor Id where
    fmap f (Id a) = Id (f a)
    
data MyMaybe a =
    MyNothing
    | MyJust a
    deriving Show
    
instance Functor MyMaybe where
    fmap _ (MyNothing) = MyNothing
    fmap f (MyJust a) = MyJust (f a)
    
    
instance Functor MyList where
    fmap _ (Nil) = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

graph :: (Enum a, Eq a) => (a -> b) -> a -> a -> [(a, b)]
graph f lower upper =
    if lower == upper then []
    else (lower, f(lower)) : graph f (succ lower) upper
    
graph' :: (Enum a) => (a -> b) -> a -> a -> [(a, b)]
graph' f lower upper = zip list (fmap f list)
    where list = [lower..upper]
    
graph'' :: (Enum a) => (a -> b) -> a -> a -> [(a, b)]
graph'' f lower upper = 
    let domain = [lower..upper]
    in domain `zip` (fmap f domain)
    
graph''' :: (Enum a) => (a -> b) -> a -> a -> [(a, b)]
graph''' f lower upper =
    fmap (\x -> (x, f x)) [lower..upper]
    
graph'''' :: (Enum a) => (a -> b) -> a -> a -> [(a, b)]
graph'''' f lower upper = (\x -> (x, f x)) <$> [lower..upper]

-- graph' f lower upper = (lower, f(lower)) : graph' f (succ lower) upper
