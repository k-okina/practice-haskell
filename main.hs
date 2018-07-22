add :: Int -> Int -> Int
add a b = a + b

add2 :: Int -> Int -> Int -> Int
add2 a b c = add a b + c

main :: IO ()
main = do {
  putStrLn "hello world";
  putStrLn "bye..." }

-- 代数的データ構造 (直積)
data Point = Point Int Int deriving Show
-- x :: Point
-- x = Point 2 3

flipPoint :: Point -> Point
flipPoint (Point a b) = Point (-a) (-b)

addPoint :: Point -> Point -> Point
addPoint (Point a b) (Point c d) = Point (a+b) (c+d)

-- 代数的データ構造 (直和)
data Animal = Cat String | Dog Int deriving Show
say :: Animal -> String
say (Cat s) = "myao! <- " ++ s
say (Dog l) = "waon! <- " ++ show l

-- 型変数
data Tuple a = Bird a a deriving Show
type Point2 = Tuple Int

flipPoint2 :: Point2 -> Point2
flipPoint2 (Bird a b) = Bird (-a) (-b)

flipPoint3 :: Point2 -> Int
flipPoint3 (Bird a _) = -a

double :: a -> Tuple a
double a = Bird a a

data Tabun a = Nai | Tyodo a

data List a = Nil | Cons a (List a) deriving Show

sample = Cons 1 $ Cons 2 $ Cons 3 Nil

-- nthelement 1 sample = nthelemnt 1 (Cons 1 xs) = nthelement 0 (Cons 2 $ Cons 3 Nil)

nthelement :: Int -> List a -> Tabun a
nthelement _ Nil = Nai
-- x :: a
-- xs :: List a
nthelement 0 (Cons x _) = Tyodo x
nthelement n (Cons _ xs) | n > 0 = nthelement (n-1) xs
nthelement n _ | n < 0 = Nai

head :: List a -> Tabun a
head = nthelement 0

tail2 :: List a -> Tabun a
tail2 Nil = Nai
tail2 (Cons x Nil) = Tyodo x
tail2 (Cons x xs) = tail2 xs

flip01 :: List a -> List a
flip01 Nil = Nil
flip01 l@(Cons x Nil) = l
flip01 (Cons x (Cons y xs)) = Cons y $ Cons x xs

data Tree a = Tree a [Tree a]

n2 = Tree 2 []
n3 = Tree 3 []
n5 = Tree 5 []
n4 = Tree 4 [n5]
sometree = Tree 1 [n2, n3, n4]

countLeafNumber :: Tree a -> Int
countLeafNumber (Tree tree []) = 1
countLeafNumber (Tree tree children) = sum $ map countLeafNumber children
