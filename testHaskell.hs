{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import Language.Haskell.PapParser

x :: [haskell| forall a . Num a => (->) a a |]
x = undefined

xx :: [haskell| (Num a, Eq a) => a -> a |]
xx = id

y :: [haskell| (Int, Int) |]
y = [haskell|

let	x = 3
        y = 2 in (x, y)

|]

yy :: Int
yy = [haskell|

let	x = 5
        y = 10
-- hage
in x + y

-- hoge

|]

yyy :: Int
yyy = [haskell|

let	x = 5
        y = 10
                    in x + y

|]

yyyy :: Int
yyyy = [haskell|

let
x = 8
y = 9
in x + y

|]

yyyyy :: Int
yyyyy = [haskell|

let
in 888

|]

times :: Int -> IO () -> IO ()
times 0 _ = return ()
times n act = act >> times (n - 1) act

z :: IO ()
z = [haskell|

do print 8
   print 10000
   times 2 $ do print 1500000
                times 3 $ do
			print 25
			print 1
   print 7000
   print 9

|]

w :: Int
w = [haskell|

let x = case 33 of
	25 -> eleven where eleven = 11
	32 -> 4
	33 -> eee where eee = 888 in x

|]

data Some = Some {
	some :: Int,
	other :: String }
	deriving Show

v :: Some
v = [haskell|

Some { some = 8, other = "hoge" }

|]

u :: Some
u = [haskell|

v { other = "eight" }

|]

[haskell|

t = t' where t' = t''
	     t'' = 999;
s = 32; r = 95

someFun 3 = 8;

someFun 9 = 77
otherFun 8 = 7

data Other = Other Other | Null deriving Show

newtype Hoge = Hoge Int deriving (Show, Eq)

type Hage = Hoge

-- line comment

class SomeClass a where
	type SomeType a
	data SomeData a
	hoge :: a -> a
	hage :: a

instance SomeClass Other where

{- comment {- nested comment -} {- nested comment -} nested-}

instance SomeClass Hoge where
	type SomeType Hoge = Int
	data SomeData Hoge = Hogeru
	hoge = id
	hage = Hoge 3

-- line comment

instance SomeClass Some where

-- line comment

|]
