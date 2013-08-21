{-# LANGUAGE QuasiQuotes #-}

import Language.Haskell.PapParser

x :: [haskell| forall a . (->) a a |]
x = undefined

y :: [haskell| (Int, Int) |]
y = [haskell|

let	x = 3
        y = 2 in (x, y)

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
	25 -> 11
	32 -> 4
	33 -> 888 in x

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
