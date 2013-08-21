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
   print 10
   times 3 $ do print 15
                print 25
   print 7
   print 9

|]
