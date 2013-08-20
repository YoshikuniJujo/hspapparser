{-# LANGUAGE QuasiQuotes #-}

import Language.Haskell.PapParser

x :: [haskell| forall a . (->) a a |]
x = undefined

y :: [haskell| (Int, Int) |]
y = (8, 2)
