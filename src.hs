{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

module Main (
	what, You(go, to, school, (:+)), (*)
) where

import Boke.Kasu

import Hoge.Hage(what, happend)
import qualified Hoge.Hage as HH hiding (what, happend)

main :: IO ()
main = do
	y@(x : _) <- getArgs
	print $ 8
	print $ 8 `div` 9
