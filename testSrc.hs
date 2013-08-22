import Language.Haskell.PapParser

import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	src <- readFile fp
	print $ haskellSrc src
