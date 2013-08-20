(.*.) :: Int -> Int -> Int
(.*.) = (*)

main = print $ 3 .*. 2 + 5

infixl 2 .*.
