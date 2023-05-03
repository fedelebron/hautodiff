module Utils (replace, inversePerm) where

import Data.List (sort)

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map (\x -> if x == a then b else x)

inversePerm :: [Int] -> [Int]
inversePerm x = map snd $ sort $ zip x [0 .. length x - 1]
