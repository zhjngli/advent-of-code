module Main (main) where

import Day01.Solution
import Day02.Solution
import Day03.Solution

main :: IO ()
main = do
    Day03.Solution.solve
    Day02.Solution.solve
    Day01.Solution.solve
