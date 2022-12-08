module Main (main) where

import Day01.Solution
import Day02.Solution
import Day03.Solution
import Day04.Solution
import Day05.Solution
import Day06.Solution
import Day07.Solution

main :: IO ()
main = do
    Day07.Solution.solve
    Day06.Solution.solve
    Day05.Solution.solve
    Day04.Solution.solve
    Day03.Solution.solve
    Day02.Solution.solve
    Day01.Solution.solve
