module Main where


import Lib
import FileLines
import KnightMove1


main :: IO ()
main = do
  putStrLn $ show $ canReachIn3 (6,2) (6,1)
