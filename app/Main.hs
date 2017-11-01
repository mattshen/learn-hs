module Main where


import Lib
import FileLines
import KnightMove


main :: IO ()
main = do
  putStrLn $ show $ routesIn3 (6,2) (6,1)