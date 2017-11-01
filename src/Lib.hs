module Lib
    ( someFunc,
      finiteRandoms,
      keepSmall,
      powerset
    ) where

import System.Random
import Control.Monad
import Control.Monad.Writer

someFunc :: IO ()
someFunc = putStrLn "someFunc"

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)  
finiteRandoms 0 gen = ([], gen)  
finiteRandoms n gen =   
    let (value, newGen) = random gen  
        (restOfList, finalGen) = finiteRandoms (n-1) newGen  
    in  (value:restOfList, finalGen)


keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False  

powerset :: [a] -> [[a]]  
powerset xs = filterM (\x -> [True, False]) xs 