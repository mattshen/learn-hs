module KnightMove1 (
    in3, KnightPos, canReachIn3 
) where

import Control.Monad

type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]  
    guard (c' `elem` [1..8] && r' `elem` [1..8])  
    return (c',r')  

-- Possible Positions in 3 moves
in3 :: KnightPos -> [KnightPos]
in3 start = do  
    first <- moveKnight start  
    second <- moveKnight first  
    third <- moveKnight second  
    return third 

canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start 

