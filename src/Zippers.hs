module Zippers 
    (Tree, Directions, Direction(..), freeTree, changeToP, changeToP1) where

import Data.List
import Control.Monad

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
data Direction = L | R deriving (Show)
type Directions = [Direction]

freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        ) 

changeToP :: Tree Char -> Tree Char  
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r) 


changeToP1 :: Directions -> Tree Char -> Tree Char
changeToP1 (L:ds) (Node x l r) = Node x (changeToP1 ds l) r
changeToP1 (R:ds) (Node x l r) = Node x l (changeToP1 ds r)
changeToP1 [] (Node _ l r) = Node 'P' l r
