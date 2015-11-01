module Rotations where

{- 
   a simple little program to take in text and perform a series of "rotations" on the text to change the order of things. 

   We're going to do this in phases.

   1. We break the text into lists-of-lists of paragraphs of sentences
   2. Pick pivot points in the paragraphs, rotate around them in order
   3. Pick pivot points within the sentences, rotate around them in order

Now, we could also just have this be lists-of-lists-of-lists as chapters of paragraphs of sentences but I don't know if that's necessarily useful.
-}

import Control.Monad.Random
import Control.Monad

rotate :: Int -> [a] -> [a]
rotate i lst = let (leftl,rightl) = splitAt i lst in rightl ++ leftl

rotateRand :: RandomGen g => [a] -> Rand g [a]
rotateRand ls = do
  i <- getRandomR (0,length ls)
  return $ rotate i ls

rotatesRand :: RandomGen g => [a] -> Rand g [a]
rotatesRand ls = do
  num <- getRandomR (0, length ls `div` 3)
  foldM (\l _ -> rotateRand l) ls [0..num]

rotateNested :: RandomGen g => [[a]] -> Rand g [[a]]
rotateNested ls = do
  ls' <- rotatesRand ls
  mapM rotatesRand ls'

rotateChapter :: RandomGen g => [[[String]]] -> Rand g [[[String]]]
rotateChapter cs = do
  cs' <- rotatesRand cs
  mapM rotateNested cs'
