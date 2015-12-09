-- let's have a little file for taking the KJV and making a little bible verse generator

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.Random
import Data.Char
import Data.List

import BibleRot

type Table = Map [String] [(String, Rational)] -- invariant: the lists all must be the same length or else the algorithm doesn't work

nextStep :: RandomGen g => [String] -> Table -> Rand g String
nextStep ss t = fromList $ fromJust $ (M.lookup ss t) 

-- the argument for the very first call needs to be a seed, from there the further
-- calls are generated from the results of the previous call
generateStrings :: RandomGen g => Int -> [String] -> Table -> Rand g [String]
generateStrings 0 _ _ = return []
generateStrings n strs t = do 
  s <- nextStep strs t 
  ss <- generateStrings (n-1) (tail strs ++ [s]) t
  return $ s : ss

-- now we make a table from a file
-- parseBible returns [(String, [[String]])] which isn't quite right format. 
-- Need to cut punctuation out and turn to all lower case as well
toWords :: String -> [String]
toWords =  map (map toLower) . map (filter punc) . concat . concat . map snd . parseBible
   where punc c = and [c /= '.', c /= '!', c /= ',', c /= '"', c /= ';', c /= ','] 

nGroup :: Int -> [a] -> [[a]]
nGroup n [] = []
nGroup n xs = (take n xs) : (nGroup n (drop n xs))
-- we're going to specialize to groups of 3 from here on

-- the invariant here is that the length of the first arg is one less than the lengths of
-- the inner lists of the second arg and lord am I dying for vector types right about now
prefixToFrequencies :: Eq a => [a] -> [[a]] -> [(a, Rational)] 
prefixToFrequencies testXs groups = let short = length testXs
                                        groups' = group $ filter (\xs -> take short xs == testXs) groups
                                    in map (\l -> (last (head l), fromIntegral $ length l)) groups'

dropLast :: [a] -> [a]
dropLast = reverse . tail . reverse

groupsToFrequencies :: (Ord a , Eq a) => [[a]] -> Map [a] [(a, Rational)]
groupsToFrequencies ts = let tGroups = nub $ map dropLast ts
                         in M.fromList $ map (\ x -> (x, prefixToFrequencies x ts)) tGroups
