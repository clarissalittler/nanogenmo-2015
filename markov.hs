-- let's have a little file for taking the KJV and making a little bible verse generator

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.Random
import Data.Char
import Data.List
import Control.Monad

import BibleRot

type Table = Map [String] [(String, Rational)] -- invariant: the lists all must be the same length or else the algorithm doesn't work


-- if what you're looking for doesn't show up then let's presume that we can drop down to just the
-- last word used as a singlet instead
nextStep :: RandomGen g => [String] -> Table -> Rand g String
nextStep ss t = case (M.lookup ss t) of
                  Nothing -> case (M.lookup [last ss] t) of
                               Nothing -> error $ "howwww: " ++ (last ss)
                               Just ws -> fromList ws
                  Just ws -> fromList ws

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
toWords = words . map toLower . filter (\x -> isAlpha x || isSpace x)
   where punc c = and [c /= '.', c /= '!', c /= ',', c /= '"', c /= ';', c /= ',', c/= ':'] 
         digit c = and $ map (\d -> c /= chr d) [0..9] 

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

verseGenerator :: String -> Int -> IO ()
verseGenerator f n = do
  s <- readFile f
  let strs = toWords $ take 1000000 s
      tab1 = groupsToFrequencies $ nGroup 3 strs
      tab2 = groupsToFrequencies $ nGroup 2 strs
      tab = M.union tab1 tab2
  x <- liftM unwords $ evalRandIO $ generateStrings n ["the", "lord"] tab
  putStrLn $ "the lord " ++ x
