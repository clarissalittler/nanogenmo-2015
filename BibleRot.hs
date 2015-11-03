import Rotations
import Data.List
import Control.Monad.Random

{- 
   This is an example of using rotations to modify text based off of the king james version of the bible from Gutenberg

   The first thing we need to do is run through the text in order to figure out where our chapter and paragraph breaks should be.

   It'd probably be easiest to do something with parsec
-}

{- 
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.String
-}

{- 
   The basic layout is as follows:
   A chapter starts with a single line,
   followed by two extraneous newlines
   A chapter is followed by an arbitrary number of paragraphs
   ended with four extraneous newlines
  
-}

{- we'll assume that a "lines" has already performed on the string -}

endOfLine :: String
endOfLine = "\r\n"

breakIntoChapters :: String -> [String]
breakIntoChapters = breakBySubstring $ concat $ (replicate 5 endOfLine)

breakChapter :: String -> (String,String)
breakChapter s = let cs = breakBySubstring (concat (replicate 3 endOfLine)) s
                 in (cs !! 0, cs !! 1)

breakParagraph :: String -> [String]
breakParagraph = breakBySubstring $ concat (replicate 2 endOfLine)

breakBySubstring :: Eq a => [a] -> [a] -> [[a]]
breakBySubstring sub l = breakBySubstring' l sub []

breakBySubstring' :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
breakBySubstring' [] sub accum = (reverse $ map reverse $ dropWhile null accum)
breakBySubstring' (x:xs) sub accum = if take (length sub) (x:xs) == sub 
                                     then breakBySubstring' (drop (length sub) (x:xs)) sub ([]:accum)
                                     else breakBySubstring' xs sub (addFirst x accum)
addFirst x [] = [[x]]
addFirst x (l : ls) = (x:l) : ls

breakSentences s = breakSentences' s []
                     
breakSentences' [] acc = (reverse acc)
breakSentences' xs@(x:xs') acc = case break (\c -> c `elem` ".?!") xs of
                                   (left,r:right) -> breakSentences' right ((left++[r]):acc)
                                   (left,[]) -> breakSentences' [] (left:acc)

parseBible :: String -> [(String,[[String]])]
parseBible s = let cs = breakIntoChapters s
                   tps = map breakChapter cs
                   tpps = map (\(t,c) -> (t,breakParagraph c)) tps
               in map (\(t,ps) -> (t, map breakSentences ps)) tpps

flattenBible :: [(String,[[String]])] -> String
flattenBible b = concat (map flattenChapter b)

flattenChapter (t,ps) = t ++ endOfLine ++ endOfLine ++ endOfLine ++ (concat $ map flattenParagraph ps)

flattenParagraph p = concat $ intersperse "\r\n" p
                   
rotateBible :: IO ()
rotateBible = do
  s <- readFile "kjv10.txt"
  let b = parseBible s
  b' <- evalRandIO $ rotateChapterWithTitle b
  writeFile "rotatedBible2.txt" (flattenBible b')
