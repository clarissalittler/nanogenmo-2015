import Rotations

{- 
   This is an example of using rotations to modify text based off of the king james version of the bible from Gutenberg

   The first thing we need to do is run through the text in order to figure out where our chapter and paragraph breaks should be.

   It'd probably be easiest to do something with parsec
-}


import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.String

{- 
   The basic layout is as follows:
   A chapter starts with a single line,
   followed by two extraneous newlines
   A chapter is followed by an arbitrary number of paragraphs
   ended with four extraneous newlines
  
-}

book = many1 (try chapter)

title :: Parser String
title = do
  ss <- many1 (alphaNum <|> oneOf " :,;")
  endOfLine
  return ss

chapter = do
  t <- title
  sequence $ replicate 2 endOfLine
  ps <- many1 (try paragraph)
  sequence $ replicate 4 endOfLine
  return (t,ps)

paragraph = do
  ss <- endBy1 (try sentence) (oneOf ".!?")
  endOfLine
  return ss

sentence = many sentenceChar
   where sentenceChar = alphaNum <|> oneOf " :,;-()'" <|> endOfLine

{- 
chapters = sepBy aChapter betwixt
    where betwixt = sequence $ replicate 6 endOfLine

aChapter = do
  t <- sentence
  sequence $ replicate 3 endOfLine
  ps <- paragraphs
  return $ (t,ps)

paragraphs = sepBy paragraph betwixt
    where betwixt = sequence $ replicate 2 endOfLine

paragraph = sepBy (try sentence) ((try (punctuation >> return ())) <|> (try (lookAhead versenum >> return ())))

sentence :: Parser String
sentence = many1 $ alphaNum <|> sentenceSpecials <|> aux
    where aux = do
            e <- endOfLine
            notFollowedBy endOfLine
            return e
sentenceSpecials = oneOf " ,;:'()-"


punctuation = oneOf ".?!"

versenum = do 
  many1 digit 
  char ':'
  many1 digit

-}
