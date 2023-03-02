{-# LANGUAGE OverloadedStrings #-}

module DocDiff where

import qualified Data.Map as Map
import qualified Data.Text      as T
import qualified Data.Text.IO   as TIO
import Data.List (isSubsequenceOf, sort)

import Text.Megaparsec.Char   (string, digitChar, eol, newline, char)
import Text.Megaparsec
import Data.Void

type Filename = String
type H1       = (Maybe Int, T.Text)
type FileChunks = Map.Map Filename (Map.Map H1 T.Text)

preprocess :: [String] -> IO FileChunks
preprocess filenames = do
  chunks <- concat <$> mapM readfile filenames
  return $ Map.fromList chunks
  where
    readfile fn = do
      content <- TIO.readFile fn
      return $ [ ( fn
                 , Map.fromList $ filechunks fn content )
                ]


-- | we could use a complete markdown parser here, but let's keep it simple until we need one.
filechunks :: Filename -> T.Text -> [(H1, T.Text)]
filechunks fn content =
  case runParser ((,) <$> preamble fn <*> some (pChunk fn)) fn content of
    Left  x      -> fail $ errorBundlePretty x
    Right (p,xs) -> ((Just 0, "__PREAMBLE"), p) : xs

preamble :: Filename -> Parser T.Text
preamble fn = T.pack <$> manyTill anyChar (lookAhead $ pChunk fn)

dotDigitChar :: Parser Char
dotDigitChar = digitChar <|> char '.'
           
pChunk :: Filename -> Parser (H1, T.Text)
pChunk fn
  | "SAFTA" `isSubsequenceOf` fn = do
      let articleN :: Parser String
          articleN = "ARTICLE " *> some digitChar <* many eol
          pH1 :: Parser String
          pH1 = manyTill anyChar eol <* many "="
      aN <- articleN
      h1 <- pH1
      tx <- (T.pack <$> (manyTill anyChar (eof <|> () <$ lookAhead articleN)))
      return ((Just (read aN), T.pack h1), tx)
  | "ANZSCEP" `isSubsequenceOf` fn = do
      let h1 = (,)
               <$> ("**Article " *> (Just . read <$> some dotDigitChar) <* ": ")
               <*> (T.pack <$> manyTill anyChar (eof <|> (() <$ ("**" >> eol))))
      (,)
        <$> h1
        <*> (T.pack <$> manyTill anyChar (lookAhead h1))

anyChar = satisfy (const True)

type Parser = Parsec Void T.Text

normalize :: [T.Text] -> [T.Text]
normalize ts = ts

-- | compute approximate "hash" by calculating the word distribution signature of a given chunk

stats :: FileChunks -> IO ()
stats fchunks =
  putStrLn $ unlines [ "* " ++ filename ++ "\n" ++
                       unlines [ "** " ++ maybe "" show n ++ " " ++ T.unpack h1
                               | ((n,h1), body) <- sort (Map.toList filebody) ]
                     | (filename, filebody) <- Map.toList fchunks
                     ]
