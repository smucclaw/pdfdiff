{-# LANGUAGE OverloadedStrings #-}

module DocDiff where

import qualified Data.Map as Map
import qualified Data.Text      as T
import qualified Data.Text.IO   as TIO
import Data.Maybe (fromMaybe)
import Data.List (isSubsequenceOf, sort, sortOn)

import Text.Megaparsec.Char   (string, digitChar, eol, newline, char)
import Text.Megaparsec
import Data.Void
import Control.Monad (void)

type Filename = String
type H1       = (Maybe T.Text, T.Text)
type FileChunks = Map.Map Filename (Map.Map H1 T.Text)

preprocess :: [String] -> IO FileChunks
preprocess filenames = do
  chunks <- concat <$> mapM readfile filenames
  return $ Map.fromList chunks
  where
    readfile fn = do
      content <- TIO.readFile fn
      putStrLn $ fn ++ ": read " ++ show (T.length content) ++ " bytes"
      return [ ( fn
               , Map.fromList $ filechunks fn content )
             ]

-- | we could use a complete markdown parser here, but let's keep it simple until we need one.
filechunks :: Filename -> T.Text -> [(H1, T.Text)]
filechunks fn content =
  case runParser ((,) <$> preamble fn <*> some (pChunk fn) <* eof) fn content of
    Left  x      -> error $ errorBundlePretty x
    Right (p ,xs) -> ((Nothing, "__PREAMBLE"), p) : xs

preamble :: Filename -> Parser T.Text
preamble fn = T.pack <$> manyTill anyChar (lookAhead $ pChunk fn)

dotDigitChar :: Parser Char
dotDigitChar = digitChar <|> char '.'
           
pChunk :: Filename -> Parser (H1, T.Text)
pChunk fn = do
  let h1 = pH1 fn
  (,)
    <$> (mapPair (Just . T.pack, T.pack) <$> h1)
    <*> (T.pack <$> manyTill anyChar (eof <|> void (lookAhead h1)))
  where
    mapPair (f1,f2) (x,y) = (f1 x, f2 y)
    pH1 fn
      | "SAFTA" `isSubsequenceOf` fn = do
      let articleN = "ARTICLE " *> some dotDigitChar <* many eol
          pH1 = manyTill anyChar eol <* many "=" <* many eol
      (,) <$> articleN <*> pH1

      | "ANZSCEP" `isSubsequenceOf` fn = do
      let h1L = T.unpack <$> string "111"
          h1R = T.unpack <$> string "bar"
          h1starL = "**Article " *> some dotDigitChar <* ": "
          h1starR = manyTill anyChar ("**" >> eol)
          h1equalsL = "Article " *> some dotDigitChar <* ": "
          h1equalsR = manyTill anyChar eol <* manyTill (char '=') eol
      choice [ try $ (,) <$> h1L       <*> h1R
             , try $ (,) <$> h1starL   <*> h1starR
             , try $ (,) <$> h1equalsL <*> h1equalsR ]

      | otherwise = error ("not prepared to process file " ++ fn)
anyChar = satisfy (const True)

type Parser = Parsec Void T.Text

normalize :: [T.Text] -> [T.Text]
normalize ts = ts

-- | compute approximate "hash" by calculating the word distribution signature of a given chunk

stats :: FileChunks -> IO ()
stats fchunks =
  putStrLn $ unlines [ "* " ++ filename ++ "\n" ++
                       unlines [ "** " ++ maybe "" T.unpack n ++ " " ++ T.unpack h1 ++
                                 "  length:" ++ show (T.length body)
                                 -- ++ "\n" ++ T.unpack body
                               | ((n,h1), body) <- sortOn fstfst (Map.toList filebody) ]
                     | (filename, filebody) <- Map.toList fchunks
                     ]
  where fstfst ((mt,_),_) = fmap ((read :: (String -> Int)) . T.unpack) . T.splitOn "." <$> mt
