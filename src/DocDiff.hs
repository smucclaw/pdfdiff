{-# LANGUAGE OverloadedStrings #-}

module DocDiff where

import qualified Data.Map as Map
import qualified Data.Text      as T
import qualified Data.Text.IO   as TIO
import Data.Maybe (fromMaybe, listToMaybe, isJust)
import Data.List (isSubsequenceOf, sort, sortOn, isPrefixOf)

import Text.Megaparsec.Char as TMC  (string, digitChar, eol, newline, char, space)
import Text.Megaparsec
import Data.Void
import Control.Monad (void, when, forM_)

import Data.Char (toLower)
import Data.Text.Metrics

import Text.PrettyPrint.Boxes

type Filename = String
type H1       = (Maybe T.Text, T.Text)
type FileChunks = Map.Map Filename Articles
type Articles   = Map.Map H1 T.Text
type Article    =        (H1,T.Text)

showBody :: Bool
showBody = True

readfiles :: [String] -> IO FileChunks
readfiles filenames = do
  chunks <- concat <$> mapM readfile filenames
  return $ Map.fromList chunks
  where
    readfile fn = do
      content <- TIO.readFile fn
      putStrLn $ fn ++ ": read " ++ show (T.length content) ++ " bytes"
      return [ ( fn
               , Map.fromList $ filechunks fn (preprocess content) )
             ]


-- | deal with this nastiness -- a footnote to an article title, which should really be a term under it
-- @
--  ARTICLE 16
--
--  ^16^ For greater certainty, this Article does not preclude the equitable, non-discriminatory and good faith application of a Party's laws relating to its social security, public retirement or compulsory savings programmes.
--
--  ## Subrogation
--
--  1.  If a Party or a designated agency of a Party makes a payment to an investor of the Party under a guarantee, a contract of insurance or other form of indemnity it has granted in respect of a covered in
-- @
preprocess :: T.Text -> T.Text
preprocess t =
  let asLines = T.lines t
      newLines = filter (not . ("^" `T.isPrefixOf`)) asLines
  in T.unlines newLines

-- | we could use a complete markdown parser here, but let's keep it simple until we need one.
filechunks :: Filename -> T.Text -> [(H1, T.Text)]
filechunks fn content =
  case runParser ((,) <$> preamble fn <*> some (pChunk fn) <* eof) fn content of
    Left  x      -> error $ errorBundlePretty x
    Right (p ,xs) -> ((Nothing, "__PREAMBLE"), p) : xs

preamble :: Filename -> Parser T.Text
preamble fn = T.pack <$> manyTill anyChar (lookAhead $ pChunk fn)

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
      let articleN = "ARTICLE " *> some dotDigitChar <* some eol <* optional (some "#" <* some " ")
          articleT = manyTill anyChar eol <* many "=" <* many eol -- title
      (,) <$> articleN <*> articleT

      | "ANZSCEP" `isSubsequenceOf` fn = do
      let h1starL = some "*" *> "Article " *> some dotDigitChar <* ": "
          h1starR = many "*" *> manyTill anyChar (some "*" >> eol)
          h1equalsL = many "*" *> "Article " *> some dotDigitChar <* ": "
          h1equalsR = many "*" *> manyTill anyChar ("*" <|> eol) <* many "*" <* manyTill (TMC.char '=') eol
      choice [ try $ (,) <$> h1starL   <*> h1starR
             , try $ (,) <$> h1equalsL <*> h1equalsR ]

      | "CSFTA" `isSubsequenceOf` fn = do
          let articleN = "**[Article " *> some dotDigitChar <* "]{.smallcaps}**" <* some eol
              pH1 = some "*" *> manyTill anyChar "**" <* many "*" <* many eol
          (,) <$> articleN <*> pH1

      | otherwise = error ("not prepared to process file " ++ fn)

anyChar :: Parser Char
anyChar = satisfy (const True)

dotDigitChar :: Parser Char
dotDigitChar = digitChar <|> TMC.char '.'

type Parser = Parsec Void T.Text

normalize :: [T.Text] -> [T.Text]
normalize ts = ts

-- | compute approximate "hash" by calculating the word distribution signature of a given chunk

stats :: FileChunks -> IO ()
stats fchunks = do
  sequence_ [ drawMatrix doc1 doc2
            | doc1@(fn1,fb1) <-                                      Map.toList fchunks
            , doc2@(fn2,fb2) <- drop 1 $ dropWhile ((fn1 /=) . fst) (Map.toList fchunks)
            -- the above is so we don't dup pairs, we just compare one half of the triangle
            ]
  putStrLn $ unlines
    [ "* " ++ filename ++ "\n" ++ unlines
      [ "** " ++ maybe "" T.unpack n ++ " " ++ T.unpack (elideSuperscripts h1title) ++ "\n" ++
        ":length: " ++ show (T.length body) ++ "\n" ++
        (if showBody
          then "*** body" ++ "\n" ++ T.unpack body ++ "\n"
          else mempty)
        ++ unlines [ "*** " ++ sn fn ++ ": most similar " ++ show cfocus ++ " = " ++
                     shortname (head $ snd <$> sort (bySimilarOneDoc cfocus fc (fn, farticles)))
                   | (fn,farticles) <- Map.toList $ fchunks `sans` filename
                   , cfocus <- [minBound .. maxBound :: ChunkFocus]
                   ]
      | fc@((n,h1title), body) <- sortByArtNum (Map.toList filebody)
      ]
    | (filename, filebody) <- Map.toList fchunks
    ]
  where sans = flip Map.delete
        elideSuperscripts = T.takeWhile (/= '^')

sortByArtNum :: [((Maybe T.Text, b1), b2)] -> [((Maybe T.Text, b1), b2)]
sortByArtNum = sortOn fstfst
  where
    fstfst ((mt,_),_) = fmap ((read :: (String -> Int)) . T.unpack) . T.splitOn "." <$> mt
        
shortname :: (Filename, H1) -> String
shortname (fn, (artnum,h1text)) = maybe "?" T.unpack artnum ++ "(" ++ T.unpack h1text ++ ")"

sn :: Filename -> String
sn fn
  | "SAFTA"   `isSubsequenceOf` fn = "SAFTA"
  | "ANZSCEP" `isSubsequenceOf` fn = "ANZSCEP"
  | "CSFTA"   `isSubsequenceOf` fn = "CSFTA"
  | otherwise                      = fn
  -- [TODO] do this based on /xxx/

data ChunkFocus = Title | Body
  deriving (Eq, Show, Enum, Bounded)

bySimilarAllDocs :: ChunkFocus -> Article -> FileChunks -> [(Filename, H1)]
bySimilarAllDocs cfocus mychunk@((myh1num,myh1title),mybody) fchunks =
  snd <$> sort (concat
  [ bySimilarOneDoc cfocus mychunk (fn, farticles)
  | (fn, farticles) <- Map.toList fchunks
  ])

bySimilarOneDoc :: ChunkFocus -> Article -> (Filename, Articles) -> [(Int,(Filename, H1))]
bySimilarOneDoc cfocus mychunk@((myh1num,myh1title),mybody) (fn, farticles) =
  [ (metric, (fn, h1))
  | chunk@(h1@(artnum, arttitle), artbody)   <- Map.toList farticles
  , let metric :: Int
        metric = bySimilar cfocus mychunk (fn,chunk)
  ]

bySimilar :: ChunkFocus -> Article -> (Filename, Article) -> Int
bySimilar cfocus mychunk@((myh1num,myh1title),mybody) (fn, ((artnum, arttitle), artbody)) =
  case cfocus of
    Title -> algo myh1title arttitle
    Body  -> algo mybody artbody
  where algo x y = levenshtein (T.toLower x) (T.toLower y)
  

drawMatrix :: (Filename, Map.Map H1 T.Text) -> (Filename, Map.Map  H1 T.Text) -> IO ()
drawMatrix doc1@(fn1,fb1) doc2@(fn2,fb2) =
  forM_ [minBound .. maxBound :: ChunkFocus] $ \chunkfocus -> do
    putStrLn $ unwords [ "* matrix of similarity"
                       , sn fn1 ++ ends fb1 , "/" ,        sn fn2 ++ ends fb2
                       , "for", show chunkfocus ]
    printBox (hsep 1 left $
              vcat left (text . T.unpack <$> ("" : headers fb2)) :
              [ vcat right (text (T.unpack artnum1) :
                            [ text $ show $ bySimilar chunkfocus myart (fn2,art2)
                            | art2@((Just artnum2, arttitle2), artbody2) <- sortByArtNum $ Map.toList fb2
                            ])
              | myart@((Just artnum1, arttitle1), artbody1) <- sortByArtNum $ Map.toList fb1
              ]
             )

  where
    headers fb = [ artnum | ((Just artnum, arttitle), artbody) <- sortByArtNum $ Map.toList fb ]
    ends fb = "(" ++ (T.unpack . head) (headers fb) ++ "--" ++ (T.unpack . last) (headers fb) ++ ")"
