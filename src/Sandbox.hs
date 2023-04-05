{-# LANGUAGE OverloadedStrings #-}

module Sandbox where

import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad (void)
import Data.Text (Text, pack)
import Data.Void ( Void )
import Text.Megaparsec ( (<?>), some, Parsec, parseTest, eof, satisfy, mkPos )
import Text.Megaparsec.Char ( alphaNumChar, char, space1 )
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

anyChar :: Parser Char
anyChar = satisfy (`notElem` ['\n', '#'])

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pItem :: Parser String
pItem = lexeme (some anyChar) <?> "list item"

pComplexItem :: Parser (String, [String])
pComplexItem = L.indentBlock scn p
  where
    p = do
      header <- pItem
      return (L.IndentMany Nothing (return . (header, )) pItem)

pItemList :: Parser (String, [(String, [String])])
pItemList = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- pItem
      return (L.IndentMany Nothing (return . (header, )) pComplexItem)

-----------------------------------------------------------------------------
-- stuff to try out in the command line

test = parseTest (some pItemList <* eof) testList

testList :: Text
testList = pack $ unlines [
    "For the purposes of this Parsing Exercise:",
    "(a) \"sdfdfgsfgs\" means the *sdgdfgdhf*, set out in Annex 999 to the ASDFKJDFSF Agreement;",
    "(b) \"dfgkldfhkdlfh\" means, with respect to a sdfksgljgfsd:",
    "  i.  lkhjhdku; or",
    "  ii. uiykhchniwgredfbcvmjh dfhljsgh:",
    "    A.  sdlj dsfgj asjhf kjjgk gasdfjh hdflhjasgh; or",
    "    B.  lashg ioiehl aslfajföä sdflkgsh"]
