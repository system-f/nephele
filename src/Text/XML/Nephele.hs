{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele where

import Text.Parser.Char
import Data.Text(Text, pack)
import Data.Functor.Product
import Prelude(Char, Ord(..), (&&), String)
import Control.Applicative

-- $setup
-- >>> import Text.Parsec
-- >>> import Control.Lens
-- >>> import Test.QuickCheck
-- >>> import Prelude
-- >>> newtype UncommentBegin = UncommentBegin String deriving (Eq, Show)
-- >>> instance Arbitrary UncommentBegin where arbitrary = fmap UncommentBegin (arbitrary `suchThat` (/= "<!--"))
-- >>> newtype UncommentEnd = UncommentEnd String deriving (Eq, Show)
-- >>> instance Arbitrary UncommentEnd where arbitrary = fmap UncommentEnd (arbitrary `suchThat` (/= "-->"))

-- | The parser for the beginning of a comment.
--
-- >>> parse commentBegin "test" "<!--"
-- Right "<!--"
--
-- prop> \(UncommentBegin s) -> isn't _Right (parse commentBegin "test" s)
commentBegin ::
  CharParsing m =>
  m Text
commentBegin =
  text "<!--"

-- | The parser for the end of a comment.
--
-- >>> parse commentEnd "test" "-->"
-- Right "-->"
--
-- prop> \(UncommentEnd s) -> isn't _Right (parse commentEnd "test" s)
commentEnd ::
  CharParsing m =>
  m Text
commentEnd =
  text "-->"

comment ::
  (CharParsing f, CharParsing g) =>
  Product f g Text
comment =
  Pair commentBegin commentEnd

character ::
  CharParsing m =>
  m Char
character =
  oneOf ['\x9', '\xA', '\xD']
  <|> satisfyRange '\x20' '\xD7FF'
  <|> satisfyRange '\xE000' '\xFFFD'
  <|> satisfyRange '\x10000' '\x10FFFF'

-- | Parse a white space character.
--
-- >>> parse whitespace "test" " "
-- Right ' '
--
-- >>> parse whitespace "test" "\t "
-- Right '\t'
--
-- >>> parse whitespace "test" "\n\t "
-- Right '\n'
--
-- >>> parse whitespace "test" "\r\n\t "
-- Right '\r'
whitespace ::
  CharParsing m =>
  m Char
whitespace =
  oneOf ['\x0020', '\x9', '\xD', '\xA']

-- | Parse zero or many white space characters.
--
-- >>> parse whitespaces "test" ""
-- Right ""
--
-- >>> parse whitespaces "test" " "
-- Right " "
--
-- >>> parse whitespaces "test" "    "
-- Right "    "
--
-- >>> parse whitespaces "test" "    abc"
-- Right "    "
--
-- >>> parse whitespaces "test" "  \t  \n "
-- Right "  \t  \n "
whitespaces ::
  CharParsing m =>
  m Text
whitespaces =
  pack <$> many whitespace

-- | Parse one or many white space characters.
--
-- >>> isn't _Right (parse whitespaces1 "test" "")
-- True
--
-- >>> parse whitespaces1 "test" " "
-- Right " "
--
-- >>> parse whitespaces1 "test" "    "
-- Right "    "
--
-- >>> parse whitespaces1 "test" "    abc"
-- Right "    "
--
-- >>> parse whitespaces1 "test" "  \t  \n "
-- Right "  \t  \n "
whitespaces1 ::
  CharParsing m =>
  m Text
whitespaces1 =
  pack <$> some whitespace

-- todo update to latest parsers (>0.10) with this function
-- https://github.com/ekmett/parsers/pull/23
satisfyRange :: CharParsing m => Char -> Char -> m Char
satisfyRange a z = satisfy (\c -> c >= a && c <= z)
