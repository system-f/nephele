{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele where

import Text.Parser.Char
import Text.Parser.Combinators
import qualified Text.Parser.Char as C
import Data.Text(Text, pack, concat)
import Prelude(Char, Eq(..), Show(..), Ord(..), (&&), String)
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

newtype Comment =
  Comment Text
  deriving (Eq, Show)

-- | The parser for a comment.
--
-- >>> parse comment "test" "<!---->"
-- Right (Comment "")
--
-- >>> parse comment "test" "<!-- abc -->"
-- Right (Comment " abc ")
--
-- >>> parse comment "test" "<!-- a-bc -->"
-- Right (Comment " a-bc ")
--
-- >>> parse comment "test" "<!-- a-b-c -->"
-- Right (Comment " a-b-c ")
--
-- >>> parse comment "test" "<!-- a-bb-cc -->"
-- Right (Comment " a-bb-cc ")
--
-- >>> parse comment "test" "<!-- abc- -->"
-- Right (Comment " abc- ")
comment ::
  CharParsing m =>
  m Comment
comment =
  -- character without '-'
  let nominus = oneOf ['\x9', '\xA', '\xD']
           <|> satisfyRange '\x20' '\x2C'
           <|> satisfyRange '\x2E' '\xD7FF'
           <|> satisfyRange '\xE000' '\xFFFD'
           <|> satisfyRange '\x10000' '\x10FFFF'
      s = (\m c -> [m, c]) <$> C.char '-' <*> nominus
      t = (:[]) <$> nominus
      ch = concat <$> many (pack <$> (try s <|> t))
  in Comment <$> between commentBegin commentEnd ch

-- | Parse a character.
--
-- @#x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]@.
--
-- >>> parse character "test" "abc"
-- Right 'a'
character ::
  CharParsing m =>
  m Char
character =
  oneOf ['\x9', '\xA', '\xD']
  <|> satisfyRange '\x20' '\xD7FF'
  <|> satisfyRange '\xE000' '\xFFFD'
  <|> satisfyRange '\x10000' '\x10FFFF'

-- | Parse zero or many characters.
characters ::
  CharParsing m =>
  m Text
characters =
  pack <$> many character

-- | Parse one or many characters.
characters1 ::
  CharParsing m =>
  m Text
characters1 =
  pack <$> some character

-- | Parse a white space character
--
-- @(#x20 | #x9 | #xD | #xA)+@.
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
