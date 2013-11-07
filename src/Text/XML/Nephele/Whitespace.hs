{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.Whitespace(
  Whitespace
, whitespace
, whitespaces
, whitespaces1
, whitespace'
, whitespaces'
) where

import Text.Parser.Char(CharParsing(..), char)
import Text.Parsec(parse)
import Text.Parsec.Text()
import Data.Text(Text, singleton)
import Control.Applicative(Applicative(..), Alternative(..), (<$>), (<$))
import Data.List.NonEmpty(NonEmpty(..), toList)
import Control.Lens(Prism', prism', (^?), _Right)
import Prelude(Char, Eq(..), Show(..), Ord(..), (&&), (||), (.), ($), Bool, String, error)

data Whitespace =
  Space
  | Tab
  | CarriageReturn
  | LineFeed
  deriving (Eq, Show)

-- | Parse a white space character.
--
-- @(#x20', char '\x9', char '\xD', char '\xA)@.
--
-- >>> parse whitespace "test" " "
-- Right Space
--
-- >>> parse whitespace "test" "\t "
-- Right Tab
--
-- >>> parse whitespace "test" "\n\t "
-- Right LineFeed
--
-- >>> parse whitespace "test" "\r\n\t "
-- Right CarriageReturn
whitespace ::
  CharParsing m =>
  m Whitespace
whitespace =
  Space <$ char '\x20'
  <|> Tab <$ char '\x9'
  <|> CarriageReturn <$ char '\xD'
  <|> LineFeed <$ char '\xA'

-- | Parse zero or many white space characters.
--
-- >>> parse whitespaces "test" ""
-- Right []
--
-- >>> parse whitespaces "test" " "
-- Right [Space]
--
-- >>> parse whitespaces "test" "    "
-- Right [Space,Space,Space,Space]
--
-- >>> parse whitespaces "test" "    abc"
-- Right [Space,Space,Space,Space]
--
-- >>> parse whitespaces "test" "  \t  \n "
-- Right [Space,Space,Tab,Space,Space,LineFeed,Space]
whitespaces ::
  CharParsing m =>
  m [Whitespace]
whitespaces =
  many whitespace

-- | Parse one or many white space characters.
--
-- >>> isn't _Right (parse whitespaces1 "test" "")
-- True
--
-- >>> parse whitespaces1 "test" " "
-- Right (Space :| [])
--
-- >>> parse whitespaces1 "test" "    "
-- Right (Space :| [Space,Space,Space])
--
-- >>> parse whitespaces1 "test" "    abc"
-- Right (Space :| [Space,Space,Space])
--
-- >>> parse whitespaces1 "test" "  \t  \n "
-- Right (Space :| [Space,Tab,Space,Space,LineFeed,Space])
whitespaces1 ::
  CharParsing m =>
  m (NonEmpty Whitespace)
whitespaces1 =
  some1 whitespace

-- | Whitespace prism from a char.
--
-- >>> ' ' ^? whitespace'
-- Just Space
--
-- >>> '\t' ^? whitespace'
-- Just Tab
--
-- >>> '\r' ^? whitespace'
-- Just CarriageReturn
--
-- >>> '\n' ^? whitespace'
-- Just LineFeed
whitespace' ::
  Prism' Char Whitespace
whitespace' =
  prism'
    (\w -> case w of
             Space -> '\x20'
             Tab -> '\x9'
             CarriageReturn -> '\xD'
             LineFeed -> '\xA')
    ((^? _Right) . parse whitespace "whitespace'" . singleton)

-- | Whitespace prism from text.
--
-- >>> pack " " ^? whitespaces'
-- Just Space
--
-- >>> pack "\t" ^? whitespaces'
-- Just Tab
--
-- >>> pack "\r" ^? whitespaces'
-- Just CarriageReturn
--
-- >>> pack "\n" ^? whitespaces'
-- Just LineFeed
whitespaces' ::
  Prism' Text Whitespace
whitespaces' =
  prism'
    (\w -> singleton $ case w of
                         Space -> '\x20'
                         Tab -> '\x9'
                         CarriageReturn -> '\xD'
                         LineFeed -> '\xA')
    ((^? _Right) . parse whitespace "character'")

-- todo move to utility
some1 ::
  Alternative f =>
  f a
  -> f (NonEmpty a)
some1 x =
  let m = toList <$> s <|> pure []
      s = (:|) <$> x <*> m
  in s
