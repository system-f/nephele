{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.Whitespace(
  Whitespace(..)
, whitespace
, Whitespaces1(..)
, whitespaces1
, whitespace'
, whitespaces'
) where

import Data.Text(Text, singleton)
import Papa hiding (Space)
import Text.Parser.Char(CharParsing(char), char)
import Text.Parsec(parse)

data Whitespace =
  Space
  | Tab
  | CarriageReturn
  | LineFeed
  deriving (Eq, Show)

-- | Parse a white space character.
--
-- @(#x20', char '\x9', char '\xD', char '\xA)@.
whitespace ::
  CharParsing m =>
  m Whitespace
whitespace =
  Space <$ char '\x20'
  <|> Tab <$ char '\x9'
  <|> CarriageReturn <$ char '\xD'
  <|> LineFeed <$ char '\xA'


newtype Whitespaces1 =
  Whitespaces1 (NonEmpty Whitespace)
  deriving (Eq, Show)

-- | Parse one or many white space characters.
whitespaces1 ::
  CharParsing m =>
  m Whitespaces1
whitespaces1 =
  Whitespaces1 <$> some1 whitespace

-- | Whitespace prism from a char.
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
