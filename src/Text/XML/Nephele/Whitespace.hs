{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.Whitespace(
  Whitespace
, whitespace
, Whitespaces1
, whitespaces1
, whitespace'
, whitespaces'
) where

import Data.Text(Text, singleton)
import Papa hiding (Space)
import Text.Parser.Char(CharParsing(char), char)
import Text.Parsec(parse)

-- $setup
-- >>> import Data.Text
-- >>> import Control.Lens

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


newtype Whitespaces1 =
  Whitespaces1 (NonEmpty Whitespace)
  deriving (Eq, Show)

-- | Parse one or many white space characters.
--
-- >>> isn't _Right (parse whitespaces1 "test" "")
-- True
--
-- >>> parse whitespaces1 "test" " "
-- Right (Whitespaces1 (Space :| []))
--
-- >>> parse whitespaces1 "test" "    "
-- Right (Whitespaces1 (Space :| [Space,Space,Space]))
--
-- >>> parse whitespaces1 "test" "    abc"
-- Right (Whitespaces1 (Space :| [Space,Space,Space]))
--
-- >>> parse whitespaces1 "test" "  \t  \n "
-- Right (Whitespaces1 (Space :| [Space,Tab,Space,Space,LineFeed,Space]))
whitespaces1 ::
  CharParsing m =>
  m Whitespaces1
whitespaces1 =
  Whitespaces1 <$> some1 whitespace

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
