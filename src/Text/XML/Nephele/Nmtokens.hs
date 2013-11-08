{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.Nmtokens(
  Nmtokens
, nmtokens
) where

import Text.Parser.Char(CharParsing(..), char)
import Text.Parsec(parse)
import Text.Parsec.Text()
import Data.Text(Text, singleton)
import Control.Applicative(Applicative(..), Alternative(..), (<$>), (<$))
import Data.List.NonEmpty(NonEmpty(..), toList)
import Prelude(Char, Eq(..), Show(..), Ord(..), (&&), (||), (.), ($), Bool, String, error)
import Text.XML.Nephele.NameCharacter(NameCharacter)
import Text.XML.Nephele.Whitespace(Whitespace)

-- $setup
-- >>> import Data.Text
-- >>> import Control.Lens

data Nmtokens =
  Nmtokens (NonEmpty (NonEmpty NameCharacter)) [NonEmpty Whitespace]

-- | Parse nmtokens.
--
-- @((NameChar)+) (S ((NameChar)+))*@.
--
-- >>> parse nmtokens "test" " "
-- Right Space
--
-- >>> parse nmtokens "test" "\t "
-- Right Tab
--
-- >>> parse nmtokens "test" "\n\t "
-- Right LineFeed
--
-- >>> parse nmtokens "test" "\r\n\t "
-- Right CarriageReturn
nmtokens ::
  CharParsing m =>
  m Nmtokens
nmtokens =
  nmtokens {-
  Space <$ char '\x20'
  <|> Tab <$ char '\x9'
  <|> CarriageReturn <$ char '\xD'
  <|> LineFeed <$ char '\xA'
  -}
