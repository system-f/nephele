{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.Digit(
  Digit
, digit
, digits
, digits1
, digit'
, digits'
) where

import Data.Text(Text, singleton)
import Papa
import Text.Parser.Char(CharParsing, satisfyRange)
import Text.Parsec(parse)

newtype Digit =
  Digit Char
  deriving (Eq, Ord, Show)

-- | Parse a digit.
--
-- @[#x0030-#x0039] | [#x0660-#x0669] | [#x06F0-#x06F9] | [#x0966-#x096F] | [#x09E6-#x09EF] | [#x0A66-#x0A6F] | [#x0AE6-#x0AEF] | [#x0B66-#x0B6F] | [#x0BE7-#x0BEF] | [#x0C66-#x0C6F] | [#x0CE6-#x0CEF] | [#x0D66-#x0D6F] | [#x0E50-#x0E59] | [#x0ED0-#x0ED9] | [#x0F20-#x0F29]@.
digit ::
  CharParsing m =>
  m Digit
digit =
  let c = asum [
            satisfyRange '\x0030' '\x0039'
          , satisfyRange '\x0660' '\x0669'
          , satisfyRange '\x06F0' '\x06F9'
          , satisfyRange '\x0966' '\x096F'
          , satisfyRange '\x09E6' '\x09EF'
          , satisfyRange '\x0A66' '\x0A6F'
          , satisfyRange '\x0AE6' '\x0AEF'
          , satisfyRange '\x0B66' '\x0B6F'
          , satisfyRange '\x0BE7' '\x0BEF'
          , satisfyRange '\x0C66' '\x0C6F'
          , satisfyRange '\x0CE6' '\x0CEF'
          , satisfyRange '\x0D66' '\x0D6F'
          , satisfyRange '\x0E50' '\x0E59'
          , satisfyRange '\x0ED0' '\x0ED9'
          , satisfyRange '\x0F20' '\x0F29'
          ]
  in Digit <$> c

-- | Parse zero or many digits.
digits ::
  CharParsing m =>
  m [Digit]
digits =
  many digit

-- | Parse one or many digits.
digits1 ::
  CharParsing m =>
  m (NonEmpty Digit)
digits1 =
  some1 digit

-- | Digit prism from a char.
digit' ::
  Prism' Char Digit
digit' =
  prism'
    (\(Digit c) -> c)
    ((^? _Right) . parse digit "digit'" . singleton)

-- | Digit prism from text.
digits' ::
  Prism' Text Digit
digits' =
  prism'
    (\(Digit c) -> singleton c)
    ((^? _Right) . parse digit "digit'")
