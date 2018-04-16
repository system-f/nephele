{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.Ideographic(
  Ideographic
, ideographic
, ideographics
, ideographics1
, ideographic'
, ideographics'
) where

import Papa
import Data.Text(Text, singleton)
import Text.Parser.Char(CharParsing(char), satisfyRange)
import Text.Parsec(parse)

newtype Ideographic =
  Ideographic Char
  deriving (Eq, Ord, Show)

-- | Parse an ideographic.
--
-- @[#x4E00-#x9FA5] | #x3007 | [#x3021-#x3029]@.
ideographic ::
  CharParsing m =>
  m Ideographic
ideographic =
  let c = asum [
            satisfyRange '\x4E00' '\x9FA5'
          , char '\x3007'
          , satisfyRange '\x3021' '\x3029'
          ]

  in Ideographic <$> c

-- | Parse zero or many ideographics.
ideographics ::
  CharParsing m =>
  m [Ideographic]
ideographics =
  many ideographic

-- | Parse one or many ideographics.
ideographics1 ::
  CharParsing m =>
  m (NonEmpty Ideographic)
ideographics1 =
  some1 ideographic

-- | Ideographic prism from a char.
ideographic' ::
  Prism' Char Ideographic
ideographic' =
  prism'
    (\(Ideographic c) -> c)
    ((^? _Right) . parse ideographic "ideographic'" . singleton)

-- | Ideographic prism from text.
ideographics' ::
  Prism' Text Ideographic
ideographics' =
  prism'
    (\(Ideographic c) -> singleton c)
    ((^? _Right) . parse ideographic "ideographic'")
