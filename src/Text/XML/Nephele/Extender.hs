{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.Extender(
  Extender
, extender
, extenders
, extenders1
, extender'
, extenders'
) where

import Text.Parser.Char(CharParsing(..), satisfyRange)
import Text.Parsec(parse)
import Text.Parsec.Text()
import Data.Text(Text, singleton)
import Control.Applicative(Applicative(..), Alternative(..), (<$>))
import Data.Foldable(asum)
import Data.List.NonEmpty(NonEmpty(..), toList)
import Control.Lens(Prism', prism', (^?), _Right)
import Prelude(Char, Eq(..), Show(..), Ord(..), (&&), (||), (.), ($), Bool, String, error)

-- $setup
-- >>> import Data.Text

newtype Extender =
  Extender Char
  deriving (Eq, Ord, Show)

-- | Parse an extender.
--
-- @#x00B7 | #x02D0 | #x02D1 | #x0387 | #x0640 | #x0E46 | #x0EC6 | #x3005 | [#x3031-#x3035] | [#x309D-#x309E] | [#x30FC-#x30FE]@.
--
-- >>> parse extender "test" "\721bc"
-- Right (Extender '\721')
extender ::
  CharParsing m =>
  m Extender
extender =
  let c = asum [
            char '\x00B7'
          , char '\x02D0'
          , char '\x02D1'
          , char '\x0387'
          , char '\x0640'
          , char '\x0E46'
          , char '\x0EC6'
          , char '\x3005'
          , satisfyRange '\x3031' '\x3035'
          , satisfyRange '\x309D' '\x309E'
          , satisfyRange '\x30FC' '\x30FE'
          ]
  in Extender <$> c

-- | Parse zero or many extenders.
extenders ::
  CharParsing m =>
  m [Extender]
extenders =
  many extender

-- | Parse one or many extenders.
extenders1 ::
  CharParsing m =>
  m (NonEmpty Extender)
extenders1 =
  some1 extender

-- | Extender prism from a char.
--
-- >>> '\721' ^? extender'
-- Just (Extender '\721')
extender' ::
  Prism' Char Extender
extender' =
  prism'
    (\(Extender c) -> c)
    ((^? _Right) . parse extender "extender'" . singleton)

-- | Extender prism from text.
--
-- >>> pack "\721bc" ^? extenders'
-- Just (Extender '\721')
extenders' ::
  Prism' Text Extender
extenders' =
  prism'
    (\(Extender c) -> singleton c)
    ((^? _Right) . parse extender "extender'")

-- todo move to utility
some1 ::
  Alternative f =>
  f a
  -> f (NonEmpty a)
some1 x =
  let m = toList <$> s <|> pure []
      s = (:|) <$> x <*> m
  in s