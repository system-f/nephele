{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.Letter {-(
  Letter
, letter
, letters
, letters1
, letter'
, letters'
) -} where

import Text.Parser.Char(CharParsing(..), satisfyRange)
import Text.Parsec(parse)
import Text.Parsec.Text()
import Data.Text(Text, singleton)
import Control.Applicative(Applicative(..), Alternative(..), (<$>))
import Data.Foldable(asum)
import Data.List.NonEmpty(NonEmpty(..), toList)
import Control.Lens(Prism', prism', (^?), _Right)
import Prelude(Char, Eq(..), Show(..), Ord(..), (&&), (||), (.), ($), Bool, String, error)
import Text.XML.Nephele.BaseCharacter(BaseCharacter, baseCharacter)
import Text.XML.Nephele.Ideographic(Ideographic, ideographic)

data Letter =
  BaseCharacterLetter BaseCharacter
  | IdeographicLetter Ideographic
  deriving (Eq, Ord, Show)

-- | Parse a letter.
--
-- @BaseChar |  Ideographic@.
--
-- >>> parse letter "test" "abc"
-- Right (Letter 'a')
letter ::
  CharParsing m =>
  m Letter
letter =
  (BaseCharacterLetter <$> baseCharacter) <|> (IdeographicLetter <$> ideographic)

-- | Parse zero or many letters.
letters ::
  CharParsing m =>
  m [Letter]
letters =
  many letter

-- | Parse one or many letters.
letters1 ::
  CharParsing m =>
  m (NonEmpty Letter)
letters1 =
  some1 letter

-- Prism c a -> Prism c b -> Prism c (Either a b)

-- | Letter prism from a char.
--
-- >>> 'a' ^? letter'
-- Just (Letter 'a')
{-
letter' ::
  Prism' Char Letter
letter' =
  prism'
    (\(Letter c) -> c)
    ((^? _Right) . parse letter "letter'" . singleton)

-- | Letter prism from text.
--
-- >>> pack "abc" ^? letters'
-- Just (Letter 'a')
letters' ::
  Prism' Text Letter
letters' =
  prism'
    (\(Letter c) -> singleton c)
    ((^? _Right) . parse letter "letter'")
  -}
-- todo move to utility
some1 ::
  Alternative f =>
  f a
  -> f (NonEmpty a)
some1 x =
  let m = toList <$> s <|> pure []
      s = (:|) <$> x <*> m
  in s