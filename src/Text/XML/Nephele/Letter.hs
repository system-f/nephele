{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.Letter(
  Letter
, letter
, letters
, letters1
, letter'
) where

import Text.Parser.Char(CharParsing(..))
import Control.Applicative(Applicative(..), Alternative(..), (<$>))
import Data.List.NonEmpty(NonEmpty(..), toList)
import Control.Lens(Iso', iso)
import Prelude(Char, Eq(..), Show(..), Ord(..), Either(..), either, (&&), (||), (.), ($), Bool, String, error)
import Text.XML.Nephele.BaseCharacter(BaseCharacter, baseCharacter)
import Text.XML.Nephele.Ideographic(Ideographic, ideographic)

-- $setup
-- >>> import Text.Parsec(parse)
-- >>> import Data.Text
-- >>> import Control.Lens

data Letter =
  BaseCharacterLetter BaseCharacter
  | IdeographicLetter Ideographic
  deriving (Eq, Ord, Show)

-- | Parse a letter.
--
-- @BaseChar |  Ideographic@.
--
-- >>> parse letter "test" "abc"
-- Right (BaseCharacterLetter (BaseCharacter 'a'))
--
-- >>> parse letter "test" "\20016bc"
-- Right (IdeographicLetter (Ideographic '\20016'))
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

-- | Letter isomorphism from either a @BaseCharacter@ or @Ideographic@.
letter' ::
  Iso' Letter (Either BaseCharacter Ideographic)
letter' =
  iso (\l -> case l of
               BaseCharacterLetter c -> Left c
               IdeographicLetter c -> Right c)
      (either BaseCharacterLetter IdeographicLetter)

-- todo move to utility
some1 ::
  Alternative f =>
  f a
  -> f (NonEmpty a)
some1 x =
  let m = toList <$> s <|> pure []
      s = (:|) <$> x <*> m
  in s