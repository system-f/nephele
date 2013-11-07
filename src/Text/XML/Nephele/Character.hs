{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.Character(
  Character
, character
, characters
, characters1
, character'
, characters'
) where

import Text.Parser.Char(CharParsing(..), char, oneOf, satisfyRange)
import Text.Parser.Combinators(try, between, sepBy)
import Text.Parsec(parse)
import Text.Parsec.Text()
import Data.Text(Text, pack, cons, concat, tail, zip, singleton)
import Control.Applicative(Applicative(..), Alternative(..), (<$>), (<$))
import Data.Foldable(asum, all, any)
import Data.List.NonEmpty(NonEmpty(..), toList)
import Data.Maybe(Maybe(..))
import Control.Lens(Prism', prism', (^?), _Right)
import Prelude(Char, Eq(..), Show(..), Ord(..), (&&), (||), (.), ($), Bool, String, error)

newtype Character =
  Character Char
  deriving (Eq, Ord, Show)

-- | Parse a character.
--
-- @#x9', char '\xA', char '\xD | [#x20' '\xD7FF] | [#xE000' '\xFFFD] | [#x10000' '\x10FFFF]@.
--
-- >>> parse character "test" "abc"
-- Right (Character 'a')
character ::
  CharParsing m =>
  m Character
character =
  let c = oneOf ['\x9', '\xA', '\xD']
          <|> satisfyRange '\x20' '\xD7FF'
          <|> satisfyRange '\xE000' '\xFFFD'
          <|> satisfyRange '\x10000' '\x10FFFF'
  in Character <$> c

-- | Parse zero or many characters.
characters ::
  CharParsing m =>
  m [Character]
characters =
  many character

-- | Parse one or many characters.
characters1 ::
  CharParsing m =>
  m (NonEmpty Character)
characters1 =
  some1 character

-- | Character prism from a char.
--
-- >>> 'a' ^? character'
-- Just (Character 'a')
character' ::
  Prism' Char Character
character' =
  prism'
    (\(Character c) -> c)
    ((^? _Right) . parse character "character'" . singleton)

-- | Character prism from text.
--
-- >>> pack "abc" ^? characters'
-- Just (Character 'a')
characters' ::
  Prism' Text Character
characters' =
  prism'
    (\(Character c) -> singleton c)
    ((^? _Right) . parse character "character'")

-- todo move to utility
some1 ::
  Alternative f =>
  f a
  -> f (NonEmpty a)
some1 x =
  let m = toList <$> s <|> pure []
      s = (:|) <$> x <*> m
  in s