{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.NameCharacter(
  NameCharacter
, nameCharacter
, nameCharacters
, nameCharacters1
) where

import Text.Parser.Char(CharParsing(..))
import Control.Applicative(Applicative(..), Alternative(..), (<$>), (<$))
import Data.List.NonEmpty(NonEmpty(..))
import Prelude(Char, Eq(..), Show(..), Ord(..), Either(..), either, (&&), (||), (.), ($), Bool, String, error)
import Text.XML.Nephele.Letter(Letter, letter)
import Text.XML.Nephele.Digit(Digit, digit)
import Text.XML.Nephele.CombiningCharacter(CombiningCharacter, combiningCharacter)
import Text.XML.Nephele.Extender(Extender, extender)

-- $setup
-- >>> import Text.Parsec(parse)
-- >>> import Data.Text
-- >>> import Control.Lens

data NameCharacter =
  LetterNameCharacter Letter
  | DigitNameCharacter Digit
  | DotNameCharacter
  | HyphenNameCharacter
  | ColonNameCharacter
  | CombiningCharacterNameCharacter CombiningCharacter
  | ExtenderNameCharacter Extender
  deriving (Eq, Ord, Show)

-- | Parse a name character.
--
-- @Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar | Extender@.
--
-- >>> parse nameCharacter "test" "abc"
-- Right (LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'a')))
--
-- >>> parse nameCharacter "test" "\20016bc"
-- Right (LetterNameCharacter (IdeographicLetter (Ideographic '\20016')))
nameCharacter ::
  CharParsing m =>
  m NameCharacter
nameCharacter =
  (LetterNameCharacter <$> letter)
  <|> (DigitNameCharacter <$> digit)
  <|> DotNameCharacter <$ char '.'
  <|> HyphenNameCharacter <$ char '-'
  <|> ColonNameCharacter <$ char ':'
  <|> (CombiningCharacterNameCharacter <$> combiningCharacter)
  <|> (ExtenderNameCharacter <$> extender)

-- | Parse zero or many nameCharacters.
nameCharacters ::
  CharParsing m =>
  m [NameCharacter]
nameCharacters =
  many nameCharacter

-- | Parse one or many nameCharacters.
nameCharacters1 ::
  CharParsing m =>
  m (NonEmpty NameCharacter)
nameCharacters1 =
  some1 nameCharacter

-- todo move to utility
some1 ::
  Alternative f =>
  f a
  -> f (NonEmpty a)
some1 x =
  (:|) <$> x <*> some x
