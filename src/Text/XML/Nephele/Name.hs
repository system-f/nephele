{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.Name(
  NameFirst
, nameFirst
, Name
, name
, names
, names1
) where

import Text.Parser.Char(CharParsing(..))
import Control.Applicative(Applicative(..), Alternative(..), (<$>), (<*>), (<$))
import Data.List.NonEmpty(NonEmpty(..), toList)
import Prelude(Char, Eq(..), Show(..), Ord(..), Either(..), either, (&&), (||), (.), ($), Bool, String, error)
import Text.XML.Nephele.NameCharacter(NameCharacter, nameCharacters)
import Text.XML.Nephele.Letter(Letter, letter)

-- $setup
-- >>> import Text.Parsec(parse)
-- >>> import Data.Text
-- >>> import Control.Lens

data NameFirst =
  LetterNameFirst Letter
  | UnderscoreNameFirst
  | ColonNameFirst
  deriving (Eq, Show)

-- | Parse the first character of a name.
--
-- >>> parse nameFirst "test" "abc"
-- Right (LetterNameFirst (BaseCharacterLetter (BaseCharacter 'a')))
nameFirst ::
  CharParsing m =>
  m NameFirst
nameFirst =
  (LetterNameFirst <$> letter)
  <|> UnderscoreNameFirst <$ char '_'
  <|> ColonNameFirst <$ char ':'

data Name =
  Name NameFirst [NameCharacter]
  deriving (Eq, Show)

-- | Parse a name.
--
-- @(Letter | '_' | ':') (NameChar)*@.
--
-- >>> parse name "test" "abc"
-- Right (Name (LetterNameFirst (BaseCharacterLetter (BaseCharacter 'a'))) [LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'b')),LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'c'))])
--
-- >>> parse name "test" "\20016bc"
-- Right (Name (LetterNameFirst (IdeographicLetter (Ideographic '\20016'))) [LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'b')),LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'c'))])
name ::
  CharParsing m =>
  m Name
name =
  Name <$> nameFirst <*> nameCharacters

-- | Parse zero or many names.
names ::
  CharParsing m =>
  m [Name]
names =
  many name

-- | Parse one or many names.
names1 ::
  CharParsing m =>
  m (NonEmpty Name)
names1 =
  some1 name

-- todo move to utility
some1 ::
  Alternative f =>
  f a
  -> f (NonEmpty a)
some1 x =
  let m = toList <$> s <|> pure []
      s = (:|) <$> x <*> m
  in s