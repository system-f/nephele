{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.Name(
  NameFirst
, nameFirst
, Name
, name
) where

import Papa
import Text.Parser.Char(CharParsing(char))
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
