{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.Nmtokens(
  Nmtokens
, nmtokens
) where

import Papa
import Text.Parser.Char(CharParsing)
import Text.XML.Nephele.Whitespace(Whitespaces1, whitespaces1)
import Text.XML.Nephele.Nmtoken(Nmtoken, nmtoken)

-- $setup
-- >>> import Text.Parsec(parse)
-- >>> import Data.Text
-- >>> import Control.Lens

data Nmtokens =
  Nmtokens (SepWith1 Whitespaces1 Nmtoken)
  deriving (Eq, Show)

-- | Parse nmtokens.
--
-- @((NameChar)+) (S ((NameChar)+))*@.
--
-- >>> parse nmtokens "test" "abc  def \tgh"
-- Right (Nmtokens (SepWith1 (Nmtoken (LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'a')) :| [LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'b')),LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'c'))])) [(Whitespaces1 (Space :| [Space]),Nmtoken (LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'd')) :| [LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'e')),LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'f'))])),(Whitespaces1 (Space :| [Tab]),Nmtoken (LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'g')) :| [LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'h'))]))]))
nmtokens ::
  CharParsing m =>
  m Nmtokens
nmtokens =
  Nmtokens <$> sepWith1 nmtoken whitespaces1

-- todo move to utility

{-
data SepWith sep a =
  EmptySepWith
  | SepWith (SepWith1 sep a)
  deriving (Eq, Show)

sepWith ::
  Alternative f =>
  f a
  -> f sep
  -> f (SepWith sep a)
sepWith p sep =
  SepWith <$> sepWith1 p sep <|> pure EmptySepWith
-}

data SepWith1 sep a =
  SepWith1 a [(sep, a)]
  deriving (Eq, Show)

sepWith1 ::
  Alternative f =>
  f a
  -> f sep
  -> f (SepWith1 sep a)
sepWith1 p sep =
  SepWith1 <$> p <*> many ((,) <$> sep <*> p)
