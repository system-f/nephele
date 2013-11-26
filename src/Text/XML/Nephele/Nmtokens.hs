{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.Nmtokens(
  Nmtokens
, nmtokens
) where

import Text.Parser.Char(CharParsing(..))
import Text.Parsec.Text()
import Control.Applicative(Applicative(..), Alternative(..), (<$>))
import Data.List.NonEmpty(NonEmpty(..))
import Prelude(Char, Eq(..), Show(..), Ord(..), (&&), (||), (.), ($), Bool, String, flip, id, error, undefined)
import Text.XML.Nephele.NameCharacter(NameCharacter, nameCharacters1)
import Text.XML.Nephele.Whitespace(Whitespace, whitespaces1)

-- $setup
-- >>> import Text.Parsec(parse)
-- >>> import Data.Text
-- >>> import Control.Lens

data Nmtokens =
  Nmtokens (SepWith1 (NonEmpty Whitespace) (NonEmpty NameCharacter))
  deriving (Eq, Show)

-- | Parse nmtokens.
--
-- @((NameChar)+) (S ((NameChar)+))*@.
--
-- >>> parse nmtokens "test" "abc  def \tgh"
-- Right (Nmtokens (SepWith1 (LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'a')) :| [LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'b')),LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'c'))]) [(Space :| [Space],LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'd')) :| [LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'e')),LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'f'))]),(Space :| [Tab],LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'g')) :| [LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'h'))])]))
nmtokens ::
  CharParsing m =>
  m Nmtokens
nmtokens =
  Nmtokens <$> sepWith1 nameCharacters1 whitespaces1

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
