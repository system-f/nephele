{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.Names(
  Names
, names
) where

import Text.Parser.Char(CharParsing(..))
import Text.Parsec.Text()
import Control.Applicative(Applicative(..), Alternative(..), (<$>))
import Prelude(Char, Eq(..), Show(..), Ord(..), (&&), (||), (.), ($), Bool, String, flip, id, error, undefined)
import Text.XML.Nephele.Whitespace(Whitespaces1, whitespaces1)
import Text.XML.Nephele.Nmtoken(Nmtoken, nmtoken)

-- $setup
-- >>> import Text.Parsec(parse)
-- >>> import Data.Text
-- >>> import Control.Lens

data Names =
  Names (SepWith1 Whitespaces1 Nmtoken)
  deriving (Eq, Show)

-- | Parse names.
--
-- @Name (S Name)*@.
--
-- >>> parse names "test" "abc  def \tgh"
-- Right (Names (SepWith1 (Nmtoken (LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'a')) :| [LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'b')),LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'c'))])) [(Whitespaces1 (Space :| [Space]),Nmtoken (LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'd')) :| [LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'e')),LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'f'))])),(Whitespaces1 (Space :| [Tab]),Nmtoken (LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'g')) :| [LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'h'))]))]))
names ::
  CharParsing m =>
  m Names
names =
  Names <$> sepWith1 nmtoken whitespaces1

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
