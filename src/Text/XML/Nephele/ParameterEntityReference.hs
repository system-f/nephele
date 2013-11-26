{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.ParameterEntityReference(
  ParameterEntityReference
, parameterEntityReference
, parameterEntityReferences
, parameterEntityReferences1
) where

import Text.Parser.Char(CharParsing(..))
import Control.Applicative(Applicative(..), Alternative(..), (<$>), (<*>))
import Data.List.NonEmpty(NonEmpty(..))
import Prelude(Char, Eq(..), Show(..), Ord(..), Either(..), either, (&&), (||), (.), ($), Bool, String, error)
import Text.XML.Nephele.Name(Name, name)

-- $setup
-- >>> import Text.Parsec(parse)
-- >>> import Data.Text
-- >>> import Control.Lens

newtype ParameterEntityReference =
  ParameterEntityReference Name
  deriving (Eq, Show)

-- | Parse a parameter entity reference.
--
-- @'%' Name ';'@.
--
-- >>> parse parameterEntityReference "test" "%abc;"
-- Right (ParameterEntityReference (Name (LetterNameFirst (BaseCharacterLetter (BaseCharacter 'a'))) [LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'b')),LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'c'))]))
parameterEntityReference ::
  CharParsing m =>
  m ParameterEntityReference
parameterEntityReference =
  ParameterEntityReference <$> (char '%' *> name <* char ';')

-- | Parse zero or many names.
parameterEntityReferences ::
  CharParsing m =>
  m [ParameterEntityReference]
parameterEntityReferences =
  many parameterEntityReference

-- | Parse one or many names.
parameterEntityReferences1 ::
  CharParsing m =>
  m (NonEmpty ParameterEntityReference)
parameterEntityReferences1 =
  some1 parameterEntityReference

-- todo move to utility
some1 ::
  Alternative f =>
  f a
  -> f (NonEmpty a)
some1 x =
  (:|) <$> x <*> many x
