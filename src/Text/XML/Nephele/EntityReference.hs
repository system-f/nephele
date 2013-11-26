{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.EntityReference(
  EntityReference
, entityReference
, entityReferences
, entityReferences1
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

newtype EntityReference =
  EntityReference Name
  deriving (Eq, Show)

-- | Parse an entity reference.
--
-- @'&' Name ';'@.
--
-- >>> parse entityReference "test" "&abc;"
-- Right (EntityReference (Name (LetterNameFirst (BaseCharacterLetter (BaseCharacter 'a'))) [LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'b')),LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'c'))]))
entityReference ::
  CharParsing m =>
  m EntityReference
entityReference =
  EntityReference <$> (char '&' *> name <* char ';')

-- | Parse zero or many names.
entityReferences ::
  CharParsing m =>
  m [EntityReference]
entityReferences =
  many entityReference

-- | Parse one or many names.
entityReferences1 ::
  CharParsing m =>
  m (NonEmpty EntityReference)
entityReferences1 =
  some1 entityReference

-- todo move to utility
some1 ::
  Alternative f =>
  f a
  -> f (NonEmpty a)
some1 x =
  (:|) <$> x <*> many x
