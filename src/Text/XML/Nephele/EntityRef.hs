{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.EntityRef where

import Text.Parser.Char(CharParsing(..))
import Control.Applicative(Applicative(..), Alternative(..), (<$>), (<*>))
import Data.List.NonEmpty(NonEmpty(..))
import Prelude(Char, Eq(..), Show(..), Ord(..), Either(..), either, (&&), (||), (.), ($), Bool, String, error)
import Text.XML.Nephele.Name(Name, name)

-- $setup
-- >>> import Text.Parsec(parse)
-- >>> import Data.Text
-- >>> import Control.Lens

newtype EntityRef =
  EntityRef Name
  deriving (Eq, Show)

-- | Parse an EntityRef.
--
-- @'&' Name ';'@.
--
-- >>> parse entityRef "test" "&abc;"
-- Right (EntityRef (Name (LetterNameFirst (BaseCharacterLetter (BaseCharacter 'a'))) [LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'b')),LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'c'))]))
entityRef ::
  CharParsing m =>
  m EntityRef
entityRef =
  EntityRef <$> (char '&' *> name <* char ';')

-- | Parse zero or many names.
entityRefs ::
  CharParsing m =>
  m [EntityRef]
entityRefs =
  many entityRef

-- | Parse one or many names.
entityRefs1 ::
  CharParsing m =>
  m (NonEmpty EntityRef)
entityRefs1 =
  some1 entityRef

-- todo move to utility
some1 ::
  Alternative f =>
  f a
  -> f (NonEmpty a)
some1 x =
  (:|) <$> x <*> many x
