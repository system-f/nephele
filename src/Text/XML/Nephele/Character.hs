{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.Character(
  Character
, character
, characters
, characters1
, character'
, characters'
, ConstrainedCharacter
, unconstrain
, constrainCharacter
, oneofCharacter
, noneofCharacter
) where

import Data.Text(Text, singleton)
import Papa
import Text.Parser.Char(CharParsing, oneOf, satisfyRange)
import Text.Parsec(parse)

-- $setup
-- >>> import Data.Text

newtype Character =
  Character Char
  deriving (Eq, Ord, Show)

-- | Parse a character.
--
-- @#x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]@.
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

data ConstrainedCharacter =
  UnconstrainedCharacter Char
  | ConstrainedCharacter Character
  deriving (Eq, Show)

unconstrain ::
  ConstrainedCharacter
  -> Character
unconstrain (UnconstrainedCharacter c) =
  Character c
unconstrain (ConstrainedCharacter c) =
  c

constrainCharacter ::
  CharParsing m =>
  (Char -> Bool)
  -> m ConstrainedCharacter
constrainCharacter p =
  (\c'@(Character c) -> case p c of
                          True -> UnconstrainedCharacter c
                          False -> ConstrainedCharacter c') <$> character

oneofCharacter ::
  (CharParsing m, Foldable f) =>
  f Char
  -> m ConstrainedCharacter
oneofCharacter s =
  constrainCharacter (`elem` s)

noneofCharacter ::
  (CharParsing m, Foldable f) =>
  f Char
  -> m ConstrainedCharacter
noneofCharacter s =
  constrainCharacter (`notElem` s)

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
