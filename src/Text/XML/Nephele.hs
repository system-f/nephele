{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele(module N) where

import Text.XML.Nephele.BaseCharacter as N
import Text.XML.Nephele.Character as N
import Text.XML.Nephele.CombiningCharacter as N
import Text.XML.Nephele.Comment as N
import Text.XML.Nephele.Digit as N
import Text.XML.Nephele.Extender as N
import Text.XML.Nephele.Ideographic as N
import Text.XML.Nephele.Letter as N
import Text.XML.Nephele.NameCharacter as N
import Text.XML.Nephele.Whitespace as N

{-
import Text.Parser.Char(CharParsing(..), char, oneOf, satisfyRange)
import Text.Parser.Combinators(try, between, sepBy)
import Text.Parsec(parse)
import Text.Parsec.Text()
import Data.Text(Text, pack, cons, concat, tail, zip, singleton)
import Control.Applicative(Applicative(..), Alternative(..), (<$>), (<$))
import Data.Foldable(asum, all, any)
import Data.List.NonEmpty(NonEmpty(..), toList)
import Data.Maybe(Maybe(..))
import Control.Lens -- (Reversing(..), Prism', prism')
import Prelude(Char, Eq(..), Show(..), Ord(..), (&&), (||), (.), ($), Bool, String, error)


-- | Parse a name character @Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar |  Extender@.
--
-- >>> parse namecharacter "test" "A"
-- Right 'A'
--
-- >>> parse namecharacter "test" "."
-- Right '.'
--
-- >>> parse namecharacter "test" "-"
-- Right '-'
--
-- >>> parse namecharacter "test" "_"
-- Right '_'
--
-- >>> parse namecharacter "test" ":"
-- Right ':'
--
-- >>> parse namecharacter "test" "A"
-- Right 'A'
--
-- >>> parse namecharacter "test" "\231"
-- Right '\231'
namecharacter ::
  CharParsing m =>
  m Char
namecharacter =
  asum [
    letter
  , digit
  , char '.'
  , char '-'
  , char '_'
  , char ':'
  , combiningcharacter
  , extender
  ]

-- | Parse zero or many name characters.
--
-- >>> parse namecharacters "test" ""
-- Right ""
--
-- >>> parse namecharacters "test" "A"
-- Right "A"
--
-- >>> parse namecharacters "test" "."
-- Right "."
--
-- >>> parse namecharacters "test" "-"
-- Right "-"
--
-- >>> parse namecharacters "test" "_"
-- Right "_"
--
-- >>> parse namecharacters "test" ":"
-- Right ":"
--
-- >>> parse namecharacters "test" "A"
-- Right "A"
--
-- >>> parse namecharacters "test" "\231"
-- Right "\231"
namecharacters ::
  CharParsing m =>
  m Text
namecharacters =
  pack <$> many namecharacter

-- | Parse one or many name characters.
--
-- >>> isn't _Right (parse namecharacters1 "test" "")
-- True
--
-- >>> parse namecharacters1 "test" "A"
-- Right "A"
--
-- >>> parse namecharacters1 "test" "."
-- Right "."
--
-- >>> parse namecharacters1 "test" "-"
-- Right "-"
--
-- >>> parse namecharacters1 "test" "_"
-- Right "_"
--
-- >>> parse namecharacters1 "test" ":"
-- Right ":"
--
-- >>> parse namecharacters1 "test" "A"
-- Right "A"
--
-- >>> parse namecharacters1 "test" "\231"
-- Right "\231"
namecharacters1 ::
  CharParsing m =>
  m Text
namecharacters1 =
  pack <$> some namecharacter

-- | Parse a name @(Letter | '_' | ':') (NameChar)*@.
--
-- >>> isn't _Right (parse name "test" "")
-- True
--
-- >>> parse name "test" "A"
-- Right "A"
--
-- >>> parse name "test" "x."
-- Right "x."
--
-- >>> parse name "test" "_-"
-- Right "_-"
--
-- >>> parse name "test" ":_"
-- Right ":_"
--
-- >>> parse name "test" "a:"
-- Right "a:"
--
-- >>> parse name "test" "A:"
-- Right "A:"
--
-- >>> parse name "test" "_\231"
-- Right "_\231"
name ::
  CharParsing m =>
  m Text
name =
  cons <$> (letter <|> char '_' <|> char ':') <*> namecharacters

-- | Parse a names @Name (S Name)*@.
-- todo cannot use sepBy
names ::
  CharParsing m =>
  m [Text]
names =
  name `sepBy` whitespace

-- | Parse a nmtoken @(NameChar)+@.
nmtoken ::
  CharParsing m =>
  m Text
nmtoken =
  namecharacters1

-- | Parse nmtokens @Nmtoken (S Nmtoken)*@.
-- todo cannot use sepBy
nmtokens ::
  CharParsing m =>
  m [Text]
nmtokens =
  nmtoken `sepBy` whitespace


                                   -}