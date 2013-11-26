{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele(module N) where

import Text.XML.Nephele.BaseCharacter as N
import Text.XML.Nephele.Character as N
import Text.XML.Nephele.CombiningCharacter as N
import Text.XML.Nephele.Comment as N
import Text.XML.Nephele.Digit as N
import Text.XML.Nephele.EntityReference as N
import Text.XML.Nephele.Extender as N
import Text.XML.Nephele.Ideographic as N
import Text.XML.Nephele.Letter as N
import Text.XML.Nephele.Name as N
import Text.XML.Nephele.NameCharacter as N
import Text.XML.Nephele.Nmtokens as N
import Text.XML.Nephele.ParameterEntityReference as N
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