{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele where

import Text.Parser.Char
import Data.Text(Text)
import Data.Functor.Product

-- $setup
-- >>> import Text.Parsec
-- >>> import Control.Lens
-- >>> import Test.QuickCheck
-- >>> import Prelude
-- >>> newtype UncommentBegin = UncommentBegin String deriving (Eq, Show)
-- >>> instance Arbitrary UncommentBegin where arbitrary = fmap UncommentBegin (arbitrary `suchThat` (/= "<!--"))
-- >>> newtype UncommentEnd = UncommentEnd String deriving (Eq, Show)
-- >>> instance Arbitrary UncommentEnd where arbitrary = fmap UncommentEnd (arbitrary `suchThat` (/= "-->"))

-- | The parser for the beginning of a comment.
--
-- >>> parse commentBegin "test" "<!--"
-- Right "<!--"
--
-- prop> \(UncommentBegin s) -> isn't _Right (parse commentBegin "test" s)
commentBegin ::
  CharParsing m =>
  m Text
commentBegin =
  text "<!--"

-- | The parser for the end of a comment.
--
-- >>> parse commentEnd "test" "-->"
-- Right "-->"
--
-- prop> \(UncommentEnd s) -> isn't _Right (parse commentEnd "test" s)
commentEnd ::
  CharParsing m =>
  m Text
commentEnd =
  text "-->"

comment ::
  (CharParsing f, CharParsing g) =>
  Product f g Text
comment =
  Pair commentBegin commentEnd
