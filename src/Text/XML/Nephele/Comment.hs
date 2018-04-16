{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.Comment(
  Comment
, commentCharacters
, comment
, comment'
) where

import Data.Text(Text, pack)
import qualified Data.Text as Text(concat)
import Papa
import Text.Parser.Char(CharParsing(char, text), oneOf, satisfyRange)
import Text.Parser.Combinators(try, between)
import Text.Parsec(parse)

-- $setup
-- >>> import Text.Parsec
-- >>> import Control.Lens
-- >>> import Test.QuickCheck
-- >>> import Data.Maybe
-- >>> import Prelude
-- >>> -- is this function required? http://lpaste.net/95317
-- >>> let generateUntilJust gen p = let try _ 0 = return Nothing; try k n = do x <- resize (2*k+n) gen; maybe (try (k+1) (n-1)) (return . Just) (p x) in do mx <- sized (try 0 . max 1); case mx of Just x  -> return x; Nothing -> sized (\n -> resize (n+1) (generateUntilJust gen p))
-- >>> newtype UncommentBegin = UncommentBegin String deriving (Eq, Show)
-- >>> instance Arbitrary UncommentBegin where arbitrary = fmap UncommentBegin (arbitrary `suchThat` (/= "<!--"))
-- >>> newtype UncommentEnd = UncommentEnd String deriving (Eq, Show)
-- >>> instance Arbitrary UncommentEnd where arbitrary = fmap UncommentEnd (arbitrary `suchThat` (/= "-->"))
-- >>> instance Arbitrary Comment where arbitrary = generateUntilJust arbitrary ((^? _Right) . parse commentCharacters "instance Arbitrary Comment" . pack)

newtype Comment =
  Comment Text
  deriving (Eq, Ord, Show)

-- | Reverses a comment.
--
-- >>> reversing <$> (pack "ABC" ^? comment')
-- Just (Comment "CBA")
--
-- prop> reversing (reversing c) == (c :: Comment)
instance Reversing Comment where
  reversing (Comment t) =
    Comment (reversing t)

-- | The parser for the characters that make up a comment.
--
-- >>> parse commentCharacters "test" ""
-- Right (Comment "")
--
-- >>> parse commentCharacters "test" " abc "
-- Right (Comment " abc ")
--
-- >>> parse commentCharacters "test" " a-bc "
-- Right (Comment " a-bc ")
--
-- >>> parse commentCharacters "test" " a-b-c "
-- Right (Comment " a-b-c ")
--
-- >>> parse commentCharacters "test" " a-bb-cc "
-- Right (Comment " a-bb-cc ")
--
-- >>> parse commentCharacters "test" " abc- "
-- Right (Comment " abc- ")
commentCharacters ::
  CharParsing m =>
  m Comment
commentCharacters =
  -- character without '-'
  let nominus = oneOf ['\x9', '\xA', '\xD']
                <|> satisfyRange '\x20' '\x2C'
                <|> satisfyRange '\x2E' '\xD7FF'
                <|> satisfyRange '\xE000' '\xFFFD'
                <|> satisfyRange '\x10000' '\x10FFFF'
      s = (\m c -> [m, c]) <$> char '-' <*> nominus
      t = (:[]) <$> nominus
      ch = Text.concat <$> many (pack <$> (try s <|> t))
  in Comment <$> ch

-- | The parser for a comment.
--
-- >>> parse comment "test" "<!---->"
-- Right (Comment "")
--
-- >>> parse comment "test" "<!-- abc -->"
-- Right (Comment " abc ")
--
-- >>> parse comment "test" "<!-- a-bc -->"
-- Right (Comment " a-bc ")
--
-- >>> parse comment "test" "<!-- a-b-c -->"
-- Right (Comment " a-b-c ")
--
-- >>> parse comment "test" "<!-- a-bb-cc -->"
-- Right (Comment " a-bb-cc ")
--
-- >>> parse comment "test" "<!-- abc- -->"
-- Right (Comment " abc- ")
comment ::
  CharParsing m =>
  m Comment
comment =
  between (text "<!--") (text "-->") commentCharacters

-- | Comment prism from text.
--
-- >>> pack "abc" ^? comment'
-- Just (Comment "abc")
comment' ::
  Prism' Text Comment
comment' =
  prism'
    (\(Comment t) -> t)
    ((^? _Right) . parse commentCharacters "comment'")
