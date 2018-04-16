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

newtype Comment =
  Comment Text
  deriving (Eq, Ord, Show)

-- | The parser for the characters that make up a comment.
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
comment ::
  CharParsing m =>
  m Comment
comment =
  between (text "<!--") (text "-->") commentCharacters

-- | Comment prism from text.
comment' ::
  Prism' Text Comment
comment' =
  prism'
    (\(Comment t) -> t)
    ((^? _Right) . parse commentCharacters "comment'")
