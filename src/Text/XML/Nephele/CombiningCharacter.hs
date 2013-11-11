{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele.CombiningCharacter(
  CombiningCharacter
, combiningCharacter
, combiningCharacters
, combiningCharacters1
, combiningCharacter'
, combiningCharacters'
) where

import Text.Parser.Char(CharParsing(..), satisfyRange)
import Text.Parsec(parse)
import Text.Parsec.Text()
import Data.Text(Text, singleton)
import Control.Applicative(Applicative(..), Alternative(..), (<$>))
import Data.Foldable(asum)
import Data.List.NonEmpty(NonEmpty(..))
import Control.Lens(Prism', prism', (^?), _Right)
import Prelude(Char, Eq(..), Show(..), Ord(..), (&&), (||), (.), ($), Bool, String, error)

-- $setup
-- >>> import Data.Text

newtype CombiningCharacter =
  CombiningCharacter Char
  deriving (Eq, Ord, Show)

-- | Parse a combining character.
--
-- @[#x0300-#x0345] | [#x0360-#x0361] | [#x0483-#x0486] | [#x0591-#x05A1] | [#x05A3-#x05B9] | [#x05BB-#x05BD] | #x05BF | [#x05C1-#x05C2] | #x05C4 | [#x064B-#x0652] | #x0670 | [#x06D6-#x06DC] | [#x06DD-#x06DF] | [#x06E0-#x06E4] | [#x06E7-#x06E8] | [#x06EA-#x06ED] | [#x0901-#x0903] | #x093C | [#x093E-#x094C] | #x094D | [#x0951-#x0954] | [#x0962-#x0963] | [#x0981-#x0983] | #x09BC | #x09BE | #x09BF | [#x09C0-#x09C4] | [#x09C7-#x09C8] | [#x09CB-#x09CD] | #x09D7 | [#x09E2-#x09E3] | #x0A02 | #x0A3C | #x0A3E | #x0A3F | [#x0A40-#x0A42] | [#x0A47-#x0A48] | [#x0A4B-#x0A4D] | [#x0A70-#x0A71] | [#x0A81-#x0A83] | #x0ABC | [#x0ABE-#x0AC5] | [#x0AC7-#x0AC9] | [#x0ACB-#x0ACD] | [#x0B01-#x0B03] | #x0B3C | [#x0B3E-#x0B43] | [#x0B47-#x0B48] | [#x0B4B-#x0B4D] | [#x0B56-#x0B57] | [#x0B82-#x0B83] | [#x0BBE-#x0BC2] | [#x0BC6-#x0BC8] | [#x0BCA-#x0BCD] | #x0BD7 | [#x0C01-#x0C03] | [#x0C3E-#x0C44] | [#x0C46-#x0C48] | [#x0C4A-#x0C4D] | [#x0C55-#x0C56] | [#x0C82-#x0C83] | [#x0CBE-#x0CC4] | [#x0CC6-#x0CC8] | [#x0CCA-#x0CCD] | [#x0CD5-#x0CD6] | [#x0D02-#x0D03] | [#x0D3E-#x0D43] | [#x0D46-#x0D48] | [#x0D4A-#x0D4D] | #x0D57 | #x0E31 | [#x0E34-#x0E3A] | [#x0E47-#x0E4E] | #x0EB1 | [#x0EB4-#x0EB9] | [#x0EBB-#x0EBC] | [#x0EC8-#x0ECD] | [#x0F18-#x0F19] | #x0F35 | #x0F37 | #x0F39 | #x0F3E | #x0F3F | [#x0F71-#x0F84] | [#x0F86-#x0F8B] | [#x0F90-#x0F95] | #x0F97 | [#x0F99-#x0FAD] | [#x0FB1-#x0FB7] | #x0FB9 | [#x20D0-#x20DC] | #x20E1 | [#x302A-#x302F] | #x3099 | #x309A@.
--
-- >>> parse combiningCharacter "test" "\784bc"
-- Right (CombiningCharacter '\784')
combiningCharacter ::
  CharParsing m =>
  m CombiningCharacter
combiningCharacter =
  let c = asum [
            satisfyRange '\x0300' '\x0345'
          , satisfyRange '\x0360' '\x0361'
          , satisfyRange '\x0483' '\x0486'
          , satisfyRange '\x0591' '\x05A1'
          , satisfyRange '\x05A3' '\x05B9'
          , satisfyRange '\x05BB' '\x05BD'
          , char '\x05BF'
          , satisfyRange '\x05C1' '\x05C2'
          , char '\x05C4'
          , satisfyRange '\x064B' '\x0652'
          , char '\x0670'
          , satisfyRange '\x06D6' '\x06DC'
          , satisfyRange '\x06DD' '\x06DF'
          , satisfyRange '\x06E0' '\x06E4'
          , satisfyRange '\x06E7' '\x06E8'
          , satisfyRange '\x06EA' '\x06ED'
          , satisfyRange '\x0901' '\x0903'
          , char '\x093C'
          , satisfyRange '\x093E' '\x094C'
          , char '\x094D'
          , satisfyRange '\x0951' '\x0954'
          , satisfyRange '\x0962' '\x0963'
          , satisfyRange '\x0981' '\x0983'
          , char '\x09BC'
          , char '\x09BE'
          , char '\x09BF'
          , satisfyRange '\x09C0' '\x09C4'
          , satisfyRange '\x09C7' '\x09C8'
          , satisfyRange '\x09CB' '\x09CD'
          , char '\x09D7'
          , satisfyRange '\x09E2' '\x09E3'
          , char '\x0A02'
          , char '\x0A3C'
          , char '\x0A3E'
          , char '\x0A3F'
          , satisfyRange '\x0A40' '\x0A42'
          , satisfyRange '\x0A47' '\x0A48'
          , satisfyRange '\x0A4B' '\x0A4D'
          , satisfyRange '\x0A70' '\x0A71'
          , satisfyRange '\x0A81' '\x0A83'
          , char '\x0ABC'
          , satisfyRange '\x0ABE' '\x0AC5'
          , satisfyRange '\x0AC7' '\x0AC9'
          , satisfyRange '\x0ACB' '\x0ACD'
          , satisfyRange '\x0B01' '\x0B03'
          , char '\x0B3C'
          , satisfyRange '\x0B3E' '\x0B43'
          , satisfyRange '\x0B47' '\x0B48'
          , satisfyRange '\x0B4B' '\x0B4D'
          , satisfyRange '\x0B56' '\x0B57'
          , satisfyRange '\x0B82' '\x0B83'
          , satisfyRange '\x0BBE' '\x0BC2'
          , satisfyRange '\x0BC6' '\x0BC8'
          , satisfyRange '\x0BCA' '\x0BCD'
          , char '\x0BD7'
          , satisfyRange '\x0C01' '\x0C03'
          , satisfyRange '\x0C3E' '\x0C44'
          , satisfyRange '\x0C46' '\x0C48'
          , satisfyRange '\x0C4A' '\x0C4D'
          , satisfyRange '\x0C55' '\x0C56'
          , satisfyRange '\x0C82' '\x0C83'
          , satisfyRange '\x0CBE' '\x0CC4'
          , satisfyRange '\x0CC6' '\x0CC8'
          , satisfyRange '\x0CCA' '\x0CCD'
          , satisfyRange '\x0CD5' '\x0CD6'
          , satisfyRange '\x0D02' '\x0D03'
          , satisfyRange '\x0D3E' '\x0D43'
          , satisfyRange '\x0D46' '\x0D48'
          , satisfyRange '\x0D4A' '\x0D4D'
          , char '\x0D57'
          , char '\x0E31'
          , satisfyRange '\x0E34' '\x0E3A'
          , satisfyRange '\x0E47' '\x0E4E'
          , char '\x0EB1'
          , satisfyRange '\x0EB4' '\x0EB9'
          , satisfyRange '\x0EBB' '\x0EBC'
          , satisfyRange '\x0EC8' '\x0ECD'
          , satisfyRange '\x0F18' '\x0F19'
          , char '\x0F35'
          , char '\x0F37'
          , char '\x0F39'
          , char '\x0F3E'
          , char '\x0F3F'
          , satisfyRange '\x0F71' '\x0F84'
          , satisfyRange '\x0F86' '\x0F8B'
          , satisfyRange '\x0F90' '\x0F95'
          , char '\x0F97'
          , satisfyRange '\x0F99' '\x0FAD'
          , satisfyRange '\x0FB1' '\x0FB7'
          , char '\x0FB9'
          , satisfyRange '\x20D0' '\x20DC'
          , char '\x20E1'
          , satisfyRange '\x302A' '\x302F'
          , char '\x3099'
          , char '\x309A'
          ]
  in CombiningCharacter <$> c

-- | Parse zero or many combining characters.
combiningCharacters ::
  CharParsing m =>
  m [CombiningCharacter]
combiningCharacters =
  many combiningCharacter

-- | Parse one or many combining characters.
combiningCharacters1 ::
  CharParsing m =>
  m (NonEmpty CombiningCharacter)
combiningCharacters1 =
  some1 combiningCharacter

-- | CombiningCharacter prism from a char.
--
-- >>> '\784' ^? combiningCharacter'
-- Just (CombiningCharacter '\784')
combiningCharacter' ::
  Prism' Char CombiningCharacter
combiningCharacter' =
  prism'
    (\(CombiningCharacter c) -> c)
    ((^? _Right) . parse combiningCharacter "combiningCharacter'" . singleton)

-- | CombiningCharacter prism from text.
--
-- >>> pack "\784bc" ^? combiningCharacters'
-- Just (CombiningCharacter '\784')
combiningCharacters' ::
  Prism' Text CombiningCharacter
combiningCharacters' =
  prism'
    (\(CombiningCharacter c) -> singleton c)
    ((^? _Right) . parse combiningCharacter "combiningCharacter'")

-- todo move to utility
some1 ::
  Alternative f =>
  f a
  -> f (NonEmpty a)
some1 x =
  (:|) <$> x <*> some x
