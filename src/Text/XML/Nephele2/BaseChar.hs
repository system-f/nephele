{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.XML.Nephele2.BaseChar(
  BaseChar
, parseBaseChar
, parseBaseChars
, parseBaseChars1
, AsBaseChar(..)
) where

import Data.Text(Text, singleton)
import Data.Text1(Text1, _Single)
import Papa
import Text.Parsec(parse)
import Text.Parser.Char(CharParsing(char), satisfyRange)
import Text.Parsec.Prim(Stream)

newtype BaseChar =
  BaseChar Char
  deriving (Eq, Ord, Show)

-- | Parse a base character.
--
-- @[#x0041-#x005A] | [#x0061-#x007A] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x00FF] | [#x0100-#x0131] | [#x0134-#x013E] | [#x0141-#x0148] | [#x014A-#x017E] | [#x0180-#x01C3] | [#x01CD-#x01F0] | [#x01F4-#x01F5] | [#x01FA-#x0217] | [#x0250-#x02A8] | [#x02BB-#x02C1] | #x0386 | [#x0388-#x038A] | #x038C | [#x038E-#x03A1] | [#x03A3-#x03CE] | [#x03D0-#x03D6] | #x03DA | #x03DC | #x03DE | #x03E0 | [#x03E2-#x03F3] | [#x0401-#x040C] | [#x040E-#x044F] | [#x0451-#x045C] | [#x045E-#x0481] | [#x0490-#x04C4] | [#x04C7-#x04C8] | [#x04CB-#x04CC] | [#x04D0-#x04EB] | [#x04EE-#x04F5] | [#x04F8-#x04F9] | [#x0531-#x0556] | #x0559 | [#x0561-#x0586] | [#x05D0-#x05EA] | [#x05F0-#x05F2] | [#x0621-#x063A] | [#x0641-#x064A] | [#x0671-#x06B7] | [#x06BA-#x06BE] | [#x06C0-#x06CE] | [#x06D0-#x06D3] | #x06D5 | [#x06E5-#x06E6] | [#x0905-#x0939] | #x093D | [#x0958-#x0961] | [#x0985-#x098C] | [#x098F-#x0990] | [#x0993-#x09A8] | [#x09AA-#x09B0] | #x09B2 | [#x09B6-#x09B9] | [#x09DC-#x09DD] | [#x09DF-#x09E1] | [#x09F0-#x09F1] | [#x0A05-#x0A0A] | [#x0A0F-#x0A10] | [#x0A13-#x0A28] | [#x0A2A-#x0A30] | [#x0A32-#x0A33] | [#x0A35-#x0A36] | [#x0A38-#x0A39] | [#x0A59-#x0A5C] | #x0A5E | [#x0A72-#x0A74] | [#x0A85-#x0A8B] | #x0A8D | [#x0A8F-#x0A91] | [#x0A93-#x0AA8] | [#x0AAA-#x0AB0] | [#x0AB2-#x0AB3] | [#x0AB5-#x0AB9] | #x0ABD | #x0AE0 | [#x0B05-#x0B0C] | [#x0B0F-#x0B10] | [#x0B13-#x0B28] | [#x0B2A-#x0B30] | [#x0B32-#x0B33] | [#x0B36-#x0B39] | #x0B3D | [#x0B5C-#x0B5D] | [#x0B5F-#x0B61] | [#x0B85-#x0B8A] | [#x0B8E-#x0B90] | [#x0B92-#x0B95] | [#x0B99-#x0B9A] | #x0B9C | [#x0B9E-#x0B9F] | [#x0BA3-#x0BA4] | [#x0BA8-#x0BAA] | [#x0BAE-#x0BB5] | [#x0BB7-#x0BB9] | [#x0C05-#x0C0C] | [#x0C0E-#x0C10] | [#x0C12-#x0C28] | [#x0C2A-#x0C33] | [#x0C35-#x0C39] | [#x0C60-#x0C61] | [#x0C85-#x0C8C] | [#x0C8E-#x0C90] | [#x0C92-#x0CA8] | [#x0CAA-#x0CB3] | [#x0CB5-#x0CB9] | #x0CDE | [#x0CE0-#x0CE1] | [#x0D05-#x0D0C] | [#x0D0E-#x0D10] | [#x0D12-#x0D28] | [#x0D2A-#x0D39] | [#x0D60-#x0D61] | [#x0E01-#x0E2E] | #x0E30 | [#x0E32-#x0E33] | [#x0E40-#x0E45] | [#x0E81-#x0E82] | #x0E84 | [#x0E87-#x0E88] | #x0E8A | #x0E8D | [#x0E94-#x0E97] | [#x0E99-#x0E9F] | [#x0EA1-#x0EA3] | #x0EA5 | #x0EA7 | [#x0EAA-#x0EAB] | [#x0EAD-#x0EAE] | #x0EB0 | [#x0EB2-#x0EB3] | #x0EBD | [#x0EC0-#x0EC4] | [#x0F40-#x0F47] | [#x0F49-#x0F69] | [#x10A0-#x10C5] | [#x10D0-#x10F6] | #x1100 | [#x1102-#x1103] | [#x1105-#x1107] | #x1109 | [#x110B-#x110C] | [#x110E-#x1112] | #x113C | #x113E | #x1140 | #x114C | #x114E | #x1150 | [#x1154-#x1155] | #x1159 | [#x115F-#x1161] | #x1163 | #x1165 | #x1167 | #x1169 | [#x116D-#x116E] | [#x1172-#x1173] | #x1175 | #x119E | #x11A8 | #x11AB | [#x11AE-#x11AF] | [#x11B7-#x11B8] | #x11BA | [#x11BC-#x11C2] | #x11EB | #x11F0 | #x11F9 | [#x1E00-#x1E9B] | [#x1EA0-#x1EF9] | [#x1F00-#x1F15] | [#x1F18-#x1F1D] | [#x1F20-#x1F45] | [#x1F48-#x1F4D] | [#x1F50-#x1F57] | #x1F59 | #x1F5B | #x1F5D | [#x1F5F-#x1F7D] | [#x1F80-#x1FB4] | [#x1FB6-#x1FBC] | #x1FBE | [#x1FC2-#x1FC4] | [#x1FC6-#x1FCC] | [#x1FD0-#x1FD3] | [#x1FD6-#x1FDB] | [#x1FE0-#x1FEC] | [#x1FF2-#x1FF4] | [#x1FF6-#x1FFC] | #x2126 | [#x212A-#x212B] | #x212E | [#x2180-#x2182] | [#x3041-#x3094] | [#x30A1-#x30FA] | [#x3105-#x312C] | [#xAC00-#xD7A3]@.
parseBaseChar ::
  CharParsing m =>
  m BaseChar
parseBaseChar =
  let c = asum [
            satisfyRange '\x0041' '\x005A'
          , satisfyRange '\x0061' '\x007A'
          , satisfyRange '\x00C0' '\x00D6'
          , satisfyRange '\x00D8' '\x00F6'
          , satisfyRange '\x00F8' '\x00FF'
          , satisfyRange '\x0100' '\x0131'
          , satisfyRange '\x0134' '\x013E'
          , satisfyRange '\x0141' '\x0148'
          , satisfyRange '\x014A' '\x017E'
          , satisfyRange '\x0180' '\x01C3'
          , satisfyRange '\x01CD' '\x01F0'
          , satisfyRange '\x01F4' '\x01F5'
          , satisfyRange '\x01FA' '\x0217'
          , satisfyRange '\x0250' '\x02A8'
          , satisfyRange '\x02BB' '\x02C1'
          , char '\x0386'
          , satisfyRange '\x0388' '\x038A'
          , char '\x038C'
          , satisfyRange '\x038E' '\x03A1'
          , satisfyRange '\x03A3' '\x03CE'
          , satisfyRange '\x03D0' '\x03D6'
          , char '\x03DA'
          , char '\x03DC'
          , char '\x03DE'
          , char '\x03E0'
          , satisfyRange '\x03E2' '\x03F3'
          , satisfyRange '\x0401' '\x040C'
          , satisfyRange '\x040E' '\x044F'
          , satisfyRange '\x0451' '\x045C'
          , satisfyRange '\x045E' '\x0481'
          , satisfyRange '\x0490' '\x04C4'
          , satisfyRange '\x04C7' '\x04C8'
          , satisfyRange '\x04CB' '\x04CC'
          , satisfyRange '\x04D0' '\x04EB'
          , satisfyRange '\x04EE' '\x04F5'
          , satisfyRange '\x04F8' '\x04F9'
          , satisfyRange '\x0531' '\x0556'
          , char '\x0559'
          , satisfyRange '\x0561' '\x0586'
          , satisfyRange '\x05D0' '\x05EA'
          , satisfyRange '\x05F0' '\x05F2'
          , satisfyRange '\x0621' '\x063A'
          , satisfyRange '\x0641' '\x064A'
          , satisfyRange '\x0671' '\x06B7'
          , satisfyRange '\x06BA' '\x06BE'
          , satisfyRange '\x06C0' '\x06CE'
          , satisfyRange '\x06D0' '\x06D3'
          , char '\x06D5'
          , satisfyRange '\x06E5' '\x06E6'
          , satisfyRange '\x0905' '\x0939'
          , char '\x093D'
          , satisfyRange '\x0958' '\x0961'
          , satisfyRange '\x0985' '\x098C'
          , satisfyRange '\x098F' '\x0990'
          , satisfyRange '\x0993' '\x09A8'
          , satisfyRange '\x09AA' '\x09B0'
          , char '\x09B2'
          , satisfyRange '\x09B6' '\x09B9'
          , satisfyRange '\x09DC' '\x09DD'
          , satisfyRange '\x09DF' '\x09E1'
          , satisfyRange '\x09F0' '\x09F1'
          , satisfyRange '\x0A05' '\x0A0A'
          , satisfyRange '\x0A0F' '\x0A10'
          , satisfyRange '\x0A13' '\x0A28'
          , satisfyRange '\x0A2A' '\x0A30'
          , satisfyRange '\x0A32' '\x0A33'
          , satisfyRange '\x0A35' '\x0A36'
          , satisfyRange '\x0A38' '\x0A39'
          , satisfyRange '\x0A59' '\x0A5C'
          , char '\x0A5E'
          , satisfyRange '\x0A72' '\x0A74'
          , satisfyRange '\x0A85' '\x0A8B'
          , char '\x0A8D'
          , satisfyRange '\x0A8F' '\x0A91'
          , satisfyRange '\x0A93' '\x0AA8'
          , satisfyRange '\x0AAA' '\x0AB0'
          , satisfyRange '\x0AB2' '\x0AB3'
          , satisfyRange '\x0AB5' '\x0AB9'
          , char '\x0ABD'
          , char '\x0AE0'
          , satisfyRange '\x0B05' '\x0B0C'
          , satisfyRange '\x0B0F' '\x0B10'
          , satisfyRange '\x0B13' '\x0B28'
          , satisfyRange '\x0B2A' '\x0B30'
          , satisfyRange '\x0B32' '\x0B33'
          , satisfyRange '\x0B36' '\x0B39'
          , char '\x0B3D'
          , satisfyRange '\x0B5C' '\x0B5D'
          , satisfyRange '\x0B5F' '\x0B61'
          , satisfyRange '\x0B85' '\x0B8A'
          , satisfyRange '\x0B8E' '\x0B90'
          , satisfyRange '\x0B92' '\x0B95'
          , satisfyRange '\x0B99' '\x0B9A'
          , char '\x0B9C'
          , satisfyRange '\x0B9E' '\x0B9F'
          , satisfyRange '\x0BA3' '\x0BA4'
          , satisfyRange '\x0BA8' '\x0BAA'
          , satisfyRange '\x0BAE' '\x0BB5'
          , satisfyRange '\x0BB7' '\x0BB9'
          , satisfyRange '\x0C05' '\x0C0C'
          , satisfyRange '\x0C0E' '\x0C10'
          , satisfyRange '\x0C12' '\x0C28'
          , satisfyRange '\x0C2A' '\x0C33'
          , satisfyRange '\x0C35' '\x0C39'
          , satisfyRange '\x0C60' '\x0C61'
          , satisfyRange '\x0C85' '\x0C8C'
          , satisfyRange '\x0C8E' '\x0C90'
          , satisfyRange '\x0C92' '\x0CA8'
          , satisfyRange '\x0CAA' '\x0CB3'
          , satisfyRange '\x0CB5' '\x0CB9'
          , char '\x0CDE'
          , satisfyRange '\x0CE0' '\x0CE1'
          , satisfyRange '\x0D05' '\x0D0C'
          , satisfyRange '\x0D0E' '\x0D10'
          , satisfyRange '\x0D12' '\x0D28'
          , satisfyRange '\x0D2A' '\x0D39'
          , satisfyRange '\x0D60' '\x0D61'
          , satisfyRange '\x0E01' '\x0E2E'
          , char '\x0E30'
          , satisfyRange '\x0E32' '\x0E33'
          , satisfyRange '\x0E40' '\x0E45'
          , satisfyRange '\x0E81' '\x0E82'
          , char '\x0E84'
          , satisfyRange '\x0E87' '\x0E88'
          , char '\x0E8A'
          , char '\x0E8D'
          , satisfyRange '\x0E94' '\x0E97'
          , satisfyRange '\x0E99' '\x0E9F'
          , satisfyRange '\x0EA1' '\x0EA3'
          , char '\x0EA5'
          , char '\x0EA7'
          , satisfyRange '\x0EAA' '\x0EAB'
          , satisfyRange '\x0EAD' '\x0EAE'
          , char '\x0EB0'
          , satisfyRange '\x0EB2' '\x0EB3'
          , char '\x0EBD'
          , satisfyRange '\x0EC0' '\x0EC4'
          , satisfyRange '\x0F40' '\x0F47'
          , satisfyRange '\x0F49' '\x0F69'
          , satisfyRange '\x10A0' '\x10C5'
          , satisfyRange '\x10D0' '\x10F6'
          , char '\x1100'
          , satisfyRange '\x1102' '\x1103'
          , satisfyRange '\x1105' '\x1107'
          , char '\x1109'
          , satisfyRange '\x110B' '\x110C'
          , satisfyRange '\x110E' '\x1112'
          , char '\x113C'
          , char '\x113E'
          , char '\x1140'
          , char '\x114C'
          , char '\x114E'
          , char '\x1150'
          , satisfyRange '\x1154' '\x1155'
          , char '\x1159'
          , satisfyRange '\x115F' '\x1161'
          , char '\x1163'
          , char '\x1165'
          , char '\x1167'
          , char '\x1169'
          , satisfyRange '\x116D' '\x116E'
          , satisfyRange '\x1172' '\x1173'
          , char '\x1175'
          , char '\x119E'
          , char '\x11A8'
          , char '\x11AB'
          , satisfyRange '\x11AE' '\x11AF'
          , satisfyRange '\x11B7' '\x11B8'
          , char '\x11BA'
          , satisfyRange '\x11BC' '\x11C2'
          , char '\x11EB'
          , char '\x11F0'
          , char '\x11F9'
          , satisfyRange '\x1E00' '\x1E9B'
          , satisfyRange '\x1EA0' '\x1EF9'
          , satisfyRange '\x1F00' '\x1F15'
          , satisfyRange '\x1F18' '\x1F1D'
          , satisfyRange '\x1F20' '\x1F45'
          , satisfyRange '\x1F48' '\x1F4D'
          , satisfyRange '\x1F50' '\x1F57'
          , char '\x1F59'
          , char '\x1F5B'
          , char '\x1F5D'
          , satisfyRange '\x1F5F' '\x1F7D'
          , satisfyRange '\x1F80' '\x1FB4'
          , satisfyRange '\x1FB6' '\x1FBC'
          , char '\x1FBE'
          , satisfyRange '\x1FC2' '\x1FC4'
          , satisfyRange '\x1FC6' '\x1FCC'
          , satisfyRange '\x1FD0' '\x1FD3'
          , satisfyRange '\x1FD6' '\x1FDB'
          , satisfyRange '\x1FE0' '\x1FEC'
          , satisfyRange '\x1FF2' '\x1FF4'
          , satisfyRange '\x1FF6' '\x1FFC'
          , char '\x2126'
          , satisfyRange '\x212A' '\x212B'
          , char '\x212E'
          , satisfyRange '\x2180' '\x2182'
          , satisfyRange '\x3041' '\x3094'
          , satisfyRange '\x30A1' '\x30FA'
          , satisfyRange '\x3105' '\x312C'
          , satisfyRange '\xAC00' '\xD7A3'
          ]
  in BaseChar <$> c

-- | Parse zero or many baseChars.
parseBaseChars ::
  CharParsing m =>
  m [BaseChar]
parseBaseChars =
  many parseBaseChar

-- | Parse one or many baseChars.
parseBaseChars1 ::
  CharParsing m =>
  m (NonEmpty BaseChar)
parseBaseChars1 =
  some1 parseBaseChar

-- | Prism to a BaseChar.
class AsBaseChar a where
  _BaseChar ::
    Prism'
      a
      BaseChar

instance AsBaseChar BaseChar where
  _BaseChar =
     id

-- | BaseChar prism from a char.
instance AsBaseChar Char where
  _BaseChar =
    prism'
      (\(BaseChar c) -> c)
      (streamBaseChar . singleton)

-- | BaseChar prism from a text.
instance AsBaseChar Text where
  _BaseChar =
    prism'
      (\(BaseChar c) -> singleton c)
      streamBaseChar

-- | BaseChar prism from a string.
instance AsBaseChar [Char] where
  _BaseChar =
    prism'
      (\(BaseChar c) -> [c])
      streamBaseChar

-- | BaseChar prism from a text1.
instance AsBaseChar Text1 where
  _BaseChar =
    prism'
      (\(BaseChar c) -> _Single # c)
      (\x -> x ^? _Single >>= \c -> c ^? _BaseChar)

-- not exported
streamBaseChar ::
  Stream a Identity Char =>
  a
  -> Maybe BaseChar
streamBaseChar =
  (^? _Right) . parse parseBaseChar "parseBaseChar"
