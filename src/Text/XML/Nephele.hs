{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Nephele(module N) where

import Text.XML.Nephele.Character as N
import Text.XML.Nephele.Comment as N
import Text.XML.Nephele.Extender as N
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

-- | Parse a letter @BaseChar |  Ideographic@.
letter ::
  CharParsing m =>
  m Char
letter =
  basecharacter <|> ideographic

-- | Parse a base character.
basecharacter ::
  CharParsing m =>
  m Char
basecharacter =
  asum [
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

-- | Parse a combining character.
combiningcharacter ::
  CharParsing m =>
  m Char
combiningcharacter =
  asum [
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

-- | Parse a digit.
digit ::
  CharParsing m =>
  m Char
digit =
  asum [
    satisfyRange '\x0030' '\x0039'
  , satisfyRange '\x0660' '\x0669'
  , satisfyRange '\x06F0' '\x06F9'
  , satisfyRange '\x0966' '\x096F'
  , satisfyRange '\x09E6' '\x09EF'
  , satisfyRange '\x0A66' '\x0A6F'
  , satisfyRange '\x0AE6' '\x0AEF'
  , satisfyRange '\x0B66' '\x0B6F'
  , satisfyRange '\x0BE7' '\x0BEF'
  , satisfyRange '\x0C66' '\x0C6F'
  , satisfyRange '\x0CE6' '\x0CEF'
  , satisfyRange '\x0D66' '\x0D6F'
  , satisfyRange '\x0E50' '\x0E59'
  , satisfyRange '\x0ED0' '\x0ED9'
  , satisfyRange '\x0F20' '\x0F29'
  ]

-- | Parse an ideographic.
ideographic ::
  CharParsing m =>
  m Char
ideographic =
  asum [
    satisfyRange '\x4E00' '\x9FA5'
  , char '\x3007'
  , satisfyRange '\x3021' '\x3029'
  ]
