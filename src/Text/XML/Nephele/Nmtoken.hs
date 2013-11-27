module Text.XML.Nephele.Nmtoken(
  Nmtoken
, nmtoken
) where

import Text.Parser.Char(CharParsing(..))
import Text.Parsec.Text()
import Control.Applicative((<$>))
import Data.List.NonEmpty(NonEmpty(..))
import Prelude(Char, Eq(..), Show(..), Ord(..), (&&), (||), (.), ($), Bool, String, flip, id, error, undefined)
import Text.XML.Nephele.NameCharacter(NameCharacter, nameCharacters1)

-- $setup
-- >>> import Text.Parsec(parse)
-- >>> import Data.Text
-- >>> import Control.Lens

data Nmtoken =
  Nmtoken (NonEmpty NameCharacter)
  deriving (Eq, Show)

-- | Parse nmtoken.
--
-- @(NameChar)+@.
--
-- >>> parse nmtoken "test" "abc  def"
-- Right (Nmtoken (LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'a')) :| [LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'b')),LetterNameCharacter (BaseCharacterLetter (BaseCharacter 'c'))]))
nmtoken ::
  CharParsing m =>
  m Nmtoken
nmtoken =
  Nmtoken <$> nameCharacters1
