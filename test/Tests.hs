{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Text(pack)
import Papa
import Test.Tasty(TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit(testCase, (@?=), assertBool)
import Text.Parsec(parse)
import Text.XML.Nephele.BaseCharacter
import Text.XML.Nephele.Character
import Text.XML.Nephele.CombiningCharacter
import Text.XML.Nephele.Comment
import Text.XML.Nephele.Digit
import Text.XML.Nephele.Extender
import Text.XML.Nephele.Ideographic
import Text.XML.Nephele.Whitespace
import qualified Text.XML.Nephele.Whitespace as Whitespace

main :: IO ()
main =
  defaultMain $
    testGroup "nephele"
      [
        testBaseCharacter
      , testCharacter
      , testCombiningCharacter
      , testComment
      , testDigit
      ]

testBaseCharacter ::
  TestTree
testBaseCharacter =
  testGroup "Text.XML.Nephele.BaseCharacter" $
    [
      testCase "baseCharacter" (parse baseCharacter "test" "abc" ^? _Right @?= 'a' ^? baseCharacter')
    , testCase "baseCharacter'" (assertBool "isJust" (isJust ('a' ^? baseCharacter')))
    , testCase "baseCharacters'" (pack "abc" ^? baseCharacters' @?= 'a' ^? baseCharacter')
    ]

testCharacter ::
  TestTree
testCharacter =
  testGroup "Text.XML.Nephele.Character" $
    [
      testCase "character" (parse character "test" "abc" ^? _Right @?= 'a' ^? character')
    , testCase "character'" (assertBool "isJust" (isJust ('a' ^? character')))
    , testCase "characters'" (pack "abc" ^? characters' @?= 'a' ^? character')
    ]

testCombiningCharacter ::
  TestTree
testCombiningCharacter =
  testGroup "Text.XML.Nephele.CombiningCharacter" $
    [
      testCase "combiningCharacter" (parse combiningCharacter "test" "\784bc" ^? _Right @?= '\784' ^? combiningCharacter')
    , testCase "combiningCharacter'" (assertBool "isJust" (isJust ('\784' ^? combiningCharacter')))
    , testCase "combiningCharacters'" (pack "\784bc" ^? combiningCharacters' @?= '\784' ^? combiningCharacter')
    ]

testComment ::
  TestTree
testComment =
  testGroup "Text.XML.Nephele.Comment" $
    [
      testCase "commentCharacters" (parse commentCharacters "test" "" ^? _Right @?= pack "" ^? comment')
    , testCase "commentCharacters" (parse commentCharacters "test" " a-bc " ^? _Right @?= pack " a-bc " ^? comment')
    , testCase "commentCharacters" (parse commentCharacters "test" " abc " ^? _Right @?= pack " abc " ^? comment')
    , testCase "commentCharacters" (parse commentCharacters "test" " a-b-c " ^? _Right @?= pack " a-b-c " ^? comment')
    , testCase "commentCharacters" (parse commentCharacters "test" " a-bb-cc " ^? _Right @?= pack " a-bb-cc " ^? comment')
    , testCase "commentCharacters" (parse commentCharacters "test" " abc- " ^? _Right @?= pack " abc- " ^? comment')
    , testCase "comment" (parse comment "test" "<!---->" ^? _Right @?= pack "" ^? comment')
    , testCase "comment" (parse comment "test" "<!-- abc -->" ^? _Right @?= pack " abc " ^? comment')
    , testCase "comment" (parse comment "test" "<!-- a-bc -->" ^? _Right @?= pack " a-bc " ^? comment')
    , testCase "comment" (parse comment "test" "<!-- a-b-c -->" ^? _Right @?= pack " a-b-c " ^? comment')
    , testCase "comment" (parse comment "test" "<!-- a-bb-cc -->" ^? _Right @?= pack " a-bb-cc " ^? comment')
    , testCase "comment" (parse comment "test" "<!-- abc- -->" ^? _Right @?= pack " abc- " ^? comment')
    , testCase "comment'" (assertBool "isJust" (isJust (pack "abc" ^? comment')))
    ]

testDigit ::
  TestTree
testDigit =
  testGroup "Text.XML.Nephele.Digit" $
    [
      testCase "digit" (parse digit "test" "5bc" ^? _Right @?= '5' ^? digit')
    , testCase "digit'" (assertBool "isJust" (isJust ('5' ^? digit')))
    , testCase "digit'" (pack "5bc" ^? digits' @?= '5' ^? digit')
    ]

testExtender ::
  TestTree
testExtender =
  testGroup "Text.XML.Nephele.Extender" $
    [
      testCase "extender" (parse extender "test" "\721bc" ^? _Right @?= '\721' ^? extender')
    , testCase "extender'" (assertBool "isJust" (isJust ('\721' ^? extender')))
    , testCase "extenders'" (pack "\721bc" ^? extenders' @?= '\721' ^? extender')
    ]

testIdeographic ::
  TestTree
testIdeographic =
  testGroup "Text.XML.Nephele.Ideographic" $
    [
      testCase "ideographic" (parse ideographic "test" "\20016bc" ^? _Right @?= '\20016' ^? ideographic')
    , testCase "ideographic'" (assertBool "isJust" (isJust ('\20016' ^? ideographic')))
    , testCase "ideographics" (pack "\20016bc" ^? ideographics' @?= '\20016' ^? ideographic')
    ]

testWhitespace ::
  TestTree
testWhitespace =
  testGroup "Text.XML.Nephele.testWhitespace" $
    [
      testCase "whitespace" (parse whitespace "test" " " @?= Right Whitespace.Space)
    , testCase "whitespace" (parse whitespace "test" "\t" @?= Right Whitespace.Tab)
    , testCase "whitespace" (parse whitespace "test" "\n\t" @?= Right Whitespace.LineFeed)
    , testCase "whitespace" (parse whitespace "test" "\r\n\t " @?= Right Whitespace.CarriageReturn)
    , testCase "whitespaces1" (parse whitespaces1 "test" " " @?= Right (Whitespaces1 (Whitespace.Space :| [])))
    , testCase "whitespaces1" (parse whitespaces1 "test" "    " @?= Right (Whitespaces1 (Whitespace.Space :| [Whitespace.Space,Whitespace.Space,Whitespace.Space])))
    , testCase "whitespaces1" (parse whitespaces1 "test" "    abc" @?= Right (Whitespaces1 (Whitespace.Space :| [Whitespace.Space,Whitespace.Space,Whitespace.Space])))
    , testCase "whitespaces1" (parse whitespaces1 "test" "  \t  \n " @?= Right (Whitespaces1 (Whitespace.Space :| [Whitespace.Space,Whitespace.Tab,Whitespace.Space,Whitespace.Space,Whitespace.LineFeed,Whitespace.Space])))
    , testCase "whitespace'" (' ' ^? whitespace' @?= Just Whitespace.Space)
    , testCase "whitespace'" ('\t' ^? whitespace' @?= Just Whitespace.Tab)
    , testCase "whitespace'" ('\r' ^? whitespace' @?= Just Whitespace.CarriageReturn)
    , testCase "whitespace'" ('\n' ^? whitespace' @?= Just Whitespace.LineFeed)
    , testCase "whitespaces'" (pack " " ^? whitespaces' @?= Just Whitespace.Space)
    , testCase "whitespaces'" (pack "\t" ^? whitespaces' @?= Just Whitespace.Tab)
    , testCase "whitespaces'" (pack "\r" ^? whitespaces' @?= Just Whitespace.CarriageReturn)
    , testCase "whitespaces'" (pack "\n" ^? whitespaces' @?= Just Whitespace.LineFeed)
    ]
