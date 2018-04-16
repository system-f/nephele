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

main :: IO ()
main =
  defaultMain $
    testGroup "nephele"
      [
        testBaseCharacter
      ]

testBaseCharacter ::
  TestTree
testBaseCharacter =
  testGroup "Text.XML.Nephele.BaseCharacter" $
    [
      testCase "baseCharacter" ((parse baseCharacter "test" "abc" ^? _Right) @?= 'a' ^? baseCharacter')
    , testCase "baseCharacter'" (assertBool "isJust" (isJust ('a' ^? baseCharacter')))
    , testCase "baseCharacters'" (pack "abc" ^? baseCharacters' @?= 'a' ^? baseCharacter')
    ]

testCharacter ::
  TestTree
testCharacter =
  testGroup "Text.XML.Nephele.Character" $
    [
      testCase "character" ((parse character "test" "abc" ^? _Right) @?= 'a' ^? character')
    , testCase "character'" (assertBool "isJust" (isJust ('a' ^? character')))
    , testCase "characters'" (pack "abc" ^? characters' @?= 'a' ^? character')
    ]

testCombiningCharacter ::
  TestTree
testCombiningCharacter =
  testGroup "Text.XML.Nephele.CombiningCharacter" $
    [
      testCase "combiningCharacter" ((parse combiningCharacter "test" "\784bc" ^? _Right) @?= '\784' ^? combiningCharacter')
    , testCase "combiningCharacter'" (assertBool "isJust" (isJust ('\784' ^? combiningCharacter')))
    , testCase "combiningCharacters'" (pack "\784bc" ^? combiningCharacters' @?= '\784' ^? combiningCharacter')
    ]
