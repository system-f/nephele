{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Text(pack)
import Papa
import Test.Tasty(TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit(testCase, (@?=))
import Text.Parsec(parse)
import Text.XML.Nephele.BaseCharacter

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
    , testCase "baseCharacter'" ('a' ^? baseCharacter' @?= 'a' ^? baseCharacter')
    , testCase "baseCharacters'" (pack "abc" ^? baseCharacters' @?= 'a' ^? baseCharacter')
    ]
