{ mkDerivation, base, checkers, hedgehog, lens, papa, parsec
, parsers, QuickCheck, semigroups, stdenv, tasty, tasty-hedgehog
, tasty-hunit, tasty-quickcheck, text, text1, transformers
}:
mkDerivation {
  pname = "nephele";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base lens papa parsec parsers semigroups text text1 transformers
  ];
  testHaskellDepends = [
    base checkers hedgehog lens papa parsec parsers QuickCheck tasty
    tasty-hedgehog tasty-hunit tasty-quickcheck text text1
  ];
  homepage = "https://github.com/qfpl/nephele";
  description = "General purpose XML parser and library";
  license = stdenv.lib.licenses.bsd3;
}
