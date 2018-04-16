{ mkDerivation, base, checkers, lens, parsec, parsers, QuickCheck
, semigroups, stdenv, tasty, tasty-hunit, tasty-quickcheck, text
, transformers
}:
mkDerivation {
  pname = "nephele";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base lens parsec parsers semigroups text transformers
  ];
  testHaskellDepends = [
    base checkers lens QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/qfpl/nephele";
  description = "General purpose XML parser and library";
  license = stdenv.lib.licenses.bsd3;
}
