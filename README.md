Nephele
=======

An XML parser and library that aims for reliability and rich libraries, potentially at the expense of performance.

Design Goals
------------

* Maintain independence of parser implementation by using the [parsers package](http://hackage.haskell.org/package/parsers).

* Preserve whitespace and other source tokens so that the original document may be recovered from the AST.

* Exploit [lens](http://hackage.haskell.org/package/lens) programming for library support.

* Implement a zipper to provide a codec, similar to the [argonaut](http://argonaut.io) library.

* Provide a compositional API for transforming the XML syntax tree to custom data types.

* Provide structured and detailed error messages.

