# TinyParseJSON

This project had the following objectives
1. To understand the internals of a Typical Monadic Parsing library such as Parsec etc, by implementing a basic                 lightweight version from scratch. As such this parser supports Failure, due to the use of the Maybe monad, but
cannot report why the error happened. This is possible by wrapping the output of parser in an Either Monad instead
2. To get hands on practice at implementing a given grammar using parser libraries. For this the JSON format was used.
3. To further implement a basic module that allows user to read and work with values parsed using the above parser.
4. To learn how to package haskell programs using the cabal build system.

With that introduction out of the way,

## TinyParse
**Reference Material** [Functional Pearls: Monadic Parsing in Haskell, Graham Hutton and Erik Meijer](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=2ahUKEwjU_cbsnZ_pAhUUXn0KHTdXAbQQFjAAegQIBBAB&url=https%3A%2F%2Fwww.cs.nott.ac.uk%2F~pszgmh%2Fpearl.pdf&usg=AOvVaw0XDgR2I8LYw5pi8vZTV2B6)

This Module, implements most of the functions described in the above paper, however as an exercise a different Monad than List is used, which is Maybe. Some of the functions such as many, <|>, some didn't need to be implemented as they already exist as a part of the Alternative typeclass.
The unexpected advantage of using Maybe was that instances for it exist in Alternative from beforehand, so making new instances for Parser was simpler

## JSON
**Reference Material** [JSON Language Definition](https://www.json.org/json-en.html)

The implementation structure closely matches that showsn in the Table on the right side of page.
The parsed values are stored internally with value constructors to be able to implement functions that work only 
with JSON data and don't change them, Although no such functions are implemented.

The available useful functions are
```parsePrintFile :: String -> (IO (Doc ann)``` which prints out the formatted contents of a json file after first read and building an internal representation of them. (No practical use except in ghci, to check structure of json file while writing functions for it).

```loadJsonFromFile :: String -> (IO JsonVal)``` which returns JSON Object in the form of a Dictionary using Data.Map
The json can now be worked with and treated as a dictionary. (This is also usable in ghci, to play around with the object and figure out how to write programs for them)

```writeJsonToFile :: FilePath -> JsonVal -> IO ()``` which writes JSON Object out to file by taking path as argument. 

```getJsonVal :: String -> JsonVal``` which takes string as input parses and returns a JSON value (Non IO).

```pretty :: JsonVal -> Doc ann``` which provides well structured, and readable output to Json Values in REPL and can subsequently be used with ```print```. This is an instance of the Pretty typeclass for JsonVal, therefore introduces the external dependency "prettyprinter".

**This is NOT a complete implementation of JSON.**
No attention was paid to unicode in the slightest, and certain other features were omitted.

## Installation
```
cabal configure
cabal build
cabal install
```
And then can be used in ghci or other haskell programs by 

```hs
import TinyParse
import JSON
```
