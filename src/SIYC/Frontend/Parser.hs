module SIYC.Frontend.Parser
( parse
) where

import SIYC.Frontend.AST
import SIYC.Util

import Control.Applicative hiding (many)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T

siycParse
  :: SourceName
  -> SourceCode
  -> Either ParseError SIYCFile
siycParse name code
  = parse siycParse' name code
  where
    siycParse'
      :: Parser SIYCFile
    siycParse'
      = do
        whiteSpace
        file <- siycFile
        eof
        return file

siycLanguage
  :: LanguageDef st
siycLanguage
  = emptyDef
  { T.commentStart    = "/*"
  , T.commentEnd      = "*/"
  , T.commentLine     = "//"
  , T.opStart         = opLetter siycLanguage
  , T.opLetter        = oneOf "+-*/!%<>=&|"
  , T.reservedNames   =
    [ "boolean"
    , "char"
    , "class"
    , "double"
    , "else"
    , "for"
    , "if"
    , "import"
    , "int"
    , "new"
    , "private"
    , "public"
    , "return"
    , "void"
    , "while"
    ]
  , T.reservedOpNames =
    [ "&&"
    , "/"
    , "=="
    , ">"
    , ">="
    , "<"
    , "<="
    , "-"
    , "%"
    , "!="
    , "||"
    , "+"
    , "*"
    , "!"
    , "--"
    , "++"
    ]
  }

siycLexer
  :: T.TokenParser st
siycLexer
  = T.makeTokenParser siycLanguage

identifier
  :: Parser Identifier
identifier
  = T.identifier siycLexer

reserved
  :: String -> Parser ()
reserved
  = T.reserved siycLexer

reservedOp
  :: String -> Parser ()
reservedOp
  = T.reservedOp siycLexer

parens
  :: Parser a -> Parser a
parens
  = T.parens siycLexer

integer
  :: Parser Integer
integer
  = T.integer siycLexer

stringLiteral
  :: Parser String
stringLiteral
  = T.stringLiteral siycLexer

semi
  :: Parser String
semi
  = T.semi siycLexer

braces
  :: Parser a -> Parser a
braces
  = T.braces siycLexer

whiteSpace
  :: Parser ()
whiteSpace
  = T.whiteSpace siycLexer

siycFile
  :: Parser SIYCFile
siycFile
  = SIYCFile <$> many siycImport <*> siycClass

siycImport
  :: Parser SIYCImport
siycImport
  = error "siycImport"

siycClass
  :: Parser SIYCClass
siycClass
  = error "siycClass"
