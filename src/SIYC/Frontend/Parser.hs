module SIYC.Frontend.Parser
( parse
) where

import SIYC.Frontend.AST
import SIYC.Util

import Control.Applicative ((<$>), (<*>))
import Control.Lens
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

commaSep
  :: Parser a -> Parser [a]
commaSep
  = T.commaSep siycLexer

whiteSpace
  :: Parser ()
whiteSpace
  = T.whiteSpace siycLexer

reserved'
  :: String -> Parser String
reserved' s
  = T.reserved siycLexer s >> return s

params
  :: Parser [SIYCParameter]
params
  = parens $ commaSep siycParameter

bracedStatements
  :: Parser [SIYCStatement]
bracedStatements
  = braces $ many siycStatement

typeName
  :: Parser TypeName
typeName
  = reserved' "boolean"
 <|>reserved' "char"
 <|>reserved' "double"
 <|>reserved' "int"
 <|>reserved' "void"
 <|>identifier

siycFile
  :: Parser SIYCFile
siycFile
  = SIYCFile <$> many siycImport <*> siycClass

siycImport
  :: Parser SIYCImport
siycImport
  = do
    reserved "import"
    name <- identifier
    semi
    return $ SIYCImport name

siycClass
  :: Parser SIYCClass
siycClass
  = do
    reserved "class"
    name <- identifier
    (fields, constructors, methods) <- braces classContents
    return $ SIYCClass name fields constructors methods
  where
    classContents
      :: Parser ([SIYCField], [SIYCConstructor], [SIYCMethod])
    classContents
      = (siycField       >>= \f -> classContents >>= return . (_1 %~ (f:)))
     <|>(siycConstructor >>= \c -> classContents >>= return . (_2 %~ (c:)))
     <|>(siycMethod      >>= \m -> classContents >>= return . (_3 %~ (m:)))
     <|>(eof >> return ([], [], []))

siycField
  :: Parser SIYCField
siycField
  = do
    field <- SIYCField <$> siycModifier <*> identifier
    semi
    return field

siycConstructor
  :: Parser SIYCConstructor
siycConstructor
  = SIYCConstructor <$> siycModifier <*> identifier <*> params <*> bracedStatements

siycMethod
  :: Parser SIYCMethod
siycMethod
  = SIYCMethod <$> siycModifier <*> typeName <*> identifier <*> params <*> bracedStatements

siycModifier
  :: Parser SIYCModifier
siycModifier
  = (reserved "private" >> return SIYCPrivate)
 <|>(reserved "public"  >> return SIYCPublic)

siycParameter
  :: Parser SIYCParameter
siycParameter
  = SIYCParameter <$> typeName <*> identifier

siycStatement
  :: Parser SIYCStatement
siycStatement
  = let
      siycBlock
        = SIYCBlock <$> bracedStatements
      siycEmpty
        = return SIYCEmpty :: Parser SIYCStatement
      siycExpression'
        = SIYCExpression <$> siycExpression
      siycFor
        = do
          reserved "for"
          (init, cond, inc) <- parens $ do
            init <- siycExpression
            semi
            cond <- siycExpression
            semi
            inc <- siycExpression
            return (init, cond, inc)
          SIYCFor init cond inc <$> siycStatement
      siycIf
        = do
          reserved "if"
          cond <- parens siycExpression
          s1 <- siycStatement
          s2 <- optionMaybe $ do
            reserved "else"
            siycStatement
          return $ SIYCIf cond s1 s2
      siycReturn
        = do
          reserved "return"
          expr <- optionMaybe siycExpression
          semi
          return $ SIYCReturn expr
      siycWhile
        = do
          reserved "while"
          SIYCWhile <$> parens siycExpression <*> siycStatement
    in error "SIYC.Frontend.Parser.siycStatement"

siycExpression
  :: Parser SIYCExpression
siycExpression
  = error "SIYC.Frontend.Parser.siycExpression"
