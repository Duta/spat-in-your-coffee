module SIYC.Frontend.AST
( SIYCFile(..)
, SIYCImport(..)
, SIYCClass(..)
, SIYCField(..)
, SIYCConstructor(..)
, SIYCMethod(..)
, SIYCParameter(..)
, SIYCStatement(..)
, SIYCExpression(..)
, SIYCModifier(..)
, SIYCInfixOp(..)
, SIYCPrefixOp(..)
, SIYCPostfixOp(..)
) where

import SIYC.Util

data SIYCFile
  = SIYCFile [SIYCImport] SIYCClass
    deriving Show

data SIYCImport
  = SIYCImport ClassName
    deriving Show

data SIYCClass
  = SIYCClass ClassName [SIYCField] [SIYCConstructor] [SIYCMethod]
    deriving Show

data SIYCField
  = SIYCField SIYCModifier TypeName Identifier
    deriving Show

data SIYCConstructor
  = SIYCConstructor SIYCModifier ClassName [SIYCParameter] [SIYCStatement]
    deriving Show

data SIYCMethod
  = SIYCMethod SIYCModifier TypeName Identifier [SIYCParameter] [SIYCStatement]
    deriving Show

data SIYCParameter
  = SIYCParameter TypeName Identifier
    deriving Show

data SIYCStatement
  = SIYCBlock [SIYCStatement]
  | SIYCEmpty
  | SIYCExpression SIYCExpression
  | SIYCFor SIYCExpression SIYCExpression SIYCExpression SIYCStatement
  | SIYCIf SIYCExpression SIYCStatement (Maybe SIYCStatement)
  | SIYCReturn (Maybe SIYCExpression)
  | SIYCWhile SIYCExpression SIYCStatement
    deriving Show

data SIYCExpression
  = SIYCAccess SIYCExpression SIYCExpression
  | SIYCAssignment SIYCExpression SIYCExpression
  | SIYCBoolean Bool
  | SIYCCall Identifier [SIYCExpression]
  | SIYCChar Char
  | SIYCDeclaration TypeName Identifier (Maybe SIYCExpression)
  | SIYCInfix SIYCExpression SIYCInfixOp SIYCExpression
  | SIYCNew ClassName [SIYCExpression]
  | SIYCPostfix SIYCExpression SIYCPostfixOp
  | SIYCPrefix SIYCPrefixOp SIYCExpression
  | SIYCString String
  | SIYCVar Identifier
    deriving Show

data SIYCModifier
  = SIYCPrivate
  | SIYCPublic
    deriving Show

data SIYCInfixOp
  = SIYCAnd
  | SIYCDivide
  | SIYCEqual
  | SIYCGreater
  | SIYCGreaterEqual
  | SIYCLess
  | SIYCLessEqual
  | SIYCMinus
  | SIYCModulus
  | SIYCNotEqual
  | SIYCOr
  | SIYCPlus
  | SIYCTimes
    deriving Show

data SIYCPrefixOp
  = SIYCNot
  | SIYCPreDecrement
  | SIYCPreIncrement
  | SIYCUnaryPlus
  | SIYCUnaryMinus
    deriving Show

data SIYCPostfixOp
  = SIYCPostDecrement
  | SIYCPostIncrement
    deriving Show
