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

data SIYCImport
  = SIYCImport ClassName

data SIYCClass
  = SIYCClass ClassName [SIYCField] [SIYCConstructor] [SIYCMethod]

data SIYCField
  = SIYCField SIYCModifier Identifier

data SIYCConstructor
  = SIYCConstructor SIYCModifier ClassName [SIYCParameter] [SIYCStatement]

data SIYCMethod
  = SIYCMethod SIYCModifier TypeName Identifier [SIYCParameter] [SIYCStatement]

data SIYCParameter
  = SIYCParameter TypeName Identifier

data SIYCStatement
  = SIYCBlock [SIYCStatement]
  | SIYCEmpty
  | SIYCExpression SIYCExpression
  | SIYCFor SIYCExpression SIYCExpression SIYCExpression
  | SIYCIf SIYCExpression SIYCStatement (Maybe SIYCStatement)
  | SIYCReturn (Maybe SIYCExpression)
  | SIYCWhile SIYCExpression SIYCStatement

data SIYCExpression
  = SIYCAssignment SIYCExpression SIYCExpression
  | SIYCBoolean Bool
  | SIYCCall Identifier [SIYCExpression]
  | SIYCChar Char
  | SIYCDeclaration TypeName Identifier (Maybe SIYCExpression)
  | SIYCInfix SIYCExpression SIYCInfixOp SIYCExpression
  | SIYCNew TypeName [SIYCExpression]
  | SIYCPostfix SIYCExpression SIYCPostfixOp
  | SIYCPrefix SIYCPrefixOp SIYCExpression
  | SIYCString String
  | SIYCVar Identifier

data SIYCModifier
  = SIYCPrivate
  | SIYCPublic

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

data SIYCPrefixOp
  = SIYCNot
  | SIYCPreDecrement
  | SIYCPreIncrement
  | SIYCUnaryPlus
  | SIYCUnaryMinus

data SIYCPostfixOp
  = SIYCPostDecrement
  | SIYCPostIncrement
