module SIYC.Backend.CAST where

import SIYC.Util

data CFile
  = CFile ClassName [CImport] CStruct [CFunction]
    deriving (Show, Eq)

data CImport
  = CImport FilePath
    deriving (Show, Eq)

data CStruct
  = CStruct [CStructField]
    deriving (Show, Eq)

data CStructField
  = CStructField TypeName Identifier
    deriving (Show, Eq)

data CFunction
  = CFunction TypeName Identifier [CParameter] [CStatement]
    deriving (Show, Eq)

data CParameter
  = CParameter TypeName Identifier
    deriving (Show, Eq)

data CStatement
  = CBlock [CStatement]
  | CEmpty
  | CExpression CExpression
  | CFor CExpression CExpression CExpression CStatement
  | CIf CExpression CStatement (Maybe CStatement)
  | CReturn (Maybe CExpression)
  | CWhile CExpression CStatement
    deriving (Show, Eq)

data CExpression
  = CAccess CExpression CExpression
  | CAssignment CExpression CExpression
  | CCall CExpression [CExpression]
  | CChar Char
  | CDeclaration TypeName Identifier (Maybe CExpression)
  | CInfix CExpression CInfixOp CExpression
  | CPostfix CExpression CPostfixOp
  | CPrefix CPrefixOp CExpression
  | CString String
  | CVar Identifier
    deriving (Show, Eq)

data CInfixOp
  = CAnd
  | CDivide
  | CEqual
  | CGreater
  | CGreaterEqual
  | CLess
  | CLessEqual
  | CMinus
  | CModulus
  | CNotEqual
  | COr
  | CPlus
  | CTimes
    deriving (Show, Eq)

data CPrefixOp
  = CNot
  | CPreDecrement
  | CPreIncrement
  | CUnaryPlus
  | CUnaryMinus
    deriving (Show, Eq)

data CPostfixOp
  = CPostDecrement
  | CPostIncrement
    deriving (Show, Eq)
