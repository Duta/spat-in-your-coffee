module SIYC.Backend.CAST where

import SIYC.Util

data CFile
  = CFile ClassName [CInclude] [CDefine] CStruct [CFunction]
    deriving (Show, Eq)

type CInclude = FilePath

data CDefine
  = CDefine String String
    deriving (Show, Eq)

type CStruct = [CStructField]

data CStructField
  = CStructField TypeName Identifier
    deriving (Show, Eq)

data CFunction
  = CFunction CTypeSig [CStatement]
    deriving (Show, Eq)

data CTypeSig
  = CTypeSig TypeName Identifier [CParameter]

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
  = CMemAccess CExpression CExpression
  | CPtrAccess CExpression CExpression
  | CAssignment CExpression CExpression
  | CCall CExpression [CExpression]
  | CChar Char
  | CDeclaration TypeName Identifier (Maybe CExpression)
  | CInfix CExpression CInfixOp CExpression
  | CInt Int
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
