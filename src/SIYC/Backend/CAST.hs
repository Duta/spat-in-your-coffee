module SIYC.Backend.CAST where

import SIYC.Util

data CFile
  = CFile String [CImport] CStruct [CFunction]
    deriving (Show, Eq)

data CImport
  = CImport FilePath
    deriving (Show, Eq)

data CStruct
  = CStruct Identifier [CStructField]
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
  = TODO
    deriving (Show, Eq)
