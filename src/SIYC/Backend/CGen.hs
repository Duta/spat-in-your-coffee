module SIYC.Backend.CGen where

import Control.Applicative ((<$>))
import Control.Monad.State

import SIYC.Backend.CAST
import qualified SIYC.BuiltIn.PrimitiveTypes as P
import SIYC.Frontend.AST
import SIYC.Util

gen
  :: [SIYCClass]
  -> Either String [CFile]
gen
  = mapM gen'
  where
    gen'
      :: SIYCClass
      -> Either String CFile
    gen' (SIYCClass name fields constructors methods)
      = evalStateT (do
            classStruct <- makeClassStruct name fields
            constructorFunctions <- makeConstructors name constructors
            methodFunctions <- makeMethods name methods
            imports <- get
            return . CFile name imports classStruct $ constructorFunctions ++ methodFunctions)
          []

headerFile
  :: ClassName
  -> FilePath
headerFile
  = (++".h")

cType
  :: TypeName
  -> TypeName
cType "boolean"
  = "int"
cType t
  = t

makeClassStruct
  :: ClassName
  -> [SIYCField]
  -> StateT [CImport] (Either String) CStruct
makeClassStruct name fields
  = CStruct name <$> mapM makeClassStruct' fields
  where
    makeClassStruct'
      :: SIYCField
      -> StateT [CImport] (Either String) CStructField
    makeClassStruct' (SIYCField _ typeName var)
      = do
        unless (typeName `elem` P.names) $
          modify (CImport (headerFile typeName):)
        return $ CStructField (cType typeName) var

makeConstructors
  :: ClassName
  -> [SIYCConstructor]
  -> StateT [CImport] (Either String) [CFunction]
makeConstructors name constructors
  = return []

makeMethods
  :: ClassName
  -> [SIYCMethod]
  -> StateT [CImport] (Either String) [CFunction]
makeMethods name methods
  = return []
