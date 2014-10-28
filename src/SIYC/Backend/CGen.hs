module SIYC.Backend.CGen
( gen
) where

import SIYC.Backend.CAST
import qualified SIYC.BuiltIn.PrimitiveTypes as P
import SIYC.Frontend.AST
import SIYC.Util

import Control.Applicative ((<$>))
import Control.Monad.State

gen
  :: SIYCFile
  -> CFile
gen (SIYCFile imports (SIYCClass name fields constructors methods))
  = CFile name imports' struct $ freeFunction : constructorFunctions ++ methodFunctions
  where
    imports' = map
      (\(SIYCImport siycClass) ->
        CImport $ siycClass ++ ".h")
      imports
    struct = CStruct $ map
      (\(SIYCField _ t v) ->
        CStructField (cType t) v)
      fields
    freeFunction = CFunction "void" ("free_" ++ name) [CParameter (ptr name) "this"]
      [ CIf (CVar "this")
          (CExpression $ CCall (CVar "free") [CVar "this"])
          Nothing
      ]
    constructorFunctions = map
      (\(SIYCConstructor _ _ ps ss) ->
        CFunction (ptr name) ("new_" ++ name) (map param ps) (map (stat name) ss))
      constructors
    methodFunctions = map
      (\(SIYCMethod _ r f ps ss) ->
        CFunction r (name ++ "_" ++ f) (CParameter (cType name) "this" : map param ps) (map (stat name) ss))
      methods

cType
  :: TypeName
  -> TypeName
cType "boolean"
  = "int"
cType t
  | t `elem` P.names = t
  | otherwise        = ptr t

ptr
  :: TypeName
  -> TypeName
ptr
  = (++" *")

param
  :: SIYCParameter
  -> CParameter
param (SIYCParameter t v)
  = CParameter (cType t) v

stat
  :: ClassName
  -> SIYCStatement
  -> CStatement
stat name (SIYCBlock stats)
  = CBlock $ map (stat name) stats
stat name SIYCEmpty
  = CEmpty
stat name (SIYCExpression e)
  = CExpression $ expr name e
stat name (SIYCFor init cond inc st)
  = CFor (expr name init) (expr name cond) (expr name inc) (stat name st)
stat name (SIYCIf cond s1 s2)
  = CIf (expr name cond) (stat name s1) (stat name <$> s2)
stat name (SIYCReturn e)
  = CReturn $ expr name <$> e
stat name (SIYCWhile cond st)
  = CWhile (expr name cond) (stat name st)

expr
  :: ClassName
  -> SIYCExpression
  -> CExpression
expr name (SIYCAssignment v e)
  = CAssignment (expr name v) (expr name e)
expr name (SIYCBoolean b)
  = error "expr name"
expr name (SIYCCall v args)
  = CCall (CVar $ name ++ "_" ++ v) $ CVar "this" : map (expr name) args
expr name (SIYCChar c)
  = CChar c
expr name (SIYCDeclaration t v e)
  = CDeclaration (cType t) v $ expr name <$> e
expr name (SIYCInfix e1 op e2)
  = CInfix (expr name e1) (inOp op) (expr name e2)
expr name (SIYCNew c args)
  = CCall (CVar $ "new_" ++ c) $ map (expr name) args
expr name (SIYCPostfix e op)
  = CPostfix (expr name e) (poOp op)
expr name (SIYCPrefix op e)
  = CPrefix (prOp op) (expr name e)
expr name (SIYCString s)
  = CString s
expr name (SIYCVar v)
  = CVar v

inOp
  :: SIYCInfixOp
  -> CInfixOp
inOp op = case op of
  SIYCAnd -> CAnd
  SIYCDivide -> CDivide
  SIYCEqual -> CEqual
  SIYCGreater -> CGreater
  SIYCGreaterEqual -> CGreaterEqual
  SIYCLess -> CLess
  SIYCLessEqual -> CLessEqual
  SIYCMinus -> CMinus
  SIYCModulus -> CModulus
  SIYCNotEqual -> CNotEqual
  SIYCOr -> COr
  SIYCPlus -> CPlus
  SIYCTimes -> CTimes

poOp
  :: SIYCPostfixOp
  -> CPostfixOp
poOp op = case op of
  SIYCPostDecrement -> CPostDecrement
  SIYCPostIncrement -> CPostIncrement

prOp
  :: SIYCPrefixOp
  -> CPrefixOp
prOp op = case op of
  SIYCNot -> CNot
  SIYCPreDecrement -> CPreDecrement
  SIYCPreIncrement -> CPreIncrement
  SIYCUnaryPlus -> CUnaryPlus
  SIYCUnaryMinus -> CUnaryMinus
