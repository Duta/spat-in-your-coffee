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
gen (SIYCFile imports (SIYCClass c fields constructors methods))
  = CFile c imports' defines struct functions 
  where
    superclass = "Object"

    imports' = map
      (\(SIYCImport siycClass) -> siycClass)
      imports

    struct = map
      (\(SIYCField _ t v) ->
        CStructField (cType t) v)
      fields

    functions = concat [news, [init], [free], methods']

    (defines, news) = error "defines"

    init = CFunction "void" ("init_" ++ c) [CParameter (ptr "void") "_this"] $ concat
      [ [CExpression . CDeclaration (ptr c) "this" . Just $ CVar "_this"]
      , [CExpression . CAssignment (CPtrAccess (CVar "this") (CVar "super")) . CVar $ superclass ++ "_base"]
      , map
          (\v ->
            CExpression . CAssignment (CPtrAccess (CVar "this") (CVar v)) . CVar $ concat [c, "_", v])
          methodNames
      , [CReturn . Just $ CInt 0]
      ]

    free = CFunction "void" ("free_" ++ c) [CParameter (ptr "void") "_this"]
      [ CExpression . CDeclaration (ptr c) "this" . Just $ CVar "_this"
      , CExpression . CPtrAccess (CVar "this") . CMemAccess (CVar "super") $ CCall (CVar "free") [CVar "_this"]
      ]

    methods' = map
      (\(SIYCMethod _ r f ps ss) ->
        CFunction (CTypeSig r (c ++ "_" ++ f) (CParameter (ptr $ cType c) "this" : map param ps)) (map (stat c) ss))
      methods

    methodNames = map (\(SIYCMethod _ _ v _ _) -> v) methods

{-
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
-}
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
  = CExpression $ expr name [] e
stat name (SIYCFor init cond inc st)
  = CFor (expr name [] init) (expr name [] cond) (expr name [] inc) (stat name st)
stat name (SIYCIf cond s1 s2)
  = CIf (expr name [] cond) (stat name s1) (stat name <$> s2)
stat name (SIYCReturn e)
  = CReturn $ expr name [] <$> e
stat name (SIYCWhile cond st)
  = CWhile (expr name [] cond) (stat name st)

expr
  :: ClassName
  -> [CExpression]
  -> SIYCExpression
  -> CExpression
expr name ctx (SIYCAccess v f)
  = expr name (ctx ++ [expr name ctx v]) f
expr name ctx (SIYCAssignment v e)
  = CAssignment (withCtx ctx $ expr name [] v) (expr name [] e)
expr name ctx (SIYCBoolean b)
  = error "expr"
expr name ctx (SIYCCall v args)
  = CCall (CVar $ name ++ "_" ++ v) $ ctx' : map (expr name []) args
  where
    ctx' = case reverse ctx of
      []     -> CVar "this"
      (v:fs) -> withCtx (reverse fs) v
expr name ctx (SIYCChar c)
  = withCtx ctx $ CChar c
expr name ctx (SIYCDeclaration t v e)
  = withCtx ctx $ CDeclaration (cType t) v $ expr name [] <$> e
expr name ctx (SIYCInfix e1 op e2)
  = withCtx ctx $ CInfix (expr name [] e1) (inOp op) (expr name [] e2)
expr name ctx (SIYCNew c args)
  = withCtx ctx $ CCall (CVar $ "new_" ++ c) $ map (expr name []) args
expr name ctx (SIYCPostfix e op)
  = withCtx ctx $ CPostfix (expr name [] e) (poOp op)
expr name ctx (SIYCPrefix op e)
  = withCtx ctx $ CPrefix (prOp op) (expr name [] e)
expr name ctx (SIYCString s)
  = withCtx ctx $ CString s
expr name ctx (SIYCVar v)
  = withCtx ctx $ CVar v


withCtx
  :: [CExpression]
  -> CExpression
  -> CExpression
withCtx [] e
  = e
withCtx ctx e
  = CPtrAccess (withCtx ctx' f) e
  where
    ctx' = init ctx
    f    = last ctx

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
