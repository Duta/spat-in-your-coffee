module SIYC.ASTChecks.NoUnknownClasses
( findUnknownClasses
) where

import qualified SIYC.BuiltIn.PrimitiveTypes as Primitives
import SIYC.Frontend.AST
import SIYC.Util

import Control.Applicative ((<$>))
import Data.List (nub)

findUnknownClasses
  :: [SIYCFile]
  -> [ClassName]
findUnknownClasses cs
  = nub $ concatMap checkClass cs
  where
    knownClasses
      :: [ClassName]
    knownClasses
      = map (\(SIYCFile _ (SIYCClass name _ _ _)) -> name) cs
    known
      :: TypeName
      -> Bool
    known name
      = any (name `elem`) [Primitives.names, knownClasses]
    check
      :: TypeName
      -> [TypeName]
    check name
      = if known name then [] else [name]
    checkClass
      :: SIYCFile
      -> [ClassName]
    checkClass (SIYCFile _ (SIYCClass _ fields constructors methods))
      = concat
      [ concatMap checkField fields
      , concatMap checkConstructor constructors
      , concatMap checkMethod methods
      ]
    checkField
      :: SIYCField
      -> [ClassName]
    checkField (SIYCField _ name _)
      = check name
    checkConstructor
      :: SIYCConstructor
      -> [ClassName]
    checkConstructor (SIYCConstructor _ name params statements)
      = concat
      [ check name
      , concatMap checkParam params
      , concatMap checkStatement statements
      ]
    checkMethod
      :: SIYCMethod
      -> [ClassName]
    checkMethod (SIYCMethod _ name _ params statements)
      = concat
      [ check name
      , concatMap checkParam params
      , concatMap checkStatement statements
      ]
    checkParam
      :: SIYCParameter
      -> [ClassName]
    checkParam (SIYCParameter name _)
      = check name
    checkStatement
      :: SIYCStatement
      -> [ClassName]
    checkStatement (SIYCBlock statements)
      = concatMap checkStatement statements
    checkStatement (SIYCEmpty)
      = []
    checkStatement (SIYCExpression expr)
      = checkExpression expr
    checkStatement (SIYCFor init cond inc stmt)
      = concat
      [ checkExpression init
      , checkExpression cond
      , checkExpression inc
      , checkStatement stmt
      ]
    checkStatement (SIYCIf cond s1 s2)
      = concat
      [ checkExpression cond
      , checkStatement s1
      , maybe [] checkStatement s2
      ]
    checkStatement (SIYCReturn expr)
      = maybe [] checkExpression expr
    checkStatement (SIYCWhile cond stmt)
      = concat
      [ checkExpression cond
      , checkStatement stmt
      ]
    checkExpression
      :: SIYCExpression
      -> [ClassName]
    checkExpression (SIYCAccess e1 e2)
      = concatMap checkExpression [e1, e2]
    checkExpression (SIYCAssignment e1 e2)
      = concatMap checkExpression [e1, e2]
    checkExpression (SIYCCall _ args)
      = concatMap checkExpression args
    checkExpression (SIYCDeclaration name _ expr)
      = concat
      [ check name
      , maybe [] checkExpression expr
      ]
    checkExpression (SIYCInfix e1 _ e2)
      = concatMap checkExpression [e1, e2]
    checkExpression (SIYCNew name args)
      = concat
      [ check name
      , concatMap checkExpression args
      ]
    checkExpression (SIYCPostfix expr _)
      = checkExpression expr
    checkExpression (SIYCPrefix _ expr)
      = checkExpression expr
    checkExpression _
      = []
