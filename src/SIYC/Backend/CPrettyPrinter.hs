module SIYC.Backend.CPrettyPrinter where

import SIYC.Backend.CAST
import SIYC.Util

import Data.Char (toUpper)
import Data.List (intercalate)

pp
  :: CFile
  -> [(FilePath, String)]
pp file
  = [headerFile file, sourceFile file]

tab
  :: String
tab
  = "  "

params
  :: [CParameter]
  -> String
params ps
  = "(" ++ intercalate ", " (map (\(CParameter t v) -> concat [t, " ", v]) ps) ++ ")"

include
  :: CInclude
  -> String
include
  = ("#include \"" ++) . (++ ".h\"")

define
  :: CDefine
  -> String
define (CDefine name val)
  = intercalate " " ["#define", name, val]

headerFile
  :: CFile
  -> (FilePath, String)
headerFile (CFile c includes defines struct functions)
  = (c ++ ".h",  file)
  where
    file = intercalate "\n" $ concat
      [ ["#ifndef " ++ map toUpper c ++ "_H"]
      , ["#define " ++ map toUpper c ++ "_H"]
      , [""]
      , map include includes
      , [""]
      , map define defines
      , [""]
      , struct'
      , [""]
      , funcSigs
      , [""]
      , ["#endif"]
      ]
    struct' =
      [ concat ["typedef struct ", c, " {"]
      ] ++ map
        (\(CStructField t v) ->
          concat [tab, t, " ", v, ";"])
        struct ++
      [ concat ["} ", c, ";"]
      ]
    funcSigs = map
      (\(CFunction r f ps ss) ->
        concat [r, " ", f] ++ params ps ++ ";")
      functions

sourceFile
  :: CFile
  -> (FilePath, String)
sourceFile (CFile c includes defines struct functions)
  = (c ++ ".c", file)
  where
    file = intercalate "\n" $ concat
      [ [include c]
      , [""]
      , functions'
      ]
    functions' = intercalate [""] $ map
      (\(CFunction r f ps ss) ->
        [ concat [r, " ", f] ++ params ps ++ " {"
        ] ++ map (intercalate "\n" . map (tab++) . stat) ss ++
        [ "}"
        ])
      functions

stat
  :: CStatement
  -> [String]
stat (CBlock stats)
  = concatMap stat stats
stat CEmpty
  = [";"]
stat (CExpression e)
  = [expr e ++ ";"]
stat (CFor init cond inc st)
  = []
stat (CIf cond s1 s2)
  = [concat ["if(", expr cond, ") {"]]
    ++ map (tab++) (stat s1)
    ++ maybe
      ["}"]
      (\s2' ->
        ["} else {"]
        ++ map (tab++) (stat s2') ++
        ["}"])
      s2
stat (CReturn e)
  = ["return" ++ maybe "" ((" "++) . expr) e ++ ";"]
stat (CWhile cond st)
  = [concat ["while(", expr cond, ") {"]]
    ++ map (tab++) (stat st) ++
    ["}"]

expr
  :: CExpression
  -> String
expr (CMemAccess v f)
  = concat [expr v, ".", expr f]
expr (CPtrAccess v f)
  = concat [expr v, "->", expr f]
expr (CAssignment v e)
  = concat [expr v, " = (", expr e, ")"]
expr (CCall f args)
  = expr f ++ "(" ++ intercalate ", " (map expr args) ++ ")"
expr (CChar c)
  = show c
expr (CDeclaration t v e)
  = concat $ [t, " ", v] ++ maybe [] (\e' -> [" = (", expr e', ")"]) e
expr (CInfix e1 op e2)
  = concat ["(", expr e1, ") ", inOp op, " (", expr e2, ")"]
expr (CPostfix e op)
  = concat ["(", expr e, ") ", poOp op]
expr (CPrefix op e)
  = concat [prOp op, " (", expr e, ")"]
expr (CString s)
  = show s
expr (CVar v)
  = v

inOp
  :: CInfixOp
  -> String
inOp op = case op of
  CAnd          -> "&&"
  CDivide       -> "/"
  CEqual        -> "=="
  CGreater      -> ">"
  CGreaterEqual -> ">="
  CLess         -> "<"
  CLessEqual    -> "<="
  CMinus        -> "-"
  CModulus      -> "%"
  CNotEqual     -> "!="
  COr           -> "||"
  CPlus         -> "+"
  CTimes        -> "*"

poOp
  :: CPostfixOp
  -> String
poOp op = case op of
  CPostDecrement -> "--"
  CPostIncrement -> "++"

prOp
  :: CPrefixOp
  -> String
prOp op = case op of
  CNot -> "!"
  CPreDecrement -> "--"
  CPreIncrement -> "++"
  CUnaryPlus    -> "+"
  CUnaryMinus   -> "-"
