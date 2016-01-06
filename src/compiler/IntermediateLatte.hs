module IntermediateLatte where

import AbsLatte ()
import Data.Int (Int32)

type Label = String

data Var =
   Param Int
 | Local Int
  deriving (Eq,Ord,Show)

data Program =
   Program [Fun] [(Int, String)]
  deriving (Eq,Ord,Show)

data Fun = Fun Label Stmt Int -- locals limit
  deriving (Eq,Ord,Show)

data Stmt =
   Empty
 | BStmt [Stmt]
 | Decl [Item]
 | Ass Var Expr
 | Incr Var
 | Decr Var
 | Ret Expr
 | VRet
 | Cond Expr Stmt
 | CondElse Expr Stmt Stmt
 | While Expr Stmt
 | SExp Expr
  deriving (Eq,Ord,Show)


data Item =
   Item Var Expr
  deriving (Eq,Ord,Show)

data Expr =
   EVar Var
 | ELitInt Int32
 | ELitTrue
 | ELitFalse
 | EApp Label [Expr]
 | EString Int -- string's number
 | Neg Expr
 | Not Expr
 | EMul Expr MulOp Expr
 | EAdd Expr AddOp Expr
 | Cat Expr Expr
 | ERel Expr RelOp Expr
 | EAnd Expr Expr
 | EOr Expr Expr
  deriving (Eq,Ord,Show)

data AddOp =
   Plus
 | Minus
  deriving (Eq,Ord,Show)

data MulOp =
   Times
 | Div
 | Mod
  deriving (Eq,Ord,Show)

data RelOp =
   LTH
 | LE
 | GTH
 | GE
 | EQU
 | NE
  deriving (Eq,Ord,Show)
