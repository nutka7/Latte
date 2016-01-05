module IntermediateLatte where

import AbsLatte ()
import Data.Int (Int32)
import qualified Data.Map as M

type Local = Int
type Label = String

data Program =
   Program [Fun] (M.Map Int String)
  deriving (Eq,Ord,Show)

data Fun = Fun Label Stmt Int -- locals limit
  deriving (Eq,Ord,Show)

data Stmt =
   Empty
 | BStmt [Stmt]
 | Decl [Item]
 | Ass Local Expr
 | Incr Local
 | Decr Local
 | Ret Expr
 | VRet
 | Cond Expr Stmt
 | CondElse Expr Stmt Stmt
 | While Expr Stmt
 | SExp Expr
  deriving (Eq,Ord,Show)


data Item =
   Item Local Expr
  deriving (Eq,Ord,Show)

data Expr =
   EVar Local
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
 | B2I Expr
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
