{-# LANGUAGE TupleSections #-}

module ToIntermediate (simplify) where

import qualified AbsLatte as A
import qualified IntermediateLatte as I
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Control.Applicative


fromIdent :: A.Ident -> String
fromIdent (A.Ident name) = name

-- Monad for simplifying statements

type VarPositions = M.Map A.Ident I.Var    -- position relative to EBP
type VarBindings = M.Map A.Ident A.Type
type FunBindings = M.Map A.Ident A.Type
type StrLabels = M.Map Int String

type StmEnv = FunBindings
data StmState = StmState { varT :: VarBindings,
                           varP :: VarPositions,
                           lastNr :: Int,
                           maxNr :: Int,
                           strings :: StrLabels }

type StmM = ReaderT StmEnv (StateT StmState Identity)

runStmM :: StmM a -> StmEnv -> StmState -> (a, StmState)
runStmM m r s = runIdentity $ runStateT (runReaderT m r) s

--

simplify :: A.Program -> I.Program
simplify (A.Program topdefs) =
    let funs = M.fromList $ map funRType topdefs
        m = mapM (state . simpFun funs) topdefs
        initSt = M.singleton 0 ""
        (funs', strLabels) = runState m initSt
    in I.Program funs' (M.toList strLabels)

    where
        funRType (A.FnDef t ident _ _) = (ident, t)


simpFun :: FunBindings -> A.TopDef -> StrLabels -> (I.Fun, StrLabels)
simpFun funs (A.FnDef _ ident args block) strLabels =
    let args' = map extractArg args
        st = StmState {
            varT = M.fromList args',
            varP = M.fromList $ zip (map fst args') (map I.Param [1..]),
            lastNr = 0,
            maxNr = 0,
            strings = strLabels }
        m = simpStm (A.BStmt block)
        (block', st') = runStmM m funs st
        fun' = I.Fun (fromIdent ident) block' (maxNr st')
    in (fun', strings st')
    where
        extractArg (A.Arg t iden) = (iden, t)


allocVar :: A.Ident -> A.Type -> StmM I.Var
allocVar ident t = do
    st <- get
    let next = lastNr st + 1
    put $ st { lastNr = next,
               maxNr = max next (maxNr st),
               varP = M.insert ident (I.Local next) (varP st),
               varT = M.insert ident t (varT st)
          }
    return (I.Local next)

getPos  :: A.Ident -> StmM I.Var
getPos  ident = gets (fromJust . M.lookup ident . varP)

getType :: A.Ident -> StmM A.Type
getType ident = gets (fromJust . M.lookup ident . varT)

data TCode = I | S | B deriving (Eq,Ord,Show)

getTCode :: A.Type -> TCode
getTCode t =
    case t of
        A.Int  -> I
        A.Str  -> S
        A.Bool -> B

defaults :: [(TCode, I.Expr)]
defaults = [
    (I, I.ELitInt 0),
    (S, I.EString 0),
    (B, I.ELitFalse)]

simpItem :: A.Type -> A.Item -> StmM I.Item
simpItem t (A.NoInit ident) =
    I.Item <$> allocVar ident t
           <*> pure (fromJust (lookup (getTCode t) defaults))

simpItem t (A.Init ident e) = -- order is very important
    flip I.Item <$> simpExp' e <*> allocVar ident t

simpStm :: A.Stmt -> StmM I.Stmt

simpStm (A.BStmt (A.Block stms)) = do
    st <- get
    stms' <- mapM simpStm stms
    st' <- get
    put $ st { maxNr = maxNr st', strings = strings st' }
    return (I.BStmt stms')

simpStm A.Empty = return I.Empty

simpStm (A.Decl t items) = I.Decl <$> mapM (simpItem t) items

simpStm (A.Ass ident e) = I.Ass <$> getPos ident <*> simpExp' e

simpStm (A.Incr ident) = I.Incr <$> getPos ident

simpStm (A.Decr ident) = I.Decr <$> getPos ident

simpStm (A.Ret e) = I.Ret <$> simpExp' e

simpStm A.VRet = return I.VRet

simpStm (A.Cond e stm) = I.Cond <$> simpExp' e <*> simpStm stm

simpStm (A.CondElse e stmT stmF) =
    I.CondElse <$> simpExp' e <*> simpStm stmT <*> simpStm stmF

simpStm (A.While e stm) = I.While <$> simpExp' e <*> simpStm stm

simpStm (A.SExp e) = I.SExp <$> simpExp' e


trAddOp :: A.AddOp -> I.AddOp
trAddOp op = case op of
                A.Plus  -> I.Plus
                A.Minus -> I.Minus

trMulOp :: A.MulOp -> I.MulOp
trMulOp op = case op of
                A.Times -> I.Times
                A.Div   -> I.Div
                A.Mod   -> I.Mod

trRelOp :: A.RelOp -> I.RelOp
trRelOp op = case op of
                A.LTH   -> I.LTH
                A.LE    -> I.LE
                A.GTH   -> I.GTH
                A.GE    -> I.GE
                A.EQU   -> I.EQU
                A.NE    -> I.NE

simpExp' :: A.Expr -> StmM I.Expr
simpExp' e = fst <$> simpExp e

simpExp :: A.Expr -> StmM (I.Expr, A.Type)

simpExp (A.EVar ident) = (,) <$> (I.EVar <$> getPos ident) <*> getType ident

simpExp (A.ELitInt int) = return (I.ELitInt (fromInteger int), A.Int)

simpExp A.ELitTrue = return (I.ELitTrue, A.Bool)
simpExp A.ELitFalse = return (I.ELitFalse, A.Bool)

simpExp (A.EString s) = do
    strLabels <- gets strings
    let next = M.size strLabels
    modify $ \st -> st { strings = M.insert next s strLabels }
    return (I.EString next, A.Str)

simpExp (A.Neg e) = (, A.Int ) <$> (I.Neg <$> simpExp' e)
simpExp (A.Not e) = (, A.Bool) <$> (I.Not <$> simpExp' e)

simpExp (A.EMul eL op eR) = (, A.Int)
    <$> (I.EMul <$> simpExp' eL <*> pure (trMulOp op) <*> simpExp' eR)

simpExp (A.EAdd eL op eR) = do
    (eL', t) <- simpExp eL
    (eR', _) <- simpExp eR
    case t of
        A.Str -> return (I.Cat eL' eR', A.Str)
        A.Int -> return (I.EAdd eL' (trAddOp op) eR', A.Int)

simpExp (A.EAnd eL eR) = (, A.Bool)
    <$> (I.EAnd <$> simpExp' eL <*> simpExp' eR)

simpExp (A.EOr eL eR) = (, A.Bool)
    <$> (I.EOr <$> simpExp' eL <*> simpExp' eR)

simpExp (A.EApp ident es) = (,)
    <$> (I.EApp (fromIdent ident) <$> mapM simpExp' es)
    <*> asks (fromJust . M.lookup ident)

simpExp (A.ERel eL op eR) = (, A.Bool)
    <$> (I.ERel <$> simpExp' eL <*> pure (trRelOp op) <*> simpExp' eR)
--    (eL', t) <- simpExp eL
--    (eR', _) <- simpExp eR
--    let (eL'', eR'') = case t of
--                        A.Int -> (eL', eR')
--                        A.Bool -> (I.B2I eL', I.B2I eR')
--    return (I.ERel eL'' (trRelOp op) eR'', A.Bool)
