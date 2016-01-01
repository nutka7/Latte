module StaticCheck (checkProgram) where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Int ( Int32 )
import Text.Printf

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import AbsLatte


-- Helpers

fromIdent :: Ident -> String
fromIdent (Ident s) = s

findDuplicate :: Ord a => [a] -> Maybe a
findDuplicate l =
    let prefixes = scanl (flip S.insert) S.empty l
        duplicates = map fst $ filter (uncurry S.member) (zip l prefixes)
    in listToMaybe duplicates


-- Monad for checking functions

type VarBindings = M.Map Ident Type
type FunBindings = M.Map Ident FunType

data StmEnv = StmEnv { funs :: FunBindings,
                       outerVars :: VarBindings,
                       retType :: Type }
type StmState = M.Map Ident Type
type StmCheck = ReaderT StmEnv (ErrorT String (StateT StmState Identity))

runStmCheck :: StmCheck a -> StmEnv -> StmState -> (Either String a, StmState)
runStmCheck m r s =
    runIdentity $ runStateT (runErrorT (runReaderT m r)) s

evalStmCheck :: StmCheck a -> StmEnv -> StmState -> Either String a
evalStmCheck m r s = fst $ runStmCheck m r s


-- Collecting functions' types

stdFuns :: [(Ident, FunType)]
stdFuns = [
    (Ident "printInt"   , FunType Void [Int]),
    (Ident "printString", FunType Void [Str]),
    (Ident "error"      , FunType Void []),
    (Ident "readInt"    , FunType Int []),
    (Ident "readString" , FunType Str [])]

extractFunT :: TopDef -> (Ident, FunType)
extractFunT (FnDef funType ident args _) =
    (ident, FunType funType (map (snd . extractArgT) args))

extractArgT :: Arg -> (Ident, Type)
extractArgT (Arg argType ident) = (ident, argType)

collectFunBindings :: [TopDef] -> Either String FunBindings
collectFunBindings topdefs = do
    let funBindings = stdFuns ++ map extractFunT topdefs
        funIdents = map fst funBindings
        mainIdent = Ident "main"
        mainType  = FunType Int []

    case lookup mainIdent funBindings of
        Nothing -> throwError "main function missing"
        Just actualType -> when (actualType /= mainType) $
          do let err  = "bad main signature, expected: %s, seen %s"
                 err' = printf err (show mainType) (show actualType)
             throwError err'

    case findDuplicate funIdents of
        Nothing -> return (M.fromList funBindings)
        Just funIdent -> do
            let err = "multiple definitions for function `%s`"
                err' = printf err (fromIdent funIdent)
            throwError err'

-- Checking function correctness

funLocInfo :: Ident -> String
funLocInfo funIdent =
    let err = "In function `%s`: "
    in printf err (fromIdent funIdent)

addLocInfo :: String -> Either String a -> Either String a
addLocInfo s (Left err)  = Left (s ++ err)
addLocInfo _ (Right val) = Right val

collectArgBindings :: [Arg] -> Either String VarBindings
collectArgBindings args = do
    let argBindings = map extractArgT args
    case findDuplicate (map fst argBindings) of
        Nothing -> return ()
        Just argIdent -> do
            let err  = "multiple declarations for parameter `%s`"
                err' = printf err (fromIdent argIdent)
            throwError err'
    case find ((== Void) . snd) argBindings of
        Nothing -> return ()
        Just (argIdent, Void) -> do
            let err  = "parameter `%s` declared void"
                err' = printf err (fromIdent argIdent)
            throwError err'
    return (M.fromList argBindings)

checkReturn :: Bool -> Type -> Either String ()
checkReturn ret rType =
     unless (ret || rType == Void) $ do
        let err  = "missing return statement"
        throwError err

checkFun :: FunBindings -> TopDef -> Either String ()
checkFun funBindings (FnDef rType ident args block) = do
    let locInfo = funLocInfo ident
    addLocInfo locInfo $ do
    argBindings <- collectArgBindings args
    let initEnv = StmEnv { funs = funBindings,
                           outerVars = M.empty,
                           retType = rType }
    ret <- evalStmCheck (checkStm (BStmt block)) initEnv argBindings
    checkReturn ret rType


-- Statement correctness


checkInit :: Type -> Item -> StmCheck ()
checkInit _ (NoInit _) = return ()
checkInit dType (Init ident exp) = do
    eType <- checkExp exp
    when (dType /= eType) $ do
        let err  = "variable `%s` was declared ``%s`` but initialized with ``%s``"
            err' = printf err (fromIdent ident) (show dType) (show eType)
        throwError err'

checkFirstDecl :: Ident -> StmCheck ()
checkFirstDecl ident = do
    outer <- asks outerVars
    current <- get
    when (M.member ident current && M.notMember ident outer) $ do
        let err  = "variable `%s` declared multiple times in the same block"
            err' = printf err (fromIdent ident)
        throwError err'

declareItem :: Type -> Item -> StmCheck ()
declareItem dType item = do
    let ident = itemIdent item
    checkFirstDecl ident
    checkInit dType item
    modify (M.insert ident dType)

itemIdent :: Item -> Ident
itemIdent (NoInit ident) = ident
itemIdent (Init ident _) = ident

checkNoDecl :: Stmt -> String -> StmCheck ()
checkNoDecl (Decl _ _) place =
    throwError $ "naked (not in block) declaration in " ++ place
checkNoDecl _ _ = return ()

checkStm :: Stmt -> StmCheck Bool
checkStm Empty = return False

checkStm (BStmt (Block stms)) = do
    s <- get
    rets <- local (\r -> r { outerVars = s }) (mapM checkStm stms)
    put s
    return (or rets)

checkStm (Decl Void items) = do
    let names = map (fromIdent . itemIdent) items
        namesStr = intercalate ", " $ map (\s -> "`" ++ s ++ "`") names
    throwError $ "can't declare void variables: " ++ namesStr

checkStm (Decl dType items) = do
    mapM_ (declareItem dType) items
    return False

checkStm (Ass ident exp) = do
    mVarType <- gets (M.lookup ident)
    case mVarType of
        Nothing -> do
            let err  = "assignment to undeclared variable `%s`"
                err' = printf err (fromIdent ident)
            throwError err'
        Just varType -> do
            expType <- checkExp exp
            when (varType /= expType) $ do
                let err  = "assigned ``%s`` expression to ``%s`` variable `%s`"
                    err' = printf err (show expType) (show varType) (fromIdent ident)
                throwError err'
    return False

checkStm (Incr ident) = do
    mVarType <- gets (M.lookup ident)
    case mVarType of
        Nothing -> do let err  = "Inc/Dec stmt on undeclared variable `%s`"
                          err' = printf err (fromIdent ident)
                      throwError err'
        Just varType ->
            when (varType /= Int) $ do
                let err  = "Incr/Decr stmt on ``%s`` variable `%s`"
                    err' = printf err (show varType) (fromIdent ident)
                throwError err'
    return False

checkStm (Decr ident) = checkStm (Incr ident)

checkStm (Ret exp) = do
    expType <- checkExp exp
    rettype <- asks retType
    when (rettype == Void) $ do
        let err  = "attempted to return ``%s`` when void return expected"
            err' = printf err (show expType)
        throwError err'
    when (expType /= rettype) $ do
        let err  = "attempted to return ``%s`` when ``%s`` value expected"
            err' = printf err (show expType) (show rettype)
        throwError err'
    return True

checkStm VRet = do
    rettype <- asks retType
    when (rettype /= Void) $ do
        let err  = "attempted to void return when expected to return ``%s``"
            err' = printf err (show rettype)
        throwError err'
    return True

checkStm (CondElse exp stmT stmF) = do
    condType <- checkExp exp
    when (condType /= Bool) $ do
        let err  = "non-logical - ``%s`` expression in if condition"
            err' = printf err (show condType)
        throwError err'
    checkNoDecl stmT "if true-branch"
    checkNoDecl stmF "if false-branch"
    retT <- checkStm stmT
    retF <- checkStm stmF
    return $ case exp of
        ELitTrue -> retT
        ELitFalse -> retF
        _ -> retT && retF

checkStm (Cond exp stm) = checkStm (CondElse exp stm Empty)

checkStm (While exp stm) = do
    condType <- checkExp exp
    when (condType /= Bool) $ do
        let err  = "non-logical - ``%s`` expression in while condition"
            err' = printf err (show condType)
        throwError err'
    checkNoDecl stm "while"
    ret <- checkStm stm
    return (exp == ELitTrue)

checkStm (SExp exp) = checkExp exp >> return False


-- Expression correctness


checkExp :: Expr -> StmCheck Type
checkExp (EVar ident) = do
    mVarType <- gets (M.lookup ident)
    case mVarType of
        Just varType -> return varType
        Nothing -> do
            let err  = "undeclared variable `%s` used in expression"
                err' = printf err (fromIdent ident)
            throwError err'

checkExp (ELitInt int) = do
    when (int > toInteger (maxBound :: Int32)) $ do
        let err  = "constant larger than 2^31 - 1: %s"
            err' = printf err (show int)
        throwError err'
    return Int

checkExp ELitTrue = return Bool
checkExp ELitFalse = return Bool
checkExp (EString _) = return Str

checkExp (EApp ident exps) = do
    mFunType <- asks (M.lookup ident . funs)
    case mFunType of
        Nothing -> do
            let err  = "call to undefined function `%s`"
                err' = printf err (fromIdent ident)
            throwError err'
        Just (FunType rType argTypes) -> do
            let expCount = length exps
                argCount = length argTypes
            when (expCount /= argCount) $ do
                let err  = "bad number of arguments in call to `%s`, expected: %s, seen: %s"
                    err' = printf err (fromIdent ident) (show argCount) (show expCount)
                throwError err'
            actualTypes <- mapM checkExp exps
            let mMismatch = findIndex (uncurry (/=)) (zip argTypes actualTypes)
            case mMismatch of
              Nothing -> return rType
              Just index -> do
                let argT = argTypes !! index
                    actualT = actualTypes !! index
                let err  = "bad type of %s. argument in call to `%s`, expected: `%s`, seen: `%s`"
                    err' = printf err (show (index+1)) (fromIdent ident) (show argT) (show actualT)
                throwError err'

checkExp (Neg exp) = do
    expType <- checkExp exp
    when (expType /= Int) $ do
        let err  = "Neg operator on ``%s`` expression"
            err' = printf err (show expType)
        throwError err'
    return Int

checkExp (Not exp) = do
    expType <- checkExp exp
    when (expType /= Bool) $ do
        let err  = "Not operator on ``%s`` expression"
            err' = printf err (show expType)
        throwError err'
    return Bool


checkExp (EMul eL Times eR) = checkBinExp "Mul" eL eR
checkExp (EMul eL Div eR) = checkBinExp "Div" eL eR
checkExp (EMul eL Mod eR) = checkBinExp "Mod" eL eR
checkExp (EAdd eL Plus eR) = checkBinExp "Add" eL eR
checkExp (EAdd eL Minus eR) = checkBinExp "Sub" eL eR
checkExp (ERel eL EQU eR) = checkBinExp "Eq" eL eR
checkExp (ERel eL NE eR) = checkBinExp "Neq" eL eR
checkExp (ERel eL _ eR) = checkBinExp "Rel" eL eR
checkExp (EAnd eL eR) = checkBinExp "And" eL eR
checkExp (EOr eL eR) = checkBinExp "Or" eL eR


ops :: [(String, [TypeCode], Type)]
ops = [
    ("Mul", [I], Int),
    ("Div", [I], Int),
    ("Mod", [I], Int),
    ("Add", [I], Int),
    ("Add", [S], Str),
    ("Sub", [I], Int),
    ("Eq" , [I, S, B], Bool),
    ("Neq", [I, S, B], Bool),
    ("Rel", [I, B], Bool),
    ("And", [B], Bool),
    ("Or" , [B], Bool)]

lookupOp :: String -> TypeCode -> Maybe Type
lookupOp opName code =
    third <$> find isMatch ops
    where
        isMatch (op, codes, _) = op == opName && code `elem` codes
        third (_, _, z) = z

data TypeCode = I | S | B deriving (Eq, Ord, Show)

typeToCode :: Type -> TypeCode
typeToCode t = case t of
    Int  -> I
    Str  -> S
    Bool -> B

checkBinExp :: String -> Expr -> Expr -> StmCheck Type
checkBinExp opName eL eR = do
    typeL <- checkExp eL
    typeR <- checkExp eR
    when (typeL /= typeR) $ do
        let err  = "mismatched types in %s operator, left: ``%s``, right: ``%s``"
            err' = printf err opName (show typeL) (show typeR)
        throwError err'
    let mRetType = lookupOp opName (typeToCode typeL)
    case mRetType of
        Just rettype -> return rettype
        Nothing -> do
            let err  = "unsupported type ``%s`` in %s op"
                err' = printf err (show typeL) (show opName)
            throwError err'

-- Program correctness

checkProgram :: Program -> Either String ()
checkProgram (Program topdefs) = do
    bindings <- collectFunBindings topdefs
    mapM_ (checkFun bindings) topdefs
