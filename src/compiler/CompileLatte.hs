module CompileLatte (compile) where

import Data.Char (showLitChar)
import IntermediateLatte
import Text.Printf
import Data.Maybe
import Data.List (intersperse, intercalate)
import Control.Monad.State

strPref :: String
strPref = "LatStr_"

funPref :: String
funPref = "LatFun_"

labPref :: String
labPref = "LatLabel_"

libFuns :: [String]
libFuns = [
    "printInt", "printString", "error", "readInt", "readString", "concatStrings"]

-- Monad for compiling

data CodeState = CodeState { next :: Int, instructions :: [String] }
type Code = State CodeState

nextLabel :: Code String
nextLabel = do
    nr <- gets next
    modify $ \st -> st { next = nr + 1 }
    return $ printf "%s%d" labPref nr

emit :: String -> Code ()
emit instr = modify $ \st -> st { instructions = instr:instructions st }

emits :: [String] -> Code ()
emits = mapM_ emit

emitLab :: String -> Code ()
emitLab label = emit (label ++ ":")

--

compile :: Program -> String
compile p =
    let initSt = CodeState { next = 0, instructions = [] }
        rev_lines = instructions $ execState (compProg p) initSt
    in intercalate "\n" $ reverse ("":rev_lines)

putString :: Int -> String -> String
putString nr str = printf "%s%d: db `%s`,0" strPref nr (escape str)
    where
        escape s = foldl (.) id (map showLitChar s) $ ""

putMainGl :: String
putMainGl = printf "global %smain" funPref

putLibFun :: String -> String
putLibFun label = printf "extern %s" label

compProg :: Program -> Code ()
compProg (Program funs strings) = do
    emit putMainGl
    emits $ map putLibFun libFuns
    emit ""
    emits $ map (uncurry putString) strings
    emit ""
    sequence_ $ intersperse (emit "") (map compFun funs)

putProlog :: Int -> [String]
putProlog localsNum = [
    "push ebp",
    "mov ebp, esp",
    printf "sub esp, %d" (4 * localsNum)]

putEpilog :: [String]
putEpilog = [
    "leave",
    "ret"]

compFun :: Fun -> Code ()
compFun (Fun label stm localsNum) = do
    emit $ printf "%s%s:" funPref label
    emits $ putProlog localsNum
    compStm stm
    emits putEpilog

compItem :: Item -> Code ()
compItem (Item var e) = compStm (Ass var e)

putAdress :: Var -> String
putAdress (Param i) = printf "ebp + %d" ((i + 1) * 4)
putAdress (Local i) = printf "ebp - %d" (i * 4)

compStm :: Stmt -> Code ()
compStm Empty = return ()
compStm (BStmt stms) = mapM_ compStm stms
compStm (Decl items) = mapM_ compItem items
compStm (Ass var e) = do
    compExp e
    emit $ printf "pop dword [%s]" (putAdress var)
compStm (Incr var) = emit $ printf "inc dword [%s]" (putAdress var)
compStm (Decr var) = emit $ printf "dec dword [%s]" (putAdress var)
compStm (Ret e) = do
    compExp e
    emit "pop eax"
    emits putEpilog
compStm VRet = emits putEpilog
compStm (SExp e) = do
    compExp e
    emit "add esp, 4"

compStm (Cond e stm) = do
    compExp e
    afterLabel <- nextLabel
    emit "pop eax"
    emit "test eax, eax"
    emit $ printf "jz %s" afterLabel
    compStm stm
    emitLab afterLabel

compStm (CondElse e stmT stmF) = do
    compExp e
    falseLabel <- nextLabel
    afterLabel <- nextLabel
    emit "pop eax"
    emit "test eax, eax"
    emit $ printf "jz %s" falseLabel
    compStm stmT
    emit $ printf "jmp %s" afterLabel
    emitLab falseLabel
    compStm stmF
    emitLab afterLabel

compStm (While e stm) = do
    bodyLabel <- nextLabel
    condLabel <- nextLabel
    emit $ printf "jmp %s" condLabel
    emitLab bodyLabel
    compStm stm
    emitLab condLabel
    compExp e
    emit "pop eax"
    emit "test eax, eax"
    emit $ printf "jnz %s" bodyLabel

jumps :: [(RelOp, String)]
jumps = [
    (LTH, "jl" ),
    (LE , "jle"),
    (GTH, "jg" ),
    (GE , "jge"),
    (EQU, "je" ),
    (NE , "jne")]


compExp :: Expr -> Code ()

compExp (EVar var) = emit $ printf "push dword [%s]" (putAdress var)
compExp (ELitInt i) = emit $ printf "push dword %d" i
compExp ELitTrue = emit "push dword 1"
compExp ELitFalse = emit "push dword 0"
compExp (EString i) = emit $ printf "push %s%d" strPref i
compExp (EApp label es) = do
    mapM_ compExp (reverse es)
    let pref = if label `elem` libFuns then "" else funPref
    emit $ printf "call %s%s" pref label
    emit $ printf "add esp, %d" (length es * 4)
    emit "push eax"
compExp (Neg e) = do
    compExp e
    emit "neg dword [esp]"
compExp (Not e) = do
    compExp e
    emit "xor dword [esp], 1"
compExp (EMul eL Times eR) = do
    compExp eL
    compExp eR
    emit "pop eax"
    emit "pop ecx"
    emit "imul eax, ecx"
    emit "push eax"
compExp (EMul eL op eR) = do
    compExp eL
    compExp eR
    emit "pop ecx"
    emit "pop eax"
    emit "cdq"
    emit "idiv ecx"
    case op of
       Div -> emit "push eax"
       Mod -> emit "push edx"
compExp (EAdd eL op eR) = do
    compExp eL
    compExp eR
    emit "pop eax"
    case op of
        Plus  -> emit "add [esp], eax"
        Minus -> emit "sub [esp], eax"
compExp (Cat eL eR) = compExp (EApp "concatStrings" [eL, eR])
compExp (ERel eL op eR) = do
    compExp eL
    compExp eR
    emit "pop ecx"
    emit "pop eax"
    emit "cmp eax, ecx"
    trueLabel  <- nextLabel
    afterLabel <- nextLabel
    emit $ printf "%s %s" (fromJust (lookup op jumps)) trueLabel
    emit "push dword 0"
    emit $ printf "jmp %s" afterLabel
    emitLab trueLabel
    emit "push dword 1"
    emitLab afterLabel
compExp (EAnd eL eR) = do
    label1 <- nextLabel
    label2 <- nextLabel
    compExp eL
    emit "pop eax"
    emit "test eax, eax"
    emit $ printf "jz %s" label1
    compExp eR
    emit $ printf "jmp %s" label2
    emitLab label1
    emit "push dword 0"
    emitLab label2
compExp (EOr eL eR) = do
    label1 <- nextLabel
    label2 <- nextLabel
    compExp eL
    emit "pop eax"
    emit "test eax, eax"
    emit $ printf "jnz %s" label1
    compExp eR
    emit $ printf "jmp %s" label2
    emitLab label1
    emit "push dword 1"
    emitLab label2
