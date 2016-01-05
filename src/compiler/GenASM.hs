-- Modified TestLatte.hs to parse and compile the program.
module Main ( main ) where

import System.IO ( hPutStrLn, hGetContents, readFile, stdin, stdout, stderr )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )

import LexLatte
import ParLatte
import SkelLatte
import PrintLatte
import AbsLatte
import StaticCheck
import ToIntermediate
import Text.Groom
--import CompileASM

import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

runFile :: ParseFun Program -> FilePath -> IO ()
runFile p f = readFile f >>= run p

putErr :: String -> IO ()
putErr = hPutStrLn stderr

run :: ParseFun Program -> String -> IO ()
run p s =
    let ts = myLLexer s
    in case p ts of
        Bad s -> do
            putErr "ERROR\n"
            putErr "Parse              Failed...\n"
            putErr "Tokens:"
            putErr $ show ts
            putErr s
            exitFailure
        Ok  tree -> do
            let check = checkProgram tree
            case check of
                Left err -> do
                    putErr "ERROR\n"
                    putErr $ err
                    exitFailure
                Right () -> do
                    putErr "OK"
                    hPutStrLn stdout (groom (simplify tree))
--                    let code = compile tree ids
--                    putStr code
                    exitSuccess

main :: IO ()
main = do
    args <- getArgs
    case args of
      [file] -> runFile pProgram file
      _ -> do
          putErr "Bad args."
          exitFailure
