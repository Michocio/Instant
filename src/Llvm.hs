module Llvm where

import qualified Data.Map as M
import LexInstant
import ParInstant
import SkelInstant
import PrintInstant
import AbsInstant
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe
import System.Environment
import System.IO
import Helpers

type RegCounter = (M.Map Ident Integer, Integer, Integer, Integer, Integer, String)

printIntDecl = "declare void @printInt(i32)\n\n"
mainFunHeader = "define i32 @main() {\n\n"
mainFunEnd = "\nret i32 0\n}"

nextReg :: String -> StateT RegCounter IO Integer
nextReg what = do
    (vars, add, sub, mul, sdiv, code) <- get
    case what of
        "add" -> do
            put(vars, add + 1, sub, mul, sdiv, code)
            return add
        "sub" -> do
            put(vars, add, sub + 1, mul, sdiv, code)
            return sub
        "mul" -> do
            put(vars, add, sub, mul + 1, sdiv, code)
            return mul
        "sdiv" -> do
            put(vars, add, sub, mul, sdiv + 1, code)
            return sdiv

emptyCounter = (M.empty, 0, 0, 0, 0, "")

startProgram :: String -> Program -> IO ()
startProgram file (Prog p) =  do
    (evalStateT (runStmt file (p)) emptyCounter)

runStmt  :: String-> [Stmt] -> StateT RegCounter IO ()
runStmt file [] = do
    (_, _, _, _, _, code) <- get
    liftIO $ writeFile (toLlvmFile file) (printIntDecl ++
        mainFunHeader ++
        code ++
        mainFunEnd)
runStmt file (x:xs) = do
    doStmt x
    runStmt file xs
    return ()

doStmt :: Stmt ->  StateT RegCounter IO ()
doStmt (SExp exp) = do
    x <- evalExp exp
    addCode  ("call void @printInt(i32 " ++ x ++")")
    return ()
doStmt (SAss var e) = do
    x <- assingment var e
    return ()

addCode :: String -> StateT RegCounter IO ()
addCode insert = do
    (vars, add, sub, mul, sdiv, code) <- get
    put(vars, add, sub, mul, sdiv, code ++ insert ++ "\n")
    return ()

evalExp :: Exp -> StateT RegCounter IO String
evalExp (ExpAdd e1 e2) = genInstr e1 e2 "add"
evalExp (ExpSub e1 e2) = genInstr e1 e2 "sub"
evalExp (ExpMul e1 e2) = genInstr e1 e2 "mul"
evalExp (ExpDiv e1 e2) = genInstr e1 e2 "sdiv"
evalExp (ExpLit const) = return $ show const
evalExp (ExpVar ident@(Ident var)) = do
    (vars, add, sub, mul, sdiv, code) <- get
    case (M.lookup ident vars) of
        (Just x) -> do
            put(M.insert ident (x+1) vars, add, sub, mul, sdiv, code)
            addCode ("%" ++ (var ++ (show x)) ++ " = " ++ "load i32, i32* " ++ "%" ++ var)
            return $ "%" ++ var ++ (show x)


genInstr :: Exp -> Exp -> String -> StateT RegCounter IO String
genInstr e1 e2 instr = do
    t1 <- evalExp e1
    t2 <- evalExp e2
    x <- nextReg instr
    addCode ("%" ++ instr ++ (show x) ++ " = " ++ instr ++ " i32 " ++ t1 ++ ", " ++ t2)
    return $ "%" ++ instr ++ (show x)

assingment :: Ident -> Exp -> StateT RegCounter IO String
assingment ident@(Ident name) e = do
    varAssign ident
    x <- evalExp e
    addCode ("store i32 " ++ (x) ++ ", " ++ "i32* " ++"%" ++ name)
    return ""
varAssign :: Ident -> StateT RegCounter IO String
varAssign var@(Ident name) = do
    (vars, add, sub, mul, sdiv, code) <- get
    case (M.lookup var vars) of
        (Just x) -> do
            put(M.insert var (x) vars, add, sub, mul, sdiv, code)
            return ""
        otherwise ->
            do
                put(M.insert var 0 vars, add, sub, mul, sdiv, code)
                addCode  ("%" ++ name ++ " = " ++ "alloca i32")
                return ""
