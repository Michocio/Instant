module Jvm where

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
import System.FilePath

srcFile file = ".source " ++ (toJVM file) ++ "\n"
classDirective file = ".class  public " ++ takeBaseName file ++ "\n"
heritanceDirective = ".super  java/lang/Object\n"

multiLineSeparator = "\n\n\n"
constructorCode = ".method public <init>()V\n\
                        \aload_0\n\
                        \invokespecial java/lang/Object/<init>()V\n\
                        \return\n\
                    \.end method\n"

mainFunHeader = ".method public static main([Ljava/lang/String;)V\n"
stackLimit = ".limit stack "
varsLimit = ".limit locals "
endMethod = "return\n\
                \.end method"

printMethodGet = "getstatic java/lang/System/out Ljava/io/PrintStream;\n"
printMethodInvoke = "invokevirtual java/io/PrintStream/println(I)V\n"
type Counter = Integer -- Variables and stack counter
type Store = M.Map Ident Counter

data ExpT =
   ExpAddT ExpT ExpT Integer
 | ExpSubT ExpT ExpT Integer
 | ExpMulT ExpT ExpT Integer
 | ExpDivT ExpT ExpT Integer
 | ExpLitT Integer Integer
 | ExpVarT Ident Integer
  deriving (Eq,Ord,Show)

type Env = (Store, Counter, Counter, String)

emptyEnv :: Env
emptyEnv = (M.empty, 0, 0, "")

startProgram :: String -> Program -> IO String
startProgram file (Prog p) =  (evalStateT (runStmt file (p)) emptyEnv)

runStmt  :: String -> [Stmt] -> StateT Env IO String
runStmt file [] = do
    (_, vars, stacks, code) <- get
    liftIO $ writeFile (toJvmFile file)(--srcFile file ++
                            classDirective file ++
                            heritanceDirective ++
                            multiLineSeparator ++
                            constructorCode ++
                            multiLineSeparator ++
                            mainFunHeader ++
                            stackLimit ++ show (stacks + 1) ++ "\n" ++
                            varsLimit ++ show (vars + 1) ++ "\n" ++
                            code ++ "\n" ++
                            endMethod ++ "\n")
    return ""
runStmt file (x:xs) = do
    doStmt x
    runStmt file xs
    return ""


doStmt :: Stmt ->  StateT Env IO ()
doStmt (SAss ident exp) =
    assingment ident exp
doStmt (SExp exp) = do
    (s, v, st, code) <- get
    put(s, v, st, code ++ printMethodGet)
    evalExp exp
    (s', v', st', code') <- get
    put(s', v', st', code' ++ printMethodInvoke)
    return ()

assingment :: Ident -> Exp ->  StateT Env IO ()
assingment ident exp = do
    (store, vars, stacks, c) <- get
    case (M.lookup ident store) of
        (Just num) -> return ()
        otherwise -> do
            put(M.insert ident (vars) store, vars + 1, stacks, c)
            return ()
    evalExp exp
    (store, vars, stacks, c) <- get
    put(store, vars, stacks, c ++ ("istore " ++ (show $ fromJust (M.lookup ident store)) ++ "\n"))

getVarNum :: Ident -> StateT Env IO Counter
getVarNum ident = do
    (store, vars, stacks, c) <- get
    case (M.lookup ident store) of
        (Just num) -> return num
        otherwise -> do
            put(M.insert ident (vars) store, vars + 1, stacks, c)
            return vars

depth :: ExpT -> Integer
depth (ExpAddT _ _ h) = h
depth (ExpSubT _ _ h) = h
depth (ExpMulT _ _ h) = h
depth (ExpDivT _ _ h) = h
depth (ExpLitT _ h) = 0
depth (ExpVarT _ h) = h

expToExpT :: Exp -> ExpT -> ExpT -> Integer -> ExpT
expToExpT (ExpAdd e1 e2)= ExpAddT
expToExpT (ExpSub e1 e2)= ExpSubT
expToExpT (ExpMul e1 e2)= ExpMulT
expToExpT (ExpDiv e1 e2)= ExpDivT

fstExp :: Exp -> Exp
fstExp (ExpAdd e1 e2)= e1
fstExp (ExpSub e1 e2)= e1
fstExp (ExpMul e1 e2)= e1
fstExp (ExpDiv e1 e2)= e1

sndExp :: Exp -> Exp
sndExp (ExpAdd e1 e2)= e2
sndExp (ExpSub e1 e2)= e2
sndExp (ExpMul e1 e2)= e2
sndExp (ExpDiv e1 e2)= e2

expToTree :: Exp -> ExpT
expToTree (ExpLit const) = (ExpLitT const 0)
expToTree (ExpVar var) = (ExpVarT var 0)
expToTree exp = let
            x1 = (expToTree $ fstExp exp)
            x2 = (expToTree $ sndExp exp)
            in
                ((expToExpT exp) x1 x2 ((depth x1) + (depth x2) + 1))


evalExp :: Exp -> StateT Env IO Integer
evalExp exp = traverse (expToTree exp) 0

-- drzewo kod current
traverse :: ExpT ->  Integer -> StateT Env IO Integer
traverse (ExpLitT val _) current = do
    (s, v, stacks, c) <- get
    put(s, v, max stacks (current + 1), c ++ ("bipush " ++ (show val) ++ "\n"))
    return $ current + 1
traverse (ExpVarT var _) current = do
    num <- (getVarNum var)
    (s, v, stacks, c) <- get
    put(s, v, max stacks (current + 1), c ++ ("iload " ++ show num ++ "\n"))
    return $ current + 1
traverse (ExpAddT e1 e2 _) current = printInstr "iadd " e1 e2 current False
traverse (ExpSubT e1 e2 _) current = printInstr "isub " e1 e2 current True
traverse (ExpMulT e1 e2 _) current = printInstr "imul " e1 e2 current False
traverse (ExpDivT e1 e2 _) current = printInstr "idiv " e1 e2 current True

printInstr :: String -> ExpT -> ExpT -> Integer -> Bool -> StateT Env IO Integer
printInstr instr e1 e2 current reverse = do
    (s, v, stacks, c) <- get
    if (depth e1 > depth e2) then
        do
            x <- traverse e1 current
            y <- traverse e2 x
            (s, v, stacks, c) <- get
            put(s, v, stacks, c ++ instr ++ "\n")
            return $ y - 1
    else
        do
            x <- traverse e2 current
            y <- traverse e1 x
            (s, v, stacks, c) <- get
            if(reverse == True) then
                put(s, v, stacks, c ++ "swap\n" ++instr ++ "\n")
            else
                put(s, v, stacks, c ++ instr ++ "\n")
            return $ y - 1
