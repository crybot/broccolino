module Main where
import Lexer
import Parser
import Control.Monad

data Bottom a = Bottom | Value a deriving (Show, Eq)                                                                                                     
type Frame a b = a -> Bottom b                                                  
type Stack a b = [Frame a b]                                                    

data Val = ValN Int | Unknown deriving (Show, Eq)
type Loc = Int                                                                  

type EnvFrame = Frame Ide Loc                                                   
type MemFrame = Frame Loc Val                                                   
type Env = [EnvFrame]                                                           
type Mem = [MemFrame]                                                           

w :: a -> Bottom b                                                              
w _ = Bottom                                                                    

add :: (Eq a, Eq b) => Frame a b  -> a -> Bottom b -> Frame a b                 
add f x y z | f x /= Bottom = undefined
            | x == z = y
            | otherwise = f z 

update :: (Eq a, Eq b) => Frame a b  -> a -> Bottom b -> Frame a b              
update f x y z | f x == Bottom = undefined
               | z == x = y               
               | otherwise = f z

searchStack :: (Eq a, Eq b) => [Frame a b] -> a -> Bottom b                    
searchStack [] _ = Bottom                                                      
searchStack (f:fs) x                                                           
    | f x /= Bottom = f x                                                       
    | otherwise = searchStack fs x                                             

addStack :: (Eq a, Eq b) => [Frame a b] -> a -> Bottom b -> [Frame a b]        
addStack (f:fs) x y = add f x y : fs                                         
addStack [] _ _ = undefined                                                    

updateStack :: (Eq a, Eq b) => [Frame a b] -> a -> Bottom b -> [Frame a b]     
updateStack [] _ _ = undefined                                                 
updateStack (f:fs) x y                                                         
    | f x /= Bottom = update f x y : fs                                       
    | otherwise = f : updateStack fs x y   

getLoc :: (Num a, Eq b) => [a -> Bottom b] -> a                                                           
getLoc (f:_) = findLoc f 0
    where findLoc f loc 
            | f loc == Bottom = loc
            | otherwise = findLoc f (loc+1)

semBop :: (Num a) => Operation -> (Val -> Val -> Val)                                
semBop o = f                                                                   
    where f (ValN x) (ValN y) = ValN (operator o x y)                               
          operator op = case op of
                   Plus -> (+)
                   Minus -> (-)
                   Times -> (*)
                   Divide -> div

semExp :: ExpAst -> Env -> Mem -> Val
semExp (ValNode n) env mem = ValN n

semExp (IdeNode id) env mem = v
    where Value loc = searchStack env id
          Value v = searchStack mem loc

semExp (ExpNode op e1 e2) env mem = o v1 v2
    where v1 = semExp e1 env mem
          v2 = semExp e2 env mem
          o = semBop op

semDec :: Dec -> Env -> Mem -> (Env, Mem)
semDec (Init id exp) env mem = (env', mem')
    where 
          val = semExp exp env mem
          env' = addStack env id (Value loc)
          mem' = addStack mem loc (Value val)
          loc = getLoc mem

semDec (Dec id) env mem = (env', mem')
    where env' = addStack env id (Value loc)
          mem' = addStack mem loc (Value Unknown)
          loc = getLoc mem

semDecL :: Statement -> Env -> Mem -> (Env, Mem)
semDecL (DecList (x:xs)) env mem = (env'', mem'')
    where (env', mem') = semDec x env mem
          (env'', mem'') = semDecL (DecList xs) env' mem'

semCom :: Com -> Env -> Mem -> Mem
semCom (Com id exp) env mem = mem'
    where mem' = updateStack mem loc (Value val)
          Value loc = searchStack env id
          val = semExp exp env mem

semCom (IfCom exp com1 com2) env mem 
    | condition /= 0 = semComL (ComList com1) env mem
    | otherwise = semComL (ComList com2) env mem
    where ValN condition = semExp exp env mem

semComL :: Statement -> Env -> Mem -> Mem
semComL (ComList (x:xs)) env mem = mem''
    where mem' = semCom x env mem
          mem'' = semComL (ComList xs) env mem'

interpretDec :: Dec -> Env -> Mem -> IO (Env, Mem)
interpretDec (Dec x) env mem =
    do
        print $ "[" ++ x ++ " = " ++ show val ++ "]"
        return (env', mem')
    where (env', mem') = semDec (Dec x) env mem
          Value val = searchStack mem' loc
          Value loc = searchStack env' x

interpretDec (Init x exp) env mem =
    do
        print $ "[" ++ x ++ " = " ++ show val ++ "]"
        return (env', mem')
    where (env', mem') = semDec (Init x exp) env mem
          Value val = searchStack mem' loc
          Value loc = searchStack env' x


interpretCom :: Com -> Env -> Mem -> IO Mem
interpretCom (Com id exp) env mem = do
    print $ "[" ++ id ++ " = " ++ show val ++ "]"
    return mem'
    where
        mem' = semCom (Com id exp) env mem
        Value val = searchStack mem' loc
        Value loc = searchStack env id

interpretCom (IfCom exp com1 com2) env mem 
    | condition /= 0 = do
        (_, mem') <- interpret (ComList com1) env mem
        return mem'
    | otherwise = do
        (_, mem') <- interpret (ComList com2) env mem
        return mem'
    where ValN condition = semExp exp env mem


interpret :: Statement -> Env -> Mem -> IO (Env, Mem)
interpret (Exp exp) env mem = do
    print $ semExp exp env mem
    return (env, mem)


interpret (DecList []) env mem  = return (env, mem)
interpret (DecList (x:xs)) env mem = do
    (env', mem') <- interpretDec x env mem
    interpret (DecList xs) env' mem'

interpret (ComList []) env mem  = return (env, mem)
interpret (ComList (x:xs)) env mem = do
    mem' <- interpretCom x env mem
    interpret (ComList xs) env mem'

main :: IO ()                                                                   
main = do
    exps <- lines <$> getContents
    let exps' =  map (parse . tokenize) exps
    foldM_ (\(env,mem) x -> interpret x env mem) ([w],[w]) exps'
    return ()
