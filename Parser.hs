module Parser where
import Lexer

data Statement = Exp ExpAst | DecList [Dec] | ComList [Com] deriving (Show, Eq)
data Dec = Dec Ide | Init Ide ExpAst deriving (Show, Eq)
data Com = Com Ide ExpAst deriving (Show, Eq)
data ExpAst = ExpNode Operation ExpAst ExpAst | ValNode Int | Empty deriving (Show,Eq)

-- GRAMMAR --
-- post-fix F stands for "left Factored"
-- post-fix ' stands for productions from which left recursion has been
-- eliminated
{-
- S -> DecL | ComL | Exp
- DecL -> Dec DecLF
- DecLF -> and DecL | epsilon
- ComL -> Com ComLF
- ComLF -> ; ComL | epsilon
- Dec -> int ide | int ide = Expr
- Com -> ide = Expr
- Expr -> TermExpr'
- Expr' -> +TermExpr' | -TermExpr' | epsilon
- Term -> FactTerm'
- Term' -> *FactTerm' | /FactTerm' | epsilon
- Fact -> num | ide | (Exp)
-}
         
check :: [Token] -> a -> a
check [] result = result
check (x:_) _ = error $ "parsing error: Unexpected token '" ++ show x ++ "'"

parse :: [Token] -> Statement
parse all@(Int:tokens) = check rest ast
    where (ast, rest) = parseDecL all

parse all@(Ide x : tokens) = check rest ast
    where (ast, rest) = parseComL all

parse tokens = check rest (Exp ast)
    where (ast, rest) = parseExpr tokens

parseDecL :: [Token] -> (Statement, [Token])
parseDecL all@(Int:tokens) = (ast', tokens'')
    where (ast, tokens') = parseDec all
          (ast', tokens'') = parseDecLF tokens' ast

parseDecLF :: [Token] -> Dec -> (Statement, [Token])
parseDecLF (And : tokens) ast = (DecList (ast : ast'), tokens')
    where (DecList ast', tokens') = parseDecL tokens 

parseDecLF [] ast = (DecList [ast], [])
parseDecLF _ _ = error "parsing error on DecLF"

parseDec :: [Token] -> (Dec, [Token])
parseDec (Int : Ide x : And : tokens) = (Dec x, And : tokens)
parseDec [Int, Ide x] = (Dec x, [])
parseDec (Int : Ide x : Equals : expression) = (Init x exp, tokens)
    where (exp, tokens) = parseExpr expression

parseComL :: [Token] -> (Statement, [Token])
parseComL all@(Ide x : tokens) = (ast', tokens'')
    where (ast, tokens') = parseCom all
          (ast', tokens'') = parseComLF tokens' ast

parseComLF :: [Token] -> Com -> (Statement, [Token])
parseComLF (SemiColon : tokens) ast = (ComList (ast : ast'), tokens')
    where (ComList ast', tokens') = parseComL tokens

parseComLF [] ast = (ComList [ast], [])
parseComLF _ _ = error "parsing error on ComLF"

parseCom :: [Token] -> (Com, [Token])
parseCom (Ide x : Equals : expression) = (Com x exp, tokens)
    where (exp, tokens) = parseExpr expression

parseExpr :: [Token] -> (ExpAst, [Token])
parseExpr tokens = (ast', tokens'')
    where (ast, tokens')  = parseTerm tokens
          (ast', tokens'') = parseExpr' tokens' ast

parseExpr' :: [Token] -> ExpAst -> (ExpAst, [Token])
parseExpr' (BinOp Plus : tokens) ast = parseExpr' tokens' (ExpNode Plus ast ast')
    where (ast', tokens') = parseTerm tokens

parseExpr' (BinOp Minus : tokens) ast = parseExpr' tokens' (ExpNode Minus ast ast')
    where (ast', tokens') = parseTerm tokens

parseExpr' (t : tokens) ast = case t of
                                   SemiColon -> (ast, t:tokens)
                                   RParen -> (ast, t:tokens)
                                   And -> (ast, t:tokens)
                                   _ -> error "parsing error on Expr'"
parseExpr' [] ast = (ast, [])

parseTerm :: [Token] -> (ExpAst, [Token])
parseTerm tokens = (ast', tokens'')
    where (ast, tokens') = parseFactor tokens
          (ast', tokens'') = parseTerm' tokens' ast

parseTerm' :: [Token] -> ExpAst -> (ExpAst, [Token])
parseTerm' (BinOp Times : tokens) ast = parseTerm' tokens' (ExpNode Times ast ast')
    where (ast', tokens') = parseFactor tokens

parseTerm' (BinOp Divide : tokens) ast = parseTerm' tokens' (ExpNode Divide ast ast')
    where (ast', tokens') = parseFactor tokens

parseTerm' (t : tokens) ast = case t of
                                   BinOp Plus -> (ast, t:tokens)
                                   BinOp Minus -> (ast, t:tokens)
                                   SemiColon -> (ast, t:tokens)
                                   And -> (ast, t:tokens)
                                   RParen -> (ast, t:tokens)
                                   _ -> error "parsing error on Term'"

parseTerm' [] ast = (ast, [])

parseFactor :: [Token] -> (ExpAst, [Token])
parseFactor (LParen : tokens) = case tokens' of
                                     RParen : tokens'' -> (ast, tokens'')
                                     _ -> error "parsing error: Missing closing parentheses"
    where (ast, tokens') = parseExpr tokens

parseFactor (Num a : tokens) = (ValNode a, tokens)
parseFactor _ = error "parsing error on Factor"


