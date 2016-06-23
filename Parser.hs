module Parser where
import Lexer

data Statement = Exp ExpAst | Dec Ide | Init Ide ExpAst deriving (Show, Eq)
data ExpAst = ExpNode Operation ExpAst ExpAst | ValNode Int | Empty deriving (Show,Eq)

{-
- S -> Dec | Com | Exp
- Dec -> int ide | int ide = Expr
- Com -> ide = Expr
- Expr -> TermExpr'
- Expr' -> +TermExpr' | -TermExpr' | epsilon
- Term -> FactTerm'
- Term' -> *FactTerm' | /FactTerm' | epsilon
- Fact -> num | ide | (Exp)
-}
         
parse :: [Token] -> Statement
parse (Int:tokens) = ast
    where (ast, _) = parseDec (Int:tokens)

parse tokens = Exp ast
    where (ast, _) = parseExpr tokens

parseDec :: [Token] -> (Statement, [Token])
parseDec [Int, Ide x] = (Dec x, [])
parseDec (Int : Ide x : Equals : expression) = (Init x exp, tokens)
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

parseExpr' tokens ast = case tokens of
                             RParen:tokens -> (ast, [])
                             [] -> (ast, [])
                             _ -> error "parsing error"

parseTerm :: [Token] -> (ExpAst, [Token])
parseTerm tokens = (ast', tokens'')
    where (ast, tokens') = parseFactor tokens
          (ast', tokens'') = parseTerm' tokens' ast

parseTerm' :: [Token] -> ExpAst -> (ExpAst, [Token])
parseTerm' (BinOp Times : tokens) ast = parseTerm' tokens' (ExpNode Times ast ast')
    where (ast', tokens') = parseFactor tokens

parseTerm' (BinOp Divide : tokens) ast = parseTerm' tokens' (ExpNode Divide ast ast')
    where (ast', tokens') = parseFactor tokens

parseTerm' (t : tokens) ast =
    case t of
         BinOp Plus -> (ast, t:tokens)
         BinOp Minus -> (ast, t:tokens)
         RParen -> (ast, t:tokens)
         _ -> error "parsing error"

parseTerm' [] ast = (ast, [])

parseFactor :: [Token] -> (ExpAst, [Token])
parseFactor (LParen : tokens) = (ast, tokens')
    where (ast, tokens') = parseExpr tokens
          
parseFactor (Num a : tokens) = (ValNode a, tokens)
parseFactor _ = error "parse error"


