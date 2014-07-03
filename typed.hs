{-# LANGUAGE TupleSections #-}

import Text.Parsec hiding (many, (<|>))
import Text.Parsec.String
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad

---Data and formatting

data Term =
	TmVar Int |
	TmAbs String Term |
	TmApp Term Term |
	TmTest Term Term Term |
	TmTrue |
	TmFalse 

type Context = [String]

pickFreshName ctx name
	| name `elem` ctx = pickFreshName ctx (name ++ "'")
	| otherwise = (name, name:ctx)

fmtTerm :: Context -> Term -> String
fmtTerm ctx (TmAbs name t1) =
	"(\\" ++ name' ++ "." ++ fmtTerm ctx' t1 ++ ")"
	where
		(name', ctx') = pickFreshName ctx name
fmtTerm ctx (TmApp t1 t2) = fmtTerm ctx t1 ++ " " ++ fmtTerm ctx t2
fmtTerm ctx (TmVar x) = ctx !! x
fmtTerm ctx (TmTest t1 t2 t3) = "If " ++ fmtTerm ctx t1 ++ " Then " ++ fmtTerm ctx t2 ++ " Else " ++ fmtTerm ctx t3
fmtTerm ctx TmTrue = "True"
fmtTerm ctx TmFalse = "False"

---Parser

pInLine :: Context -> Parser (Maybe String, Term)
pInLine ctx = (pAssign ctx <|> fmap (Nothing,) (pApp ctx))

pAssign ctx = do
	char ':'
	name <- many1 lower
	spaces
	char '='
	spaces
	term <- pApp ctx
	return (Just name, term)

--pApp is the top level parser. Every term is some for of application.
--Even if there is no application at all, it will be parsed as 'something'
--applied to nothing.
pApp ctx = do
	apps <- sepBy1 (pTerm ctx) spaces
	return $ foldl1 TmApp apps

--What we call a Term is actuall every term which is not an application.
--This is to avoid left recursion in the pApp parser. To allow for nested 
--applications we have the pPar parser
pTerm :: Context -> Parser Term
pTerm ctx = pBool ctx <|> pTest ctx <|> pAbs ctx <|> pVar ctx <|> pPar ctx

--This parser allows for nested applications.
pPar ctx = between (char '(') (char ')') (pApp ctx)

pVar ctx = do
	name <- many1 lower
	let brujin = elemIndex name ctx
	maybe (fail  $ "Unbound var: " ++ name) (return . TmVar) brujin

pAbs ctx = do
	char '\\'
	b <- many1 lower
	char '.' >>  many space
	t <- pTerm (b:ctx)
	return (TmAbs b t)

pTest ctx = TmTest <$>
	((string "If" >> spaces) *> pTerm ctx <* spaces) <*>
	((string "Then" >> spaces) *> pTerm ctx <* spaces) <*>
	((string "Else" >> spaces) *> pTerm ctx)

pBool ctx = (string "True" *> pure TmTrue) <|> (string "False" *> pure TmFalse)

---Evaluation

shift :: Int -> Int -> Term -> Term
shift d c (TmVar x)
	| x < c = TmVar x
	| x >= c = TmVar (x + d)
shift d c (TmAbs name t) = TmAbs name (shift d (c+1) t)
shift d c (TmApp t1 t2) = TmApp (shift d c t1) (shift d c t2)
shift d c (TmTest t1 t2 t3) = TmTest
	(shift d c t1)
	(shift d c t2)
	(shift d c t3)
shift d c x = x

subst :: Int -> Term -> Term -> Term
subst j s (TmVar x)
	| x == j = s
	| otherwise = TmVar x
subst j s (TmAbs name t) = TmAbs name (subst (j+1) (shift 1 0 s) t)
subst j s (TmApp t1 t2) = TmApp (subst j s t1) (subst j s t2)
subst j s (TmTest t1 t2 t3) = TmTest
	(subst j s t2)
	(subst j s t2)
	(subst j s t3)
subst j s x = x

isValue :: Term -> Bool
isValue (TmAbs _ _) = True
isValue TmTrue = True
isValue TmFalse = True
isValue _ = False

recEval :: Term -> Term
recEval (TmApp (TmAbs name t1) t2)
	| isValue t2 = recEval $ shift (-1) 0 (subst 0 (shift 1 0 t2) t1)
	| otherwise = recEval $ TmApp (TmAbs name t1) (recEval t2)
recEval (TmApp t1 t2) = recEval $ TmApp (recEval t1) t2
recEval (TmTest TmTrue t2 t3) = recEval t2
recEval (TmTest TmFalse t2 t3) = recEval t3
recEval (TmTest t1 t2 t3) = recEval $ TmTest (recEval t1) t2 t3
recEval t = t

---REPL

contextWrap name ctxTerm term  = TmApp (TmAbs name term) ctxTerm

repl :: Context -> (Term -> Term) -> IO ()
repl ctxNames ctx = do
	line <- getLine
	case (parse (pInLine ctxNames) line line) of
		(Right (Nothing, term)) -> do
			(putStrLn . fmtTerm ctxNames . recEval) (ctx term)
			repl ctxNames ctx
		(Right (Just name, term)) -> do
			let term' = (recEval (ctx term))
			repl (name:ctxNames) (ctx . contextWrap name term')
		(Left r) -> putStrLn (show r) >> repl ctxNames ctx

main = repl [] id
