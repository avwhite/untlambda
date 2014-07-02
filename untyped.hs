{-# LANGUAGE TupleSections #-}

import Text.Parsec hiding (many, (<|>))
import Text.Parsec.String
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.Reader

---Data and formatting

data Term =
	TmVar Int |
	TmAbs String Term |
	TmApp Term Term

type Context = [String]

pickFreshName name ctx
	| name `elem` ctx = pickFreshName (name ++ "'") ctx
	| otherwise = (name, name:ctx)

fmtTerm :: Term -> Reader Context String
fmtTerm (TmAbs name t1) = do
	(name', ctx) <- asks (pickFreshName name)
	return "(\\" ++ name' ++ "." ++ local (fmtTerm t1) ctx ++ ")"
fmtTerm (TmApp t1 t2) = do
	first <- fmtTerm t1
	second <- fmtTerm t2
	return (first ++ " " ++ second)
fmtTerm (TmVar x) = asks (!! x)

---Parser

pInLine :: Context -> Parser (Maybe String, Term)
pInLine ctx = (pAssign ctx <|> fmap (Nothing,) (pTerm ctx))

pAssign ctx = do
	char ':'
	name <- many1 lower
	spaces
	char '='
	spaces
	term <- pTerm ctx
	return (Just name, term)

pTerm :: Context -> Parser Term
pTerm ctx = pAbs ctx <|> pApp ctx

pPar ctx = between (char '(') (char ')') (pTerm ctx)

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

pApp ctx = do
	apps <- sepBy1 (pAbs ctx <|> pVar ctx <|> pPar ctx) spaces
	return $ foldl1 TmApp apps

---Evaluation

shift :: Int -> Int -> Term -> Term
shift d c (TmVar x)
	| x < c = TmVar x
	| x >= c = TmVar (x + d)
shift d c (TmAbs name t) = TmAbs name (shift d (c+1) t)
shift d c (TmApp t1 t2) = TmApp (shift d c t1) (shift d c t2)

subst :: Int -> Term -> Term -> Term
subst j s (TmVar x)
	| x == j = s
	| otherwise = TmVar x
subst j s (TmAbs name t) = TmAbs name (subst (j+1) (shift 1 0 s) t)
subst j s (TmApp t1 t2) = TmApp (subst j s t1) (subst j s t2)

isValue :: Term -> Bool
isValue (TmAbs _ _) = True
isValue _ = False

recEval :: Term -> Term
recEval (TmApp (TmAbs name t1) t2)
	| isValue t2 = recEval $ shift (-1) 0 (subst 0 (shift 1 0 t2) t1)
	| otherwise = recEval $ TmApp (TmAbs name t1) (recEval t2)
recEval (TmApp t1 t2) = recEval $ TmApp (recEval t1) t2
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
