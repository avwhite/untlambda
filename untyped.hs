import Text.Parsec hiding (many, (<|>))
import Text.Parsec.String
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad

data Term =
	TmVar Int |
	TmAbs String Term |
	TmApp Term Term

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

-------Parser
pTerm :: Context -> Parser Term
pTerm ctx = pAbs ctx <|> pApp ctx

pPar ctx = between (char '(') (char ')') (pTerm ctx)

pVar ctx = do
	name <- lower
	let brujin = elemIndex [name] ctx
	maybe (fail "Unbound var") (return . TmVar) brujin

pAbs ctx = do
	char '\\'
	b <- lower
	char '.' >>  many space
	t <- pTerm ([b]:ctx)
	return (TmAbs [b] t)

pApp ctx = do
	apps <- many1 (pVar ctx <|> pPar ctx)
	return $ foldl1 TmApp apps

stdCtx = ["a","b","c","x","y","z"]

repl = forever $ do
	line <- getLine
	case (parse (pTerm stdCtx) line line) of
		(Right r) -> do
			putStrLn . fmtTerm [] . recEval $ r
		(Left r) -> putStrLn (show r)

main = repl
