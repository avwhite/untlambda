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
	TmAbs String Term Ty |
	TmApp Term Term |
	TmTest Term Term Term |
	TmTrue |
	TmFalse |
	TmZero |
	TmSucc Term |
	TmPred Term |
	TmIsZero Term  deriving (Show)

data Ty =
	TyBool |
	TyArr Ty Ty |
	TyNat deriving (Show, Eq)

type Context = [String]
type TyContext = [Ty]

pickFreshName ctx name
	| name `elem` ctx = pickFreshName ctx (name ++ "'")
	| otherwise = (name, name:ctx)

fmtTerm :: Context -> Term -> String
fmtTerm ctx (TmAbs name t1 ty) =
	"(\\" ++ name' ++ ":" ++ show ty ++ "." ++ fmtTerm ctx' t1 ++ ")"
	where
		(name', ctx') = pickFreshName ctx name
fmtTerm ctx (TmApp t1 t2) = fmtTerm ctx t1 ++ " " ++ fmtTerm ctx t2
fmtTerm ctx (TmVar x) = ctx !! x
fmtTerm ctx (TmTest t1 t2 t3) = "If " ++ fmtTerm ctx t1 ++ " Then " ++ fmtTerm ctx t2 ++ " Else " ++ fmtTerm ctx t3
fmtTerm ctx TmTrue = "True"
fmtTerm ctx TmFalse = "False"
fmtTerm ctx TmZero = "0"
fmtTerm ctx (TmSucc t) = "Succ " ++ fmtTerm ctx t
fmtTerm ctx (TmPred t) = "Pred " ++ fmtTerm ctx t
fmtTerm ctx (TmIsZero t) = "Iszero " ++ fmtTerm ctx t

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
pTerm ctx =
	try (pBool ctx) <|>
	try (pTest ctx) <|>
	try (pAbs ctx) <|>
	try (pZero ctx) <|>
	try (pSucc ctx) <|>
	try (pPred ctx) <|>
	try (pIsZero ctx) <|>
	try (pVar ctx) <|>
	try (pPar ctx)

--This parser allows for nested applications.
pPar ctx = between (char '(') (char ')') (pApp ctx)

pVar ctx = do
	name <- many1 lower
	let brujin = elemIndex name ctx
	maybe (fail  $ "Unbound var: " ++ name) (return . TmVar) brujin

pAbs ctx = do
	char '\\'
	b <- many1 lower
	char ':'
	ty <- pType
	char '.' >>  many space
	t <- pApp (b:ctx)
	return (TmAbs b t ty)

pTest ctx = TmTest <$>
	((string "If" >> spaces) *> pTerm ctx <* spaces) <*>
	((string "Then" >> spaces) *> pTerm ctx <* spaces) <*>
	((string "Else" >> spaces) *> pTerm ctx)

pBool ctx = (string "True" *> pure TmTrue) <|> (string "False" *> pure TmFalse)

pZero ctx = char '0' >> return TmZero

pSucc ctx = string "Succ " >> TmSucc <$> pTerm ctx

pPred ctx = string "Pred " >> TmPred <$> pTerm ctx

pIsZero ctx = string "IsZero " >> TmIsZero <$> pTerm ctx

pType = do
	arrs <- sepBy1 (pTyBool <|> pTyPar <|> pTyNat) (string "->")
	return $ foldr1 TyArr arrs

pTyBool = string "Bool" >> return TyBool

pTyNat = string "Nat" >> return TyNat

pTyPar = between (char '(') (char ')') pType

---Type checking

typeof :: TyContext -> Term -> Maybe Ty
typeof ctx (TmVar x) = return $ ctx !! x
typeof ctx (TmAbs x t ty) = do
	ty2 <- typeof (ty:ctx) t
	return $ TyArr ty ty2
typeof ctx (TmApp t1 t2) = do
	ty1 <- typeof ctx t1
	ty2 <- typeof ctx t2
	case ty1 of
		(TyArr ty11 ty12) ->
			if ty2 /= ty11 then Nothing else
				return ty12
		_ -> Nothing
typeof ctx TmTrue = return TyBool
typeof ctx TmFalse = return TyBool
typeof ctx (TmTest t1 t2 t3) = do
	ty1 <- typeof ctx t1
	ty2 <- typeof ctx t2
	ty3 <- typeof ctx t3
	if ty1 /= TyBool then Nothing else
		if ty2 /= ty3 then Nothing else
			return ty2
typeof ctx TmZero = return TyNat
typeof ctx (TmSucc t) = do
	ty <- typeof ctx t
	case ty of
		TyNat -> return TyNat
		_ -> Nothing
typeof ctx (TmPred t) = do
	ty <- typeof ctx t
	case ty of
		TyNat -> return TyNat
		_ -> Nothing
typeof ctx (TmIsZero t) = do
	ty <- typeof ctx t
	case ty of
		TyNat -> return TyBool
		_ -> Nothing

---Evaluation

shift :: Int -> Int -> Term -> Term
shift d c (TmVar x)
	| x < c = TmVar x
	| x >= c = TmVar (x + d)
shift d c (TmAbs name t ty) = TmAbs name (shift d (c+1) t) ty
shift d c (TmApp t1 t2) = TmApp (shift d c t1) (shift d c t2)
shift d c (TmTest t1 t2 t3) = TmTest
	(shift d c t1)
	(shift d c t2)
	(shift d c t3)
shift d c (TmSucc t) = (TmSucc (shift d c t))
shift d c (TmPred t) = (TmPred (shift d c t))
shift d c (TmIsZero t) = (TmIsZero (shift d c t))
shift d c x = x

subst :: Int -> Term -> Term -> Term
subst j s (TmVar x)
	| x == j = s
	| otherwise = TmVar x
subst j s (TmAbs name t ty) = TmAbs name (subst (j+1) (shift 1 0 s) t) ty
subst j s (TmApp t1 t2) = TmApp (subst j s t1) (subst j s t2)
subst j s (TmTest t1 t2 t3) = TmTest
	(subst j s t2)
	(subst j s t2)
	(subst j s t3)
subst j s (TmSucc t) = TmSucc (subst j s t)
subst j s (TmPred t) = TmPred (subst j s t)
subst j s (TmIsZero t) = TmIsZero (subst j s t)
subst j s x = x

isNumValue :: Term -> Bool
isNumValue TmZero = True
isNumValue (TmSucc t) = isNumValue t
isNumValue _ = False

isValue :: Term -> Bool
isValue (TmAbs _ _ _) = True
isValue TmTrue = True
isValue TmFalse = True
isValue TmZero = True
isValue (TmSucc t) = isValue t
isValue x = isNumValue x

unSucc :: Term -> Term
unSucc (TmSucc t) = t
unSucc _ = error "Something is really wrong"

recEval :: Term -> Term
recEval (TmApp (TmAbs name t1 ty) t2)
	| isValue t2 = recEval $ shift (-1) 0 (subst 0 (shift 1 0 t2) t1)
	| otherwise = recEval $ TmApp (TmAbs name t1 ty) (recEval t2)
recEval (TmApp t1 t2) = recEval $ TmApp (recEval t1) t2
recEval (TmTest TmTrue t2 t3) = recEval t2
recEval (TmTest TmFalse t2 t3) = recEval t3
recEval (TmTest t1 t2 t3) = recEval $ TmTest (recEval t1) t2 t3
recEval (TmSucc t) = TmSucc (recEval t)
recEval (TmPred TmZero) = TmZero
recEval (TmPred t)
	| isNumValue t = recEval $ unSucc t --Since t is a numeric value and not zero it must be a TmSucc
	| otherwise = recEval $ TmPred (recEval t)
recEval (TmIsZero TmZero) = TmTrue
recEval (TmIsZero t)
	| isNumValue t = TmFalse
	| otherwise = recEval $ TmIsZero (recEval t)
recEval t = t

---REPL

contextWrap name ctxTerm ty term  = TmApp (TmAbs name term ty) ctxTerm

repl :: Context -> (Term -> Term) -> IO ()
repl ctxNames ctx = do
	line <- getLine
	case (parse (pInLine ctxNames) line line) of
		(Right (Nothing, term)) ->
			case typeof [] (ctx term) of
				(Just ty) -> do
					(putStrLn . fmtTerm ctxNames . recEval) (ctx term)
					repl ctxNames ctx
				Nothing -> putStrLn "Type error" >> repl ctxNames ctx
		(Right (Just name, term)) ->
			case typeof [] (ctx term) of
				(Just ty) -> do
					let term' = (recEval (ctx term))
					repl (name:ctxNames) (ctx . contextWrap name term' ty)
				Nothing -> putStrLn "Type error" >> repl ctxNames ctx
		(Left r) -> putStrLn (show r) >> repl ctxNames ctx

main = repl [] id
