{-# LANGUAGE TupleSections #-}

import Text.Parsec hiding (many, (<|>))
import Text.Parsec.String
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
import System.Console.Readline

import Parser
import Tokenizer

---Data and formatting

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
fmtTerm ctx TmTrue = "true"
fmtTerm ctx TmFalse = "false"
fmtTerm ctx TmZero = "z"
fmtTerm ctx (TmSucc t) = "succ " ++ fmtTerm ctx t
fmtTerm ctx (TmPred t) = "pred " ++ fmtTerm ctx t
fmtTerm ctx (TmIsZero t) = "isz " ++ fmtTerm ctx t
fmtTerm ctx (TmFix t) = "lolwut"

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
typeof ctx (TmFix t) = do
	ty <- typeof ctx t
	case ty of
		TyArr t1 t2 -> if t1 == t2 then return t1 else Nothing
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
shift d c (TmFix t) = (TmFix (shift d c t))
shift d c x = x

subst :: Int -> Term -> Term -> Term
subst j s (TmVar x)
	| x == j = s
	| otherwise = TmVar x
subst j s (TmAbs name t ty) = TmAbs name (subst (j+1) (shift 1 0 s) t) ty
subst j s (TmApp t1 t2) = TmApp (subst j s t1) (subst j s t2)
subst j s (TmTest t1 t2 t3) = TmTest
	(subst j s t1)
	(subst j s t2)
	(subst j s t3)
subst j s (TmSucc t) = TmSucc (subst j s t)
subst j s (TmPred t) = TmPred (subst j s t)
subst j s (TmIsZero t) = TmIsZero (subst j s t)
subst j s (TmFix t) = TmFix (subst j s t)
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
recEval (TmFix (TmAbs name t ty)) =
	recEval $ subst 0 (TmFix (TmAbs name t ty)) t
recEval (TmFix t) = recEval $ TmFix (recEval t)
recEval t = t

---REPL

contextWrap name ctxTerm ty term  = TmApp (TmAbs name term ty) ctxTerm

startCtx =
	(contextWrap "succ"
		(TmAbs "n" (TmSucc (TmVar 0)) TyNat)
		(TyArr TyNat TyNat)) 
	. (contextWrap "pred"
		(TmAbs "n" (TmPred (TmVar 0)) TyNat)
		(TyArr TyNat TyNat)) 
	. (contextWrap "isz"
		(TmAbs "n" (TmIsZero (TmVar 0)) TyNat)
		(TyArr TyNat TyBool))
	. (contextWrap "zero"
		TmZero
		TyNat)
	. (contextWrap "false"
		TmFalse
		TyBool)
	. (contextWrap "true"
		TmTrue
		TyBool)

repl :: Context -> (Term -> Term) -> IO ()
repl ctxNames ctx = do
	(Just line) <- readline ">"
	case (lambdaParse . alexScanTokens $ line) ctxNames of
		(Nothing, term) -> putStrLn (show term) >>
			case typeof [] (ctx term) of
				(Just ty) -> do
					(putStrLn . fmtTerm ctxNames . recEval) (ctx term)
					repl ctxNames ctx
				Nothing -> putStrLn "Type error" >> repl ctxNames ctx
		(Just name, term) -> putStrLn (show term) >>
			case typeof [] (ctx term) of
				(Just ty) -> do
					let term' = recEval $ ctx term
					repl (name:ctxNames) (ctx . contextWrap name term' ty)
				Nothing -> putStrLn "Type error" >> repl ctxNames ctx

main = repl ["true", "false", "z", "isz", "pred", "succ"] startCtx
