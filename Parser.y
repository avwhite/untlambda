{
module Parser where

import Tokenizer
import Data.List
}

%name lambdaParse
%tokentype {Token}
%error {parseError}

%token
	lam {ToLambda}
	':' {ToColon}
	'.' {ToDot}
	'(' {ToOpenParan}
	')' {ToCloseParan}
	let {ToLet}
	if {ToIf}
	then {ToThen}
	else {ToElse}
	'->' {ToArr}
	'=' {ToEq}
	Nat {ToNat}
	Bool {ToBool}
	identifier {ToIdent $$}

%%

InLine
	: let identifier ':' Type '=' Juxt
		{\ctx -> (Just $2, TmFix (TmAbs $2 ($6 ($2:ctx)) $4))}
	| Juxt
		{\ctx -> (Nothing, $1 ctx)}

Juxt
	: Juxt Term
		{\ctx -> TmApp ($1 ctx) ($2 ctx)}
	| Term
		{$1}

Term
	: lam identifier ':' Type '.' Juxt
		{\ctx -> TmAbs $2 ($6 ($2:ctx)) $4}
	| '(' Juxt ')'
		{$2}
	| if Juxt then Juxt else Juxt
		{\ctx -> TmTest ($2 ctx) ($4 ctx) ($6 ctx)}
	| identifier
		{
		\ctx -> maybe
			(error "Unbound Var")
			(TmVar)
			(elemIndex $1 ctx)
		}

Type
	: Nat
		{TyNat}
	| Bool
		{TyBool}
	| Type '->' Type
		{TyArr $1 $3}
	| '(' Type ')'
		{$2}


{

parseError _ = error "Parse error"

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
	TmIsZero Term |
	TmFix Term deriving (Show)

data Ty =
	TyBool |
	TyArr Ty Ty |
	TyNat deriving (Show, Eq)

}
