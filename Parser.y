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
	: let identifier ':' Type '=' Term
		{\ctx -> (Just $2, TmFix (TmAbs $2 ($6 ($2:ctx)) $4))}
	| Term
		{\ctx -> (Nothing, $1 ctx)}

Term
	: lam identifier ':' Type '.' Term
		{\ctx -> TmAbs $2 ($6 ($2:ctx)) $4}
	| '(' Term ')'
		{$2}
	| if Term then Term else Term
		{\ctx -> TmTest ($2 ctx) ($4 ctx) ($6 ctx)}
	| Term Term
		{\ctx -> TmApp ($1 ctx) ($2 ctx)}
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
