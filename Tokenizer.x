{
module Tokenizer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
	$white+ ;
	\\ {\s -> ToLambda}
	: {\s -> ToColon}
	\. {\s -> ToDot}
	\( {\s -> ToOpenParan}
	\) {\s -> ToCloseParan}
	let {\s -> ToLet}
	if {\s -> ToIf}
	then {\s -> ToThen}
	else {\s -> ToElse}
	\-> {\s -> ToArr}
	\= {\s -> ToEq}
	Nat {\s -> ToNat}
	Bool {\s -> ToBool}
	$alpha+ {\s -> ToIdent s}

{
data Token =
	ToLambda |
	ToColon |
	ToDot |
	ToLet |
	ToArr |
	ToEq |
	ToOpenParan |
	ToCloseParan |
	ToIf |
	ToThen |
	ToElse |
	ToNat |
	ToBool |
	ToIdent String
	deriving (Eq, Show)
}
