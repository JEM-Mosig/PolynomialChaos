(* ::Package:: *)

BeginPackage["FastIntegrate`"]


Begin["FastIntegrate`"]


(* ::Section:: *)
(*Public Definitions*)


ClearAll[
	IntegratePolynomial
]


(* ::Subsection:: *)
(*IntegratePolynomial*)


IntegratePolynomial::usage = "IntegratePolynomial[expr, x] works just like Integrate[expr, x], but only for polynomials.";

SyntaxInformation[IntegratePolynomial] = {
	"ArgumentsPattern" -> {_, __},
	"LocalVariables" -> {"Integrate", {2, Infinity}}
};


(* ::Section:: *)
(*Private Definitions*)


Begin["`Private`"];


(* ::Subsection:: *)
(*IntegratePolynomial*)


(* ::Text:: *)
(*Indefinite integral over polynomials in x:*)


IntegratePolynomial[expr_, x_Symbol] := Total@MapIndexed[
	With[{e = First[#2] - 1}, #1 x^(e+1)/(e+1)]&,
	CoefficientList[expr, x]
] /; PolynomialQ[expr, x]


(* ::Text:: *)
(*Definite integral:*)


IntegratePolynomial[expr_, {x_, xmin_, xmax_}] := With[{i = IntegratePolynomial[expr,x]},
	(i /. x -> xmax) - (i /. x -> xmin)
] /; PolynomialQ[expr, x]


(* ::Text:: *)
(*Integration over multiple variables:*)


IntegratePolynomial[expr_, var1_, varx__] := Fold[
	IntegratePolynomial, expr, {var1, varx}
] /; PolynomialQ[expr, First/@{var1, varx}]


(* ::Section:: *)
(*End Package*)


End[] (* `Private` *)


End[] (* FastIntegrate` *)


EndPackage[]; (* FastIntegrate` *)
