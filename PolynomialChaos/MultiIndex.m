(* ::Package:: *)

BeginPackage["PolynomialChaos`MultiIndex`"]


Begin["PolynomialChaos`MultiIndex`"]


(* ::Section:: *)
(*Public Definitions*)


ClearAll[
	MultiIndexList,
	MultiIndexMapping,
	NextPermutation
]


(* ::Subsection:: *)
(*MultiIndexList*)


MultiIndexList::usage = "MultiIndexList[m,n] generates a list of m-indices with entries from 0 to n in graded lexicographic order.";

Options[MultiIndexList] = {
	Method -> "NormMax", 
	Ordering -> "GradedLexicographic"
};

SyntaxInformation[MultiIndexList] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}
};


(* ::Subsection:: *)
(*MultiIndexMapping*)


MultiIndexMapping::usage = "MultiIndexMapping[m,n] generates a list of rules, mapping single-indices (integers) to m-indices with entries from 0 to n in graded lexicographic order.";

Options[MultiIndexMapping] = {
	Method -> "NormMax", 
	Ordering -> "GradedLexicographic"
};

SyntaxInformation[MultiIndexMapping] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}
};


(* ::Subsection:: *)
(*NextPermutation*)


NextPermutation::usage = "NextPermuation[p] gives the permutation following p in lexicographic order.";

SyntaxInformation[NextPermutation] = {
	"ArgumentsPattern" -> {_}
};


(* ::Section:: *)
(*Sub-Module: OrderingFunctions*)


Begin["`OrderingFunctions`"]


LexicographicOrderingFunction[a_, b_] :=
With[{la = Plus@@a, lb = Plus@@b},
	(* sort by difference of the first non-matching subindex *)
	SelectFirst[a - b, (# != 0)&] > 0
]


GradedLexicographicOrderingFunction[a_, b_] :=
With[{la = Plus@@a, lb = Plus@@b},
	If[la!=lb,
		(* first sort by norm of the index *)
		lb > la,
		(* then sort by difference of the first non-matching subindex *)
		SelectFirst[a - b, (# != 0)&] > 0
	]
]


GradedReverseLexicographicOrderingFunction[a_, b_] :=
With[{la = Plus@@a, lb = Plus@@b},
	If[la!=lb,
		(* first sort by norm of the index *)
		lb > la,
		(* then sort by difference of the last non-matching subindex *)
		Not[SelectFirst[Reverse[a - b], (# != 0)&] > 0]
	]
]


End[] (* `OrderingFunctions` *)


(* ::Section:: *)
(*Private Definitions*)


Begin["`Private`"];


(* ::Subsection:: *)
(*MultiIndexList*)


MultiIndexList[length_, n_, OptionsPattern[]] :=
With[{
	array = Switch[OptionValue[Method], 
		"NormMax", Select[Tuples[Array[Identity, n + 1, 0], length], (Plus@@# <= n)&],
		"EachMax", Tuples[Array[Identity, n + 1, 0], length]
	],
	orderFun = Switch[OptionValue[Ordering], 
		"GradedLexicographic", MultiIndex`OrderingFunctions`GradedLexicographicOrderingFunction,
		"GradedReverseLexicographic", MultiIndex`OrderingFunctions`GradedReverseLexicographicOrderingFunction,
		"Lexicographic", MultiIndex`OrderingFunctions`LexicographicOrderingFunction,
		_, OptionValue[Ordering]
	]
},
	Sort[array, orderFun]
]


(* ::Subsection:: *)
(*MultiIndexMapping*)


MultiIndexMapping[length_, n_, options:OptionsPattern[]] :=
MapIndexed[
	(First[#2] -> #1)&,
	MultiIndexList[length, n, options]
]


(* ::Subsection:: *)
(*NextPermutation*)


(* this code is copied out of the Combinatorica package *)

NextPermutation[l_List] := Sort[l] /; l === Reverse[Sort[l]]

NextPermutation[l_List] := Module[{n = Length[l], i, j, t, nl = l},
	i = n-1;
	While[Order[nl[[i]], nl[[i+1]]] == -1, i--];
	j=n;
	While[Order[nl[[j]], nl[[i]]] == 1, j--];
	{nl[[i]], nl[[j]]} = {nl[[j]], nl[[i]]};
	Join[Take[nl, i], Reverse[Drop[nl, i]]]
]


(* ::Section:: *)
(*End Package*)


End[] (* `Private` *)


End[] (* PolynomialChaos`MultiIndex` *)


EndPackage[]; (* PolynomialChaos`MultiIndex` *)
