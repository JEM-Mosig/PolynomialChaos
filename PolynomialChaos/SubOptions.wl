(* ::Package:: *)

BeginPackage["SubOptions`"]


Begin["SubOptions`"]


(* ::Section:: *)
(*Public Definitions*)


ClearAll[
	SubOption
]


(* ::Subsection:: *)
(*SubOption*)


SubOption::usage = "SubOption[OptionValue[tag], subtag] returns the value of the suboption subtag of the option tag.";

SyntaxInformation[SubOption] = {
	"ArgumentsPattern" -> {__}
};


(* ::Section:: *)
(*Private Definitions*)


Begin["`Private`"]


(* ::Subsection:: *)
(*SubOption*)


SubOption::inv = "Invalid argument(s).";


SubOption[{main_String, subs__Rule}] := main
SubOption[{main_String, subs__Rule}, name_] := name /. {subs} /. name -> Automatic
SubOption[{main_String, subs__Rule}, name_, default_] := name /. {subs} /. name -> default
SubOption[main_String] := main
SubOption[main_String, name_] := Automatic
SubOption[main_String, name_, default_] := default

SubOption[{subs__Rule}, name_] := name /. {subs} /. name -> Automatic
SubOption[{subs__Rule}, name_, default_] := name /. {subs} /. name -> default

SubOption[{main_Symbol, subs__Rule}] := main
SubOption[{main_Symbol, subs__Rule}, name_] := name /. {subs} /. name -> Automatic
SubOption[{main_Symbol, subs__Rule}, name_, default_] := name /. {subs} /. name -> default
SubOption[main_Symbol] := main
SubOption[main_Symbol, name_] := Automatic
SubOption[main_Symbol, name_, default_] := default

SubOption[___] := (
	Message[SubOption::inv];
	$Failed
)


(* ::Section:: *)
(*End Package*)


End[] (* `Private` *)


End[] (* SubOptions` *)


EndPackage[] (* SubOptions` *)
