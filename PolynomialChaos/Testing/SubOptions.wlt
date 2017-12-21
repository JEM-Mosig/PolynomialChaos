PrintTemporary["Testing SubOption symbol..."];


VerificationTest[
  Needs["PolynomialChaos`"],
  Null,
  TestID -> "Load:MultiIndex"
]


Begin["PolynomialChaos`SubOptions`"]


VerificationTest[
	Module[{f},
		Options[f] = {Method -> {"Fast", "a" -> 1, "b" -> 2}};
		f[OptionsPattern[]] := SubOption[OptionValue[Method], "a"];
		f[]
	]
	,
	1
]


VerificationTest[
	Module[{f},
		Options[f] = {Method -> {"Fast", "a" -> 1, "b" -> 2}};
		f[OptionsPattern[]] := SubOption[OptionValue[Method], "b"];
		f[]
	]
	,
	2
]


VerificationTest[
	Module[{f},
		Options[f] = {Method -> {"Fast", "a" -> 1, "b" -> 2}};
		f[OptionsPattern[]] := SubOption[OptionValue[Method], "c"];
		f[]
	]
	,
	Automatic
]


VerificationTest[
	Module[{f},
		Options[f] = {Method -> {"Fast", "a" -> 1, "b" -> 2}};
		f[OptionsPattern[]] := SubOption[OptionValue[Method]];
		f[]
	]
	,
	"Fast"
]


VerificationTest[
	Module[{f},
		Options[f] = {Method -> {"a" -> 1, "b" -> 2}};
		f[OptionsPattern[]] := SubOption[OptionValue[Method]];
		f[]
	]
	,
	$Failed
	,
	{SubOption::inv}
]


VerificationTest[
	Module[{f},
		Options[f] = {Method -> {"a" -> 1, "b" -> 2}};
		f[OptionsPattern[]] := SubOption[OptionValue[Method], "a"];
		f[]
	]
	,
	1
]


VerificationTest[
	Module[{f},
		Options[f] = {Method -> {"a" -> 1, "b" -> 2}};
		f[OptionsPattern[]] := SubOption[OptionValue[Method], "c"];
		f[]
	]
	,
	Automatic
]


VerificationTest[
	Module[{f},
		Options[f] = {Method -> {"a" -> 1, "b" -> 2}};
		f[OptionsPattern[]] := SubOption[OptionValue[Method], "c", \[Pi]];
		f[]
	]
	,
	\[Pi]
]


End[] (* PolynomialChaos`SubOptions` *)