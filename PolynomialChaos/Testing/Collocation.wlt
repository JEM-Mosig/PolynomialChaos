PrintTemporary["Testing Collocation symbol..."];


VerificationTest[
	Needs["PolynomialChaos`"];
  (* load the following for comparison: *)
  Needs["NumericalDifferentialEquationAnalysis`"],
	Null
]


VerificationTest[
	Collocation[0, {x, UniformDistribution[]}, 3]["MomentList"]
	,
	{0, 0}
	,
	SameTest -> Equal
]


VerificationTest[
	Collocation[x, {x, UniformDistribution[]}, 3]["MomentList"]
	,
	{1/2, 1/12} (* = Integrate[{x, (x - 1/2)^2}, {x, 0, 1}] *)
	,
	SameTest -> Equal
]


VerificationTest[
	Collocation[x, {x, UniformDistribution[]}, {y, BetaDistribution[2, 3]}, 3]["MomentList"]
	,
	{1/2, 1/12} (* = Integrate[{x, (x - 1/2)^2}, {x, 0, 1}] *)
	,
	SameTest -> Equal
]


VerificationTest[
	Collocation[x, {x, UniformDistribution[{-1, 1}]}, {y, BetaDistribution[2, 3]}, 3]["MomentList"] // Chop
	,
	{0, 1/3} (* = Integrate[{x, (x - 0)^2}/2, {x, 0, 1}] *)
	,
	SameTest -> Equal
]


(* ::Subsection:: *)
(*Quadrature Rules (ConstructRule)*)


(* ::Text:: *)
(*Compare QuadratureRules with constant weighting function to MMA inbuilt GaussianQuadratureWeights by \
solving an integral over some test function.*)


With[{order = 5, a = -1, b = 1, f = Function[{x}, Sin[1-x^3]+x^2]},
	VerificationTest[	
		Module[{x, w},
			{x, w} = ConstructRule[order, UniformDistribution[{a, b}]];
			Abs[b-a] w.f[x]
		]
		,
		Module[{x, w},
			{x, w} = Transpose[GaussianQuadratureWeights[order, a, b]];
			w.f[x]
		]
		,
		SameTest -> (Abs[#1 - #2] < 10^-10 &)
	]
]


With[{order = 5, a = -3, b = 2, f = Function[{x}, Sin[1-x^3]+x^2]},
	VerificationTest[	
		Module[{x, w, QR, rc},
			{x, w} = ConstructRule[order, UniformDistribution[{a, b}]];
			Abs[b-a]w.f[x]
		]
		,
		Module[{x, w},
			{x, w} = Transpose[GaussianQuadratureWeights[order, a, b]];
			w.f[x]
		]
		,
		SameTest -> (Abs[#1 - #2] < 10^-10 &)
	]
]


(* ::Subsection:: *)
(*Check against Expectation*)


With[{
		distr=TransformedDistribution[2 x - 1, x \[Distributed] BetaDistribution[10,10]],
		fun=3+\[Alpha]^2
	},
	VerificationTest[
		Module[{col,mma},
			col=Collocation[fun,{\[Alpha],distr},10,
				Abs->False,Listable->True,
				Method->{"PseudoSpectral","IntegrationRule"->Automatic}
			]["MomentList"];
			mma=With[{exp=Expectation[y,y\[Distributed]TransformedDistribution[fun,\[Alpha]\[Distributed]distr]]},
				{exp,Expectation[(y-exp)^2,y\[Distributed]TransformedDistribution[fun,\[Alpha]\[Distributed]distr]]}
			];
			Max[col-mma]<10^-12
		]
	]
]


With[{
		distr=TransformedDistribution[2 x - 1, x \[Distributed] BetaDistribution[10,10]],
		fun=3-\[Alpha]
	},
	VerificationTest[
		Module[{col,mma},
			col=Collocation[fun,{\[Alpha],distr},10,
				Abs->False,Listable->True,
				Method->{"PseudoSpectral","IntegrationRule"->Automatic}
			]["MomentList"];
			mma=With[{exp=Expectation[y,y\[Distributed]TransformedDistribution[fun,\[Alpha]\[Distributed]distr]]},
				{exp,Expectation[(y-exp)^2,y\[Distributed]TransformedDistribution[fun,\[Alpha]\[Distributed]distr]]}
			];
			Max[col-mma]<10^-12
		]
	]
]


With[{
		distr=TransformedDistribution[2 x - 1, x \[Distributed] BetaDistribution[10,10]],
		fun=1+Sin[\[Alpha]]
	},
	VerificationTest[
		Module[{col,mma},
			col=Collocation[fun,{\[Alpha],distr},10,
				Abs->False,Listable->True,
				Method->{"PseudoSpectral","IntegrationRule"->Automatic}
			]["MomentList"];
			mma=With[{exp=NExpectation[y,y\[Distributed]TransformedDistribution[fun,\[Alpha]\[Distributed]distr]]},
				{exp,NExpectation[(y-exp)^2,y\[Distributed]TransformedDistribution[fun,\[Alpha]\[Distributed]distr]]}
			];
			Max[col-mma]<10^-12
		]
	]
]


With[{
		distr=UniformDistribution[{-1,1}],
		fun=1+Sin[\[Alpha]]
	},
	VerificationTest[
		Module[{col,mma},
			col=Collocation[fun,{\[Alpha],distr},10,
				Abs->False,Listable->True,
				Method->{"PseudoSpectral","IntegrationRule"->Automatic}
			]["MomentList"];
			mma=With[{exp=NExpectation[y,y\[Distributed]TransformedDistribution[fun,\[Alpha]\[Distributed]distr]]},
				{exp,NExpectation[(y-exp)^2,y\[Distributed]TransformedDistribution[fun,\[Alpha]\[Distributed]distr]]}
			];
			Max[col-mma]<10^-12
		]
	]
]


With[{
		distr=UniformDistribution[{0,1}],
		fun=1+Sin[\[Alpha]]
	},
	VerificationTest[
		Module[{col,mma},
			col=Collocation[fun,{\[Alpha],distr},10,
				Abs->False,Listable->True,
				Method->{"PseudoSpectral","IntegrationRule"->Automatic}
			]["MomentList"];
			mma=With[{exp=NExpectation[y,y\[Distributed]TransformedDistribution[fun,\[Alpha]\[Distributed]distr]]},
				{exp,NExpectation[(y-exp)^2,y\[Distributed]TransformedDistribution[fun,\[Alpha]\[Distributed]distr]]}
			];
			Max[col-mma]<10^-12
		]
	]
]


With[{
		distr=UniformDistribution[{-1,2}],
		fun=1+Sin[\[Alpha]]
	},
	VerificationTest[
		Module[{col,mma},
			col=Collocation[fun,{\[Alpha],distr},10,
				Abs->False,Listable->True,
				Method->{"PseudoSpectral","IntegrationRule"->Automatic}
			]["MomentList"];
			mma=With[{exp=NExpectation[y,y\[Distributed]TransformedDistribution[fun,\[Alpha]\[Distributed]distr]]},
				{exp,NExpectation[(y-exp)^2,y\[Distributed]TransformedDistribution[fun,\[Alpha]\[Distributed]distr]]}
			];
			Max[col-mma]<10^-12
		]
	]
]


With[{
		distr=GammaDistribution[1,2],
		fun=1+\[Alpha]
	},
	VerificationTest[
		Module[{col,mma},
			col=Collocation[fun,{\[Alpha],distr},3,
				Abs->False,Listable->True,
				Method->{"PseudoSpectral","IntegrationRule"->Automatic}
			]["MomentList"];
			mma=With[{exp=NExpectation[y,y\[Distributed]TransformedDistribution[fun,\[Alpha]\[Distributed]distr]]},
				{exp,NExpectation[(y-exp)^2,y\[Distributed]TransformedDistribution[fun,\[Alpha]\[Distributed]distr]]}
			];
			Max[col-mma]<10^-9
		]
	]
]
