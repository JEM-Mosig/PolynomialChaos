PrintTemporary["Testing GPCBasis symbol..."];


VerificationTest[
	Needs["PolynomialChaos`"],
	Null
]


VerificationTest[
	GPCBasis[{\[Alpha], UniformDistribution[{-1, 1}]}, 5]
	,
	{1, Sqrt[3] \[Alpha], 1/2 Sqrt[5] (-1+3 \[Alpha]^2), 1/2 Sqrt[7] (-3 \[Alpha]+5 \[Alpha]^3), 3/8 (3-30 \[Alpha]^2+35 \[Alpha]^4), 1/8 Sqrt[11] (15 \[Alpha]-70 \[Alpha]^3+63 \[Alpha]^5)}
	,
	SameTest -> (Simplify[#1] === Simplify[#2]&)
]


VerificationTest[
	With[{basis = GPCBasis[{x, UniformDistribution[{-5, \[Pi]}]}, 5]},
		PadRight[
			Table[
				Integrate[
					PDF[UniformDistribution[{-5, \[Pi]}], x] basis[[m]] basis[[n]],
					{x, -5, \[Pi]}
				],
				{m,1,4}, {n,1,m}
			],
			{4,4}
		]
	]
	,
	IdentityMatrix[4]
]


VerificationTest[
	With[{basis = GPCBasis[{x, BetaDistribution[2, 3]}, 5]},
		PadRight[
			Table[
				Integrate[
					PDF[BetaDistribution[2, 3], x] basis[[m]] basis[[n]],
					{x, 0, 1}
				],
				{m,1,4}, {n,1,m}
			],
			{4,4}
		]
	]
	,
	IdentityMatrix[4]
]


VerificationTest[
	GPCBasis[{x, TransformedDistribution[-1+2x, x\[Distributed]BetaDistribution[3,4]]}, 4]
	,
	{1,(1+7 x)/Sqrt[6],1/2 Sqrt[7/3] (-1+2 x+9 x^2),1/4 Sqrt[7/2] (-1-9 x+9 x^2+33 x^3),1/8 Sqrt[7/2] (3-12 x-66 x^2+44 x^3+143 x^4)}
	,
	SameTest -> (Simplify[#1 == #2]&)
]


VerificationTest[
	GPCBasis[{x, TransformedDistribution[-1+2x, x \[Distributed] BetaDistribution[3,4]]}, 4]
	,
	(* this result was constructed using default orthogonalization method *)
	{1,(1+7 x)/Sqrt[6],1/2 Sqrt[7/3] (-1+2 x+9 x^2),1/4 Sqrt[7/2] (-1-9 x+9 x^2+33 x^3),1/8 Sqrt[7/2] (3-12 x-66 x^2+44 x^3+143 x^4)}
	,
	SameTest -> (Simplify[#1 == #2]&)
]


VerificationTest[
	GPCBasis[{x, TransformedDistribution[-1+2x, x \[Distributed] NormalDistribution[3,4]]}, 4]
	,
	(* this result was constructed using default orthogonalization method *)
	{1,1/8 (-5+x),(-39-10 x+x^2)/(64 Sqrt[2]),(835-117 x-15 x^2+x^3)/(512 Sqrt[6]),(3313+3340 x-234 x^2-20 x^3+x^4)/(8192 Sqrt[6])}
	,
	SameTest -> (Simplify[#1 == #2]&)
]


VerificationTest[
	GPCBasis[{x, TransformedDistribution[12-39x, x \[Distributed] BetaDistribution[5,2]]}, 4]
	,
	(* this result was constructed using default orthogonalization method *)
	{1, (2*(111 + 7*x))/(39*Sqrt[5]), (Sqrt[7]*(615 + 112*x + 4*x^2))/507, (2*(36819 + 21303*x + 2115*x^2 + 55*x^3))/(19773*Sqrt[5]), (Sqrt[7/3]*(-48762 + 26352*x + 7524*x^2 + 528*x^3 + 11*x^4))/59319}
	,
	(* the basis is not unique *)
	SameTest -> (ArrayQ[FullSimplify[#1 / #2], (-1|1)]&)
]


VerificationTest[
	With[{basis = GPCBasis[{x, TransformedDistribution[12-39x, x \[Distributed] BetaDistribution[5,2]]}, 5]},
		PadRight[
			Table[
				Integrate[
					PDF[TransformedDistribution[12-39x, x \[Distributed] BetaDistribution[5,2]], x] basis[[m]] basis[[n]],
					{x, -27, 12}
				],
				{m,1,4}, {n,1,m}
			],
			{4,4}
		]
	]
	,
	IdentityMatrix[4]
]

