PrintTemporary["Testing DistributionSupport symbol..."];


VerificationTest[
	Needs["PolynomialChaos`"],
	Null
]


VerificationTest[
	DistributionSupport[UniformDistribution[{-12.3, 17.98}]]
	,
	{-12.3, 17.98}
]


VerificationTest[
	DistributionSupport[BetaDistribution[52, 76]]
	,
	{0, 1}
]


VerificationTest[
	DistributionSupport[NormalDistribution[5.2, 7.6]]
	,
	{-\[Infinity], \[Infinity]}
]


VerificationTest[
	DistributionSupport[LogNormalDistribution[5.2, 7.6]]
	,
	{0, \[Infinity]}
	,
	SameTest -> Equal
]


VerificationTest[
	DistributionSupport[ChiSquareDistribution[3]]
	,
	{0, \[Infinity]}
	,
	SameTest -> Equal
]


VerificationTest[
	DistributionSupport[TransformedDistribution[-10 + 2 x, x \[Distributed] BetaDistribution[3, 9]]]
	,
	{-10, -8}
	,
	SameTest -> Equal
]
