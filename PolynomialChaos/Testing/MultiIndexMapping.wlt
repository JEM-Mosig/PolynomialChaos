PrintTemporary["Testing MultiIndexMapping symbol..."];


VerificationTest[
	Needs["PolynomialChaos`"],
	Null,
	TestID -> "Load:MultiIndex"
]


Begin["PolynomialChaos`MultiIndex`"]


VerificationTest[
	MultiIndexMapping[3, 2],
	{1->{0,0,0},2->{1,0,0},3->{0,1,0},4->{0,0,1},5->{2,0,0},6->{1,1,0},7->{1,0,1},8->{0,2,0},9->{0,1,1},10->{0,0,2}}
];


VerificationTest[
	MultiIndexMapping[3, 2, Ordering -> Automatic],
	{1->{0,0,0},2->{0,0,1},3->{0,0,2},4->{0,1,0},5->{0,1,1},6->{0,2,0},7->{1,0,0},8->{1,0,1},9->{1,1,0},10->{2,0,0}}
];


VerificationTest[
	Length@MultiIndexMapping[3, 2, Method -> "EachMax"],
	27
];


VerificationTest[
	MultiIndexMapping[3, 2, Ordering -> "GradedReverseLexicographic"],
	{1->{0,0,0},2->{1,0,0},3->{0,1,0},4->{0,0,1},5->{2,0,0},6->{1,1,0},7->{0,2,0},8->{1,0,1},9->{0,1,1},10->{0,0,2}}
];


End[] (* PolynomialChaos`MultiIndex` *)