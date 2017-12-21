PrintTemporary["Testing MultiIndexList symbol..."];


VerificationTest[
	Needs["PolynomialChaos`"],
	Null
]


Begin["PolynomialChaos`MultiIndex`"]


VerificationTest[
	MultiIndexList[3, 2],
	{{0,0,0},{1,0,0},{0,1,0},{0,0,1},{2,0,0},{1,1,0},{1,0,1},{0,2,0},{0,1,1},{0,0,2}}
];


(* Example from Xiu (2010), p. 66 *)

VerificationTest[
	Take[MultiIndexList[4, 3], 18],
	{
		{0,0,0,0}, 
		{1,0,0,0},
		{0,1,0,0},
		{0,0,1,0}, 
		{0,0,0,1},
		{2,0,0,0},
		{1,1,0,0},
		{1,0,1,0},
		{1,0,0,1},
		{0,2,0,0},
		{0,1,1,0},
		{0,1,0,1},
		{0,0,2,0},
		{0,0,1,1},
		{0,0,0,2},
		{3,0,0,0},
		{2,1,0,0},
		{2,0,1,0}
	}
];


VerificationTest[
	MultiIndexList[3, 2, Ordering -> Automatic],
	{{0,0,0},{0,0,1},{0,0,2},{0,1,0},{0,1,1},{0,2,0},{1,0,0},{1,0,1},{1,1,0},{2,0,0}}
];


VerificationTest[
	Length@MultiIndexList[3, 2, Method -> "EachMax"],
	27
];


VerificationTest[
	MultiIndexList[3, 2, Ordering -> "GradedReverseLexicographic"],
	{{0,0,0},{1,0,0},{0,1,0},{0,0,1},{2,0,0},{1,1,0},{0,2,0},{1,0,1},{0,1,1},{0,0,2}}
];


End[] (* PolynomialChaos`MultiIndex` *)