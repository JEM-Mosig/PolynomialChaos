PrintTemporary["Testing GPCBasis symbol...\nThis may take a few minutes."];


VerificationTest[
  Needs["PolynomialChaos`"],
  Null
]


VerificationTest[
  With[{basis = GPCBasis[{x, NormalDistribution[-12, 6]}, 5]},
    PadRight[
      Table[
        Integrate[
          PDF[NormalDistribution[-12, 6], x] basis[[m]] basis[[n]],
          {x, -\[Infinity], \[Infinity]}
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
  With[{basis = GPCBasis[{x, BetaDistribution[5, 2]}, {y, NormalDistribution[-2, 8]}, 3]},
    PadRight[
      Table[
        Integrate[
          PDF[BetaDistribution[5, 2], x] PDF[NormalDistribution[-2, 8], y] basis[[m]] basis[[n]],
          {x, 0, 1}, {y, -\[Infinity], \[Infinity]}
        ],
        {m, 1, 10}, {n, 1, m}
      ],
      {10, 10}
    ]
  ]
  ,
  IdentityMatrix[10]
]
