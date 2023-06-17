#!/usr/bin/env wolframscript

(* a + b + c + d == 0
d = c * rho

b = -a - c * (1+rho)
-b = a + c * (1+rho)

-ap = d rho^2 + a + b rho^4
-cp = b + c rho^5 + d rho^5

ap = a (rho^4-1) + c * (rho^4+rho^5 - rho^3)
cp = a + c (rho - rho^5) *)

countMatrix = {{7,6}, {1,1}};

rho = Exp[2 Pi I/6]; (* 60 *)

Q = {{(rho^4-1), (rho^4+rho^5 - rho^3)}, {1, (rho - rho^5)}};
P = Conjugate[Q].Q;
evals = Eigenvalues[P];
evecs = Eigenvectors[P];

directions = {};
For[clock = 1, clock <= 12, clock++,
	vector2d = 2 {Sin[2 Pi / 12 * clock], Cos[2 Pi / 12 * clock]};
	vector4d = If[Mod[clock, 2] == 0, 
		Join[vector2d, {0, 0}],
		Join[{0, 0}, vector2d]
	];
	AppendTo[directions, vector4d];
];
directionMap = <||>;
For[did = 1, did <= Length[directions], did++,
	directionMap[directions[[did]]] = did;
];

directionsInPolygon = {6, 4, 7, 5, 2, 4, 1, 3, 12, 10, 10, 8, 11, 9, 6};

polygon4d = {{0, 0, 0, 0}};
For[vid = 2, vid <= 14, vid++,
	lastPoint = Last[polygon4d];
	newPoint = lastPoint + directions[[ directionsInPolygon[[vid]] ]];
	AppendTo[polygon4d, newPoint];
];

(* Print[polygon4d]; *)

a04d = polygon4d[[9]] - polygon4d[[3]];
c04d = polygon4d[[1]] - polygon4d[[13]];

a0 = {a04d[[1]] + I a04d[[2]], a04d[[3]] + I a04d[[4]]};
c0 = {c04d[[1]] + I c04d[[2]], c04d[[3]] + I c04d[[4]]};

complex10 = {a0[[1]], c0[[1]]};
complex20 = {a0[[2]], c0[[2]]};

solution1 = vplus/.Solve[ evecs[[1]] * vplus + evecs[[2]] * vminus == complex10, {vplus, vminus}][[1]];
solution2 = vplus/.Solve[ evecs[[1]] * vplus + evecs[[2]] * vminus == complex20, {vplus, vminus}][[1]];

slope = solution2/solution1//Simplify;
Print[slope];
Print[N[slope]];

targetSlope = ( (5 Sqrt[3] - Sqrt[5]) + I * (3 - 3 * Sqrt[15]) ) /14;
Print[N[targetSlope]];

Print[FullSimplify[slope - targetSlope]];

