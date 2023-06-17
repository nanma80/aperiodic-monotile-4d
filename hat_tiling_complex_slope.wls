#!/usr/bin/env wolframscript

(* 
rho = exp*(2 pi i/6) (60 deg)
rho^3 = -1

a + b + c + d == 0
d = c * rho

b = -a - c * (1+rho)
-b = a + c * (1+rho)

a' = d rho^2 + a + b rho^4 = -c + a + a rho + c * (rho + rho^2) = a (1+rho) + c (rho + rho^2 - 1)
c' = d + c + b rho = c (rho + 1) - a rho - c rho* (1+rho) = - a rho + c (1 - rho^2)

*)

countMatrix = {{6, 5}, {1,1}};
Print[Simplify[Eigenvalues[countMatrix]]];
Print[Simplify[Eigenvalues[countMatrix]]//N];
Print[N[{GoldenRatio^4, GoldenRatio^(-4)}]];

rho = Exp[2 Pi I/6]; (* 60 *)

Q = {{ 1 + rho, rho + rho^2 - 1}, {- rho, 1 - rho^2}};
P = Q;
evals = Simplify[Eigenvalues[P]];
evecs = Simplify[Eigenvectors[P]];

Print[evals//N];
Print[N[{GoldenRatio^2, GoldenRatio^(-2)}]];

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

(* 
(Sqrt[3] - Sqrt[5] I)/4
0.433013 - 0.559017 I
 *)
targetSlope = (Sqrt[3] - Sqrt[5] I)/4;
Print[N[targetSlope]];

Print[FullSimplify[slope - targetSlope]];

