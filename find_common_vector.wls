#!/usr/bin/env wolframscript

shape = "spectre_4d";
(* shape = "hat_4d"; *)
targetSlope = If[ shape == "spectre_4d", ( (5 Sqrt[3] - Sqrt[5]) + I * (3 - 3 * Sqrt[15]) ) /14, (Sqrt[3] - Sqrt[5] I)/4];

finalScale = If[ shape == "spectre_4d", 6, 2 Sqrt[5]];

(* targetSlope = 1/targetSlope; *)
sr = Re[targetSlope];
si = Im[targetSlope];

(* my convention, need to invert *)
(* asym1 = {{1, 0, -sr, si},
		{0, 1, -si, -sr}};
 *)
(* Arnaud's convention for the subspace *)
asymSubspace = {
	{-sr, si, 1, 0}, 
	{-si, -sr, 0, 1}
};

(* projection ot the asym subspace *)
asym = {
	{1, 0, sr, si},
	{0, 1, -si, sr}
};

strip[t_] := Map[{-#[[4]], #[[1]], #[[2]], #[[3]]}&, {
		{Cos[t], 0, Sin[t], 0},
		{0, Cos[t], 0, Sin[t]}
	}];

solution = Solve[ Det[Join[asym, strip[t]]] == 0 && t>0 && t<Pi/2, t];
Print[t/.solution];
Print[N[t/.solution]];

For[sid = 1, sid <= Length[solution], sid++,
	combinationSol = Solve[{x,y}.asym == {z,1}.Simplify[(strip[t]/.solution[[sid]])], {x, y, z} ];
	comvec = {x,y}.asym*finalScale/.combinationSol[[1]];
	Print[];
	Print[comvec//FullSimplify];
	Print[{x,y}.asym*finalScale/.combinationSol[[1]]//N];
	Print[{z,1}.(strip[t]*finalScale/.solution[[sid]])/.combinationSol[[1]]//N];
];

