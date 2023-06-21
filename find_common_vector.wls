shape = "spectre_4d";
(* shape = "hat_4d"; *)
targetSlope = If[ shape == "spectre_4d", ( (5 Sqrt[3] - Sqrt[5]) + I * (3 - 3 * Sqrt[15]) ) /14, (Sqrt[3] - Sqrt[5] I)/4];

(* targetSlope = 1/targetSlope; *)
sr = Re[targetSlope];
si = Im[targetSlope];

asym = {{1, 0, -sr, si},{0, 1, -si, -sr}};
strip[t_] := Map[{-#[[4]], #[[1]], #[[2]], #[[3]]}&, {
		{Cos[t], 0, Sin[t], 0},
		{0, Cos[t], 0, Sin[t]}
	}];

solution = Solve[ Det[Join[asym, strip[t]]] == 0 && t>0 && t<Pi/2, t];
t/.solution

combinationSol = Solve[{x,y}.asym == {z,1}.(strip[t]/.solution[[1]]), {x, y, z} ]
comvec = {x,y}.asym/.combinationSol[[1]];
Print[comvec];
Print[{x,y}.asym/.combinationSol[[1]]//N];
Print[{z,1}.(strip[t]/.solution[[1]])/.combinationSol[[1]]//N];

