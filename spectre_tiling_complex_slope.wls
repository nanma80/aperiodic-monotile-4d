#!/usr/bin/env wolframscript

countMatrix = {{7,6}, {1,1}};

rho = Exp[2 Pi I/6]; (* 60 *)
(* a + b + c + d == 0
d = c * rho

b = -a - c * (1+rho)
-b = a + c * (1+rho)

-ap = d rho^2 + a + b rho^4
-cp = b + c rho^5 + d rho^5

ap = a (rho^4-1) + c * (rho^4+rho^5 - rho^3)
cp = a + c (rho - rho^5) *)

Q = {{(rho^4-1), (rho^4+rho^5 - rho^3)}, {1, (rho - rho^5)}};
P = Conjugate[Q].Q;
Print[Q//FullSimplify];
