(* ::Package:: *)

(* shape and curvedness monge form stuff *)
(* Flip Phillips *)
(* May 2014 (re-written from code I wrote in 1995 or so) *)
(* June 2022 - wtf am I doing? *)
(* v. 1 *)


CurvaturesPatch[k1_, k2_][u_, v_] := Chop[k1 u^2 + k2 v^2]


ShapeCurvednessPatch[s_, c_][u_, v_] :=
	Chop[#1 u^2 + #2 v^2]& @@ PrincipalCurvaturesFromSC[s, c]


MeanGaussianPatch[h_, k_][u_, v_] :=
	Chop[#1 u^2 + #2 v^2]& @@ PrincipalCurvaturesFromHK[h, k]
