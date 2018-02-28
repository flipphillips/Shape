(* ::Package:: *)

(* shape and curvedness monge form stuff *)
(* Flip Phillips *)
(* May 2014 (re-written from code I wrote in 1995 or so) *)
(* v. 1 *)


BeginPackage["Shape`ShapePatches`"];


Patches::usage="Patches is a set of routines to create parametrically defined patches from curvature information";


curvaturesPatch::usage = "curvaturesPatch[u,v,k1,k2] is a parametric Monge-form patch from curvatures.";
shapeCurvednessPatch::usage = "shapeCurvednessPatch[u,v,s,c] creates a parametric Monge-form patch from Shape Index and Curvedness.";
meanGaussPatch::usage = "meanGaussPatch[u,v,h,k] creates a parametric Monge-form patch from mean and Gaussian curvatures.";


Begin["`Private`"];


(* these are internal, but availble in the metrics package now, so... *)


kFromSC[s_,c_]:={k1->((-(1/2)+I/2) c Cos[(\[Pi] s)/2]-(1/2-I/2) c Cos[(3 \[Pi] s)/2]+(1/2+I/2) c Sin[(\[Pi] s)/2]-(1/2+I/2) c Sin[(3 \[Pi] s)/2])/(-I+Cos[\[Pi] s]+I Sin[\[Pi] s]),k2->(1/2+I/2) c (Cos[(\[Pi] s)/2]-I Sin[(\[Pi] s)/2]) (-I+Cos[\[Pi] s]+I Sin[\[Pi] s])}


kFromHK[h_,k_]:={k1->h-Sqrt[h^2-k],k2->h+Sqrt[h^2-k]}


Unprotect[{curvaturesPatch,shapeCurvednessPatch,meanGaussPatch}];


curvaturesPatch[u_,v_,k1_,k2_]:={u,v,k1 u^2+k2 v^2}


shapeCurvednessPatch[u_,v_,s_,c_]:=Chop[{u,v,k1 u^2+k2 v^2}/.kFromSC[s,c]]


meanGaussPatch[u_,v_,h_,k_]:=Chop[{u,v,k1 u^2+k2 v^2}/.kFromHK[h,k]]


End[];


Protect[{curvaturesPatch,shapeCurvednessPatch,meanGaussPatch}];


EndPackage[];
