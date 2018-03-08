(* ::Package:: *)

(* :Title: Shape Metric Tools *)
(* :Context: Shape` *)
(* :Author: Flip Phillips *)
(* :Summary: 
	This package provides various curvature-related function to Mathematica.
*)
(* :Package Version: $Revision: 32 $ *)
(* :Mathematica Version: 10.0+ *)
(* :Copyright: Copyright 1999-2016, Flip Phillips, All Rights Reserved.  *)
(* :History: May14  added new contrast metric
			 Feb15 added visualization
			 Apr16 added conditional versions of shape index, derived K, H, etc  *)
(* :Keywords: *)
(* :Limitations: *)
(* :Discussion: *)

(* PC from H and K *)
ToPrincipalCurvatures[h_,k_]:=
	Module[{a=Clip[Sqrt[h^2-k],{0.0,Infinity}]},
	{h-a,h+a}]
	
ToPrincipalCurvatures[{h_,k_}]:=ToPrincipalCurvatures[h,k]

(* from 1st and 2nd derivatives *)
MeanCurvatureFromGradients[dx_,dy_,dxx_,dyy_,dxy_]:=(dxx(1+dy^2)+dyy(1+dx^2)-2 dx dy dxy)/(1+dx^2+dy^2)^(3/2)
GaussianCurvatureFromGradients[dx_,dy_,dxx_,dyy_,dxy_]:=(dxx dyy -dxy^2)/(1+dx^2+dy^2)^2

(* the traditional *)
GaussianCurvature[k1_,k2_]:=k1*k2
GaussianCurvature[{k1_,k2_}]:=GaussianCurvature[k1,k2]

MeanCurvature[k1_,k2_]:=(k1+k2)/2
MeanCurvature[{k1_,k2_}]:=MeanCurvature[k1,k2]

(* koenderink *)
ShapeIndex[k1_,k2_]:=0.0/;k1==k2
ShapeIndex[k1_,k2_]:=(2/Pi)ArcTan[k1-k2,k1+k2]/;k1>=k2
ShapeIndex[k1_,k2_]:=(2/Pi)ArcTan[k2-k1,k1+k2]/;k1<k2
ShapeIndex[{k1_,k2_}]:=ShapeIndex[k1,k2]

Curvedness[k1_,k2_]:=Sqrt[(k1^2+k2^2)/2]
Curvedness[{k1_,k2_}]:=Curvedness[k1,k2]

(* our variants *)
MichelsonContrast[k1_,k2_]:=0/;(k1+k2)==0
MichelsonContrast[k1_,k2_]:=(k1-k2)/(k1+k2)/;k1>=k2
MichelsonContrast[k1_,k2_]:=(k2-k1)/(k1+k2)/;k1<k2
MichelsonContrast[{k1_,k2_}]:=MichelsonContrast[k1,k2]

CurvatureContrast[k1_,k2_]:=Abs[Abs[k1]-Abs[k2]]
CurvatureContrast[{k1_,k2_}]:=CurvatureContrast[k1,k2]

NormalizedCurvatureContrast[k1_,k2_]:=0/;(Abs[k1]+Abs[k2])==0
NormalizedCurvatureContrast[k1_,k2_]:=Abs[(Abs[k1]-Abs[k2])/(Abs[k1]+Abs[k2])]/;k1>=k2
NormalizedCurvatureContrast[k1_,k2_]:=Abs[(Abs[k2]-Abs[k1])/(Abs[k1]+Abs[k2])]/;k1<k2
NormalizedCurvatureContrast[{k1_,k2_}]:=NormalizedCurvatureContrast[k1,k2]

TotalCurvature[k1_,k2_]:=Abs[k1]+Abs[k2]
TotalCurvature[{k1_,k2_}]:=TotalCurvature[k1,k2]

MaximumCurvature[k1_,k2_]:=Max[{Abs[k1],Abs[k2]}]
MaximumCurvature[{k1_,k2_}]:=MaximumCurvature[k1,k2]

MaximumSignedCurvature[k1_,k2_]:=If[Abs[k1]>Abs[k2],k1,k2]
MaximumSignedCurvature[{k1_,k2_}]:=MaximumSignedCurvature[k1,k2]


(* visualization / classification *)
koenderKolor={
		{-1.0,RGBColor[0.0,1.0,0.0]},
		{-0.75,RGBColor[0.0,1.0,0.5]},
		{-0.5,RGBColor[0.0,1.0,1.0]},
		{-0.25,RGBColor[0.5,1.0,1.0]},
		{0.0,RGBColor[1.0,1.0,1.0]},
		{0.25,RGBColor[1.0,1.0,0.5]},
		{0.5,RGBColor[1.0,1.0,0.0]},
		{0.75,RGBColor[1.0,0.5,0.0]},
		{1.0,RGBColor[1.0,0.0,0.0]}
};

ShapeIndexColor[si_]:=Blend[koenderKolor,si]
ShapeIndexCategory[si_] := 
		Which[
			si < -1, 0,
			si < -7/8, 1,
			si < -5/8, 2,
			si < -3/8, 3,
			si < -1/8, 4,
			si <  1/8, 5,
			si <  3/8, 6,
			si <  5/8, 7,
			si <  7/8, 8,
			si <  1,   9,
			True, 0
		]
