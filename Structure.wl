(* ::Package:: *)

(* :Title: Shape Metric Tools *)
(* :Context: Structure` *)
(* :Author: Flip Phillips *)
(* :Summary: *)
(* :Package Version: $Revision: 31 $ *)
(* :Mathematica Version: 10.0+ *)
(* :Copyright: Copyright 1999-2016, Flip Phillips, All Rights Reserved.  *)
(* :History:  *)
(* :Keywords: *)
(* :Limitations: *)
(* :Discussion: *)

BeginPackage["Shape`Structure`"]
  
VertexFrame::usage="VertexFrame[v,n,neighborhood] calculates the vertex frame for vertex v with normal n and neighbors."
OBJStructure::usage="OBJStructure[filename] calculates the differential structure of the OBJ file. This file should have vertex normals."
 
Begin["`Private`"]

(* returns p,n,pd1,pd2,k1,k2 *)

VertexFrame[v_,n_,hood_]:=Module[{nhood,xhood,m,a,b,c,ks,pd,pds,pd1,pd2},
	(* degenerate cases *)
	If[Chop[Norm[n],10^-8]==0||Chop[Norm[n-{0.,0.,-1.}],10^-8]==0,
		Return[{v,n,{0.,0.,0.},{0.,0.,0.},{0.,0.}}]];

	(* localize points *)
	nhood=#-v&/@hood;
	
	(* rotate so normal is z+ *)
	xhood=If[Chop[n]!={0,0,1.},
		RotationTransform[{n,{0.,0.,1.}}]/@nhood,
		nhood];

	(* find a ls fit *)
	m={#[[1]]^2.0/2.0,Times@@#,#[[2]]^2.0/2.0}&/@xhood[[All,1;;2]];

	(* Weingarten Matrix *)
	{a,b,c}=LeastSquares[m,xhood[[All,3]]];
	{ks,pd}=Eigensystem[{{a,b},{b,c}}];

	(* rotate them back *)
	pds=If[Chop[n]!={0,0,1.},
		RotationTransform[{{0.,0.,1.},n}]/@(Append[#,0.]&/@pd),
		pd];

	(* sort them from min,max *)
	{{pd1,pd2},ks}=If[LessEqual@@ks,{pds,ks},Reverse/@{pds,ks}];

	{v,n,pd1,pd2,ks}
]

mVertexVertexConnectivity[f_]:=Module[{connectivity,vs},
	connectivity = Import[f, "PolygonData"];
	vs = Union[Flatten[connectivity]];

	Function[index,
		Cases[DeleteDuplicates[Flatten[Select[connectivity, MemberQ[#, index] &]]],Except[index]]] /@ vs
]

mVertexVertexConnectivityRules[f_]:=Module[{connectivity,vs},
	connectivity = Import[f, "PolygonData"];
	vs = Union[Flatten[connectivity]];

	Function[index,index -> 
		Cases[DeleteDuplicates[Flatten[Select[connectivity, MemberQ[#, index] &]]],Except[index]]] /@ vs
]
	
mVertexCoordinateRules[f_]:=Module[{ps},
		ps = Import[f,"VertexData"];
		MapThread[Rule,{Range[Length[ps]],ps}]
]

neighborFest[i_,raf_,ri_]:=Select[DeleteDuplicates[Flatten[Nest[raf, i, ri]]], # != i &]

(*raf = ReplaceAll[Dispatch[mo["VertexVertexConnectivityRules"]]];*)
  


OBJStructure[f_] := 
	Module[{ns,vcr,vvcr,hoods},
		vcr = mVertexCoordinateRules[f];
		vvcr = mVertexVertexConnectivityRules[f];
		ns = Normalize/@Import[f,"VertexNormals"];

		(* onering right now *)
  		hoods =  vvcr[[All,2]] /. Dispatch[vcr];
  
		MapThread[VertexFrame, {vcr[[All,2]], ns, hoods}]
	]
  		
End[] (* End Private Context *)

EndPackage[]
