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

BeginPackage["Structure`"]
(* Exported symbols added here with SymbolName::usage *)  
VertexFrame::usage="VertexFrame[v,n,neighborhood] calculates the vertex frame for vertex v with normal n and neighbors."

Begin["`Private`"] (* Begin Private Context *) 

VertexFrame[v_,n_,hood_]:=Module[{nhood,xhood,m,a,b,c,ks,pd,pds},
	(* degenerate cases *)
	If[Chop[Norm[n],10^-8]==0||Chop[Norm[n-{0.,0.,-1.}],10^-8]==0,
		Return[{{0.,0.},{{0.,0.,0.},{0.,0.,0.}}}]];

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

	pds=If[Chop[n]!={0,0,1.},
		RotationTransform[{{0.,0.,1.},n}]/@(Append[#,0.]&/@pd),
		pd];

	If[LessEqual@@ks,
		{ks,pds},Reverse/@{ks,pds}]]


MeshRegionFrames[o_, ns_] := 
 Module[{mo, ps, vcr, vvc, vvcr, raf, hoods, neighbors},
  mo = Graphics`Region`ToMeshObject[o];
  
  ps = mo["VertexCoordinates"];
  vcr = Dispatch[mo["VertexCoordinateRules"]];
  vvc = mo["VertexVertexConnectivity"];
  vvcr = Dispatch[mo["VertexVertexConnectivityRules"]];
  
  raf = ReplaceAll[vvcr];
  neighbors[i_, ri_] := 
   Select[Union[Flatten[Nest[raf, i, ri]]], # != i &];
  
  hoods = vvc /. vcr;
  
  MapThread[VertexFrame, {ps, ns, hoods}]]
  		
End[] (* End Private Context *)

EndPackage[]