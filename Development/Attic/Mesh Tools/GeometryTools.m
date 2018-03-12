(* ::Package:: *)

(* ::Section:: *)
(*3D Geometry Tools*)


(* :Title: 3D Geometry Tools *)

(* :Context: ExpTools`GeometryTools` *)

(* :Author: Flip Phillips *)

(* :Summary: 
	This package can calculate connectivity and normals given just raw 
	points-and-polygons data - mainly triangles. 
*)

(* :Package Version: $Revision: 31 $ *)

(* :Mathematica Version: 10.0 *)

(* :Copyright: Copyright 1999-2014, Flip Phillips, All Rights Reserved.  *)

(* :History: *)

(* :Keywords:
	computetional geometry
*)

(* :Limitations:  *)

(* :Discussion:  ... *)

(* :ToDo: Go through and make 'optimized' local calls that can take advantage of redundant computations- for example, when you compute normals, you 
		compute a bunch of things multiple times... we should minimize that redundancy *)


(* ::Subsection:: *)
(*Frontmatter*)


BeginPackage["ExpTools`GeometryTools`"];


GeometryTools::usage="Provides functions to compute connectivity and approximate normals to raw points-and-polys data";


(* ::Subsubsection:: *)
(*Utils*)


FacesToTriangles::usage="FacesToTriangles[faces] converts face indices into vertex lists.";


TriangleToEdges::usage="TriangleToEdges[t] converts a triangle into an edge-vertex-list.";


ObtuseQ::usage="ObtuseQ[ts] Is any angle in ts obtuse?";


TrianglesQ::usage="TrianglesQ[f] is the face list triangles only.";


(* ::Subsubsection:: *)
(*Faces*)


FaceNormals::usage="FaceNormals[verts,faces] computes the face normals via crossproduct.";


FaceAngles::usage="FaceAngles[verts,faces] computes the face angles.";


FaceAreas::usage="FaceAreas[verts,faces] computes the face areas.";


FaceBarycenters::usage="FaceBarycenters[verts,faces] computes the face centroids.";


FaceCircumcenters::usage="FaceBarycenters[verts,faces] computes the face circumcenters.";


(* ::Subsubsection:: *)
(*Vertices*)


VertexFaces::usage:="VertexFaces[faces] returns a list of faces connected to each vertex.";


VertexEdges::usage:="VertexEdges[faces] returns the directed edge list for each vertex."


VertexNeighbors::usage="VertexNeighbors[faces] directed points for each vertex, e.g. the 'second' point in the edge";


VertexFaceNormals::usage="VertexFaceNormals[verts,faces] gives the face normals for all faces connected to each vertex.";


VertexFaceAreas::usage="VertexFaceAreas[verts,faces] face areas for all faces connected to each vertex.";


VertexFaceAngles::usage="VertexFaceAngles[verts,faces] gives the angles of the faces connected to each vertex.";


VertexVertexAngles::usage="VertexVertexAngles[verts,faces] gives the angle at the target vertex for connected faces.";


VertexEdgeAngles::usage="VertexEdgeAngles[verts,faces] gives a list of angles opposite each vertex edge";


VertexEdgeLengths::usage="VertexEdgeLengths[verts,faces] gives a list of edge lengths."


VertexEdgeSquaredLengths::usage="VertexEdgeSquaredLengths[verts,faces] gives a list of squared edge lengths.";


VertexEdgeDifferences::usage="VertexEdgeDifferences[verts,faces] gives a list of edge differences.";


CenterVertices::usage="CenterVertices[vertexList] centers the object about its center of gravity";


ReduceVertices::usage="ReduceVertices[verts,faces] compactifies the face-list to be on the range [1,nverts].";


(* ::Subsubsection:: *)
(*Edges*)


Edges::usage="Edges[faces] returns an undirected edge vertex index list from faces.";


EdgeLengths::usage="EdgeLengths[verts,faces] returns the length of edges faces.";


EdgeAngleRules::usage="EdgeAngleRules[verts,faces] returns a dispatch table that has the edge-adjacent angles indexable by edge.";


EdgeLengthRules::usage="EdgeLengthRules[verts,faces] returns a dispatch table from edge to edge lengths.";


EdgeSquaredLengthRules::usage="EdgeSquaredLengthRules[verts,faces] returns a dispatch table from edge to edge lengths.";


EdgeDifferenceRules::usage="EdgeDifferenceRules[verts,faces] returns a dispatch table from edge to vertex differences.";


(* ::Subsubsection:: *)
(*Differential Geometry*)


ApproximateVertexNormals::usage="ApproximateVertexNormals[verts,faces] approximates vertex normals using the method of XXXX";


(* ::Subsubsection:: *)
(*WRI GraphicsComplex[] utils*)


Normals::usage="Normals[obj] returns the vertex normals of an GraphicsComplex[] object";


Vertices::usage="Vertices[obj] returns the vertex list from a GraphicsComplex[] object";


Faces::usage="Faces[obj] returns the face list from as GraphicsComplex[] object";


(* ::Subsubsection:: *)
(*Deprecated*)


ComplexConnectivity::usage="ComplexConnectivity[verts,faces] computes 1-ring connectivity
	between vertices and faces. Returns {edges, vertexfaces}";


ComplexNormals::usage="ComplexNormals[verts,faces,vfaces] approximates
	face and vertex normals. Returns {fnormals, vnormals)";


SmoothVertexNormals::usage="SmoothVertexNormals[vnormals,faces,depth]";


ComplexGeometry::usage="ComplexGeometry[verts,faces] does both 
	ComplexGeometry and Normals. Returns {verts,faces,edges,vfaces,fnormals,vnormals}.";


(* ::Subsection:: *)
(*Private Stuff*)


Begin["`Private`"];


(* ::Subsubsection:: *)
(*Vector / Face stuff*)


(* ::Text:: *)
(*Go from a point + two vertices to a pair of vectors so we can do things with it.*)


vecx[{p_,v1_,v2_}]:={(v1-p),(v2-p)};


(* ::Text:: *)
(*The cross product... always the cross product.*)


FaceNormal[t_]:=
		Normalize[Cross@@vecx[t]];


(* ::Text:: *)
(*Compute the face angles for a given triangle... (rotateleft = CCW / right hand rule ordering)*)


FaceAngle[t_]:=Module[{eds,vecs},
	eds=NestList[RotateLeft,t,2];
	vecs=Map[vecx,eds];
	VectorAngle@@@vecs]


(* ::Subsubsection:: *)
(*Centers*)


(* ::Text:: *)
(*The centroid/barycenter of a triangle*)


barycenter[t_]:=Mean[t]


(* ::Text:: *)
(*The circumcenter - that I thought was wrong, but I know now it is right.*)


circumcenter[{aa_,bb_,cc_}]:=Module[{a,b,n},
	a=aa-cc;
	b=bb-cc;
	n=a\[Cross]b;

	(Norm[a]^2 b-Norm[b]^2 a)\[Cross]n/(2Norm[n]^2)+cc]


(* ::Text:: *)
(*The circumradius*)


circumradius[{aa_,bb_,cc_}]:=Module[{a,b,n},
	a=aa-cc;
	b=bb-cc;
	n=a\[Cross]b;

	(Norm[a]Norm[b]Norm[a-b])/(2Norm[n])]


(* ::Subsubsection:: *)
(*Some Normal calculators*)


meanNormals[verts_,faces_]:=Module[{vf,fn,angleAreaNormal},
	vf=VertexFaces[faces];
	fn=FaceNormals[verts,faces];

	Normalize[Mean[fn[[#]]]]&/@vf
]


areaWeightedNormals[verts_,faces_]:=Module[{vf,fn,fa,angleAreaNormal},
	vf=VertexFaces[faces];
	fn=FaceNormals[verts,faces];
	fa=FaceAreas[verts,faces];

	Normalize[Mean[fa[[#]] fn[[#]]]]&/@vf
]


areaAngleWeightedNormalsX[verts_,faces_]:=Module[{vf,fn,fa,fx,angleAreaNormal},
	vf=VertexFaces[faces];
	fn=FaceNormals[verts,faces];
	fx=FaceAngles[verts,faces];
	fa=FaceAreas[verts,faces];

	angleAreaNormal[v_]:=Module[{cf,angles,areas,normals},
		cf=vf[[v]];
		angles=Flatten[MapThread[Extract,{fx[[cf]],Position[#,v]&/@faces[[cf]]}]];
		areas=fa[[cf]];
		normals=fn[[cf]];

		Normalize[Mean[angles areas normals]]];

	angleAreaNormal/@Range[Max[faces]]]



areaAngleWeightedNormals[verts_,faces_]:=Module[{vf,va,fn,fa},
	vf=VertexFaces[faces];
	va=VertexVertexAngles[verts,faces];
	fn=FaceNormals[verts,faces];
	fa=FaceAreas[verts,faces];
	
	Normalize/@MapThread[Mean[#2 fa[[#1]] fn[[#1]]]&,{vf,va}]]


centroidWeightedNormals[verts_,faces_]:=Module[{vf,fn,fc,cw,centroidNormal},
	vf=VertexFaces[faces];
	fn=FaceNormals[verts,faces];
	fc=FaceBarycenters[verts,faces];

	(* mean distance from the centroids *)
	centroidNormal[v_]:=Module[{cf,w},
		cf=vf[[v]];
		w=1/EuclideanDistance[verts[[v]],#]&/@fc[[cf]];
		Normalize[Mean[fn[[cf]]*w]]];

	centroidNormal/@Range[Max[faces]]]



centroidAngleWeightedNormals[verts_,faces_]:=Module[{vf,fn,fc,fx,cw,centroidNormal},
	vf=VertexFaces[faces];
	fn=FaceNormals[verts,faces];
	fc=FaceBarycenters[verts,faces];
	fx=FaceAngles[verts,faces];

	(* mean distance from the centroids *)
	centroidNormal[v_]:=Module[{cf,angles,dists,w},
		cf=vf[[v]];
		angles=Flatten[MapThread[Extract,{fx[[cf]],Position[#,v]&/@faces[[cf]]}]];

		dists=EuclideanDistance[verts[[v]],#]&/@fc[[cf]];
		w=angles/dists;
		Normalize[Mean[fn[[cf]]*w]]];

	centroidNormal/@Range[Max[faces]]]


(* ::Subsubsection:: *)
(*DG Functions*)


(* ::Text:: *)
(*compute the weights for a vertex as per *)
(**)
(*Meyer, M., Desbrun, M., Schr\[ODoubleDot]der, P., & Barr, A. H. (2002). Discrete differential-geometry operators for triangulated 2-manifolds. Visualization and Mathematics, 3(2), 52\[Dash]58.*)


(* ::Text:: *)
(*Helpers - edge to measure (cotangent \[Alpha]\[Beta], length^2, difference between points*)


cotRules[faces_,f\[Theta]_]:=
	Dispatch[Flatten[Map[Map[Apply[Rule,#]&,{TriangleToEdges[First[#]],RotateRight[#[[2]]]}\[Transpose]]&,{faces,Cot[f\[Theta]]}\[Transpose]]]]


lengthRules[verts_,faces_]:=Module[{te},
	Dispatch[Flatten[Map[Apply[Rule,#]&,Transpose[{te=TriangleToEdges[#],Map[Apply[EuclideanDistance,#]^2&,verts[[#]]&/@te,{1}]}]]&/@faces]]]


dprules[verts_,faces_]:=Module[{te},
	Dispatch[Flatten[Map[Apply[Rule,#]&,Transpose[{te=TriangleToEdges[#],Map[Apply[Subtract,Reverse[#]]&,verts[[#]]&/@te,{1}]}]]&/@faces]]]


(* ::Text:: *)
(*Area-based weights as per above*)


meyerWeights[\[Theta]s_,angles_,voroni_,areas_]:=
	Map[
		If[!ObtuseQ[#[[1]]],#[[3]],
		If[!ObtuseQ[{#[[2]]}],#[[4]]/2,#[[4]]/4]]&,{\[Theta]s,angles,voroni,areas}\[Transpose]]


(* ::Text:: *)
(*The beef - complicated to be sure... but perhaps more betterish?*)


meyerNormals[verts_,faces_]:=Module[{vf\[Theta],vfa,vfn,vva,ve,f\[Theta],cots,lens,voroniAreas,faceNWeights},
	vf\[Theta]=VertexFaceAngles[verts,faces];
	vfa=VertexFaceAreas[verts,faces];
	vva=VertexVertexAngles[verts,faces];
	ve=VertexEdges[faces];
	f\[Theta]=FaceAngles[verts,faces];

	(* we need the cotangents and lengths *)
	cots=ve/.cotRules[faces,f\[Theta]];
	lens=ve/.lengthRules[verts,faces];

	(* compute Voroni regions *)
	voroniAreas=Map[Apply[Plus,#]&,cots*lens,{2}]/8;

	faceNWeights=MapThread[meyerWeights,{vf\[Theta],vva,voroniAreas,vfa}];

	vfn=VertexFaceNormals[verts,faces];
	Normalize/@Map[Mean,faceNWeights*vfn,{1}]]


(* ::Subsection:: *)
(*Main stuff*)


Unprotect[{
	FacesToTriangles,TriangleToEdges,ObtuseQ,TrianglesQ,

	FaceNormals,FaceAngles,FaceAreas,FaceBarycenters,FaceCircumcenters,

	VertexFaces,VertexEdges,VertexNeighbors,
	VertexFaceNormals,VertexFaceAreas,VertexFaceAngles,VertexVertexAngles,
	VertexEdgeAngles,VertexEdgeLengths,VertexEdgeDifferences,VertexEdgeSquaredLengths,
	CenterVertices,ReduceVertices,

	Edges,EdgeAngleRules,EdgeLengthRules,EdgeDifferenceRules,EdgeSquaredLengthRules,

	Vertices,Faces,Normals,

	ApproximateVertexNormals
}];


(* ::Subsubsection:: *)
(*Utilities*)


(* ::Text:: *)
(*As we move to v 10 I think I might set these up as regions instead of just raw co\[ODoubleDot]rdinates*)


FacesToTriangles[verts_,faces_]:=verts[[#]]&/@faces


TriangleToEdges[t_]:=Partition[t,2,1,1]


ObtuseQ[t_]:=AnyTrue[t,(#>=\[Pi]/2)&]


(* ::Subsubsection:: *)
(*Face stuff*)


(* ::Text:: *)
(*Simple face normal calculator.*)


FaceNormals[verts_,faces_]:=FaceNormal /@ FacesToTriangles[verts,faces]


(* ::Text:: *)
(*Angles*)


FaceAngles[verts_,faces_]:=FaceAngle /@ FacesToTriangles[verts,faces]


(* ::Text:: *)
(*Area -- this version is 10+*)


FaceAreas[verts_,faces_]:=Area[Triangle[verts[[#]]]]&/@faces


(* ::Text:: *)
(*Centers*)


FaceBarycenters[verts_,faces_]:=
	barycenter/@ FacesToTriangles[verts,faces]


(* ::Text:: *)
(*Voroni / circumcenters*)


FaceCircumcenters[verts_,faces_]:=
	circumcenter/@ FacesToTriangles[verts,faces]


(* ::Subsubsection:: *)
(*Vertex Stuff*)


(* ::Text:: *)
(*For ever vertex, a list of faces connected to it. Theyre not ordered in any significant way in this version. Maybe I should *)


VertexFaces[faces_]:=Module[{vfaces},
	vfaces=Table[{},{Max[faces]}];
	Do[Map[AppendTo[vfaces[[#]],f]&,faces[[f]]],{f,1,Length[faces]}];
	Union/@vfaces]


(* ::Text:: *)
(*Vertex neighbors- directed (e.g. for each vertex, the neigubor is the 'second' point in order). There should probably be a 'depth' parameter to do this recursively, for more nuanced / weird calculations that I do,*)


VertexNeighbors[faces_]:=Module[{vf,AVertexNeighbors},
	vf=VertexFaces[faces];

	AVertexNeighbors[ff_,vf_,v_]:=Module[{facelist,positions},
		facelist=ff[[vf[[v]]]];
		positions=Mod[FirstPosition[#,v],3]+1&/@facelist;
		Flatten[MapThread[Part,{facelist,positions}]]];

	AVertexNeighbors[faces,vf,#]&/@Range[Max[faces]]
]


(* ::Text:: *)
(*Syntactic Sugar - ordered though!*)


VertexEdges[faces_]:=MapThread[Function[{v,l},Map[{v,#}&,l]],{Range[Max[faces]],VertexNeighbors[faces]}]


(* ::Text:: *)
(*Associated w/ faces connected to vertices, angles, areas*)


VertexFaceNormals[verts_,faces_]:=Module[{vf,fn},
	vf=VertexFaces[faces];
	fn=FaceNormals[verts,faces];

	fn[[#]]&/@vf]


VertexFaceAngles[verts_,faces_]:=Module[{vf,f\[Theta]},
	vf=VertexFaces[faces];
	f\[Theta]=FaceAngles[verts,faces];

	f\[Theta][[#]]&/@vf]


VertexFaceAreas[verts_,faces_]:=Module[{vf,fa},
	vf=VertexFaces[faces];
	fa=FaceAreas[verts,faces];

	fa[[#]]&/@vf]


VertexVertexAngles[verts_,faces_]:=Module[{vf, vf\[Theta], vpos},
	vf=VertexFaces[faces];
	vf\[Theta]=VertexFaceAngles[verts,faces];

	(* the vertex list position on each face connected to that vertex *)
	vpos=Function[v,Position[#,v]&/@faces[[vf[[v]]]]]/@Range[Max[faces]];
	(* extract the angles at each face there *)
	Flatten/@MapThread[Extract,{vf\[Theta],vpos},2]]


(* ::Text:: *)
(*For each edge, the angle of the two adjacent corners*)


VertexEdgeAlphaBetas[verts_,faces_]:=Module[{ve,ear},
	ve=VertexEdges[faces];
	ear=EdgeAngleRules[verts,faces];

	(({#,Reverse[#]}/.ear)&/@ #)&/@ve ]


VertexEdgeCotSum[verts_,faces_]:=Module[{veab},
	veab=VertexEdgeAlphaBetas[verts,faces];
	






(* ::Text:: *)
(*Make a nice little adjecent edge database.*)


VertexEdgeAngles[verts_,faces_]:=Module[{ve,vf,vf\[Theta],AVertexEdgeAngle},
	ve=VertexEdges[faces];
	vf=VertexFaces[faces];
	vf\[Theta]=VertexFaceAngles[verts,faces];

	AVertexEdgeAngle[edges_,tris_,angles_]:=Module[{edgetris,missing,where},
		edgetris=faces[[tris]];
		missing=Flatten[MapThread[Complement,{edgetris,edges}]];
		where=MapThread[FirstPosition,{edgetris,missing}];
		Flatten[MapThread[Part,{angles,where}]]];

	MapThread[AVertexEdgeAngle,{ve,vf,vf\[Theta]}]]


VertexEdgeLengths[verts_,faces_]:=Map[Apply[EuclideanDistance,#]&,Map[verts[[#]]&,VertexEdges[faces],{2}],{2}]


VertexEdgeSquaredLengths[verts_,faces_]:=Map[Apply[SquaredEuclideanDistance,#]&,Map[verts[[#]]&,VertexEdges[faces],{2}],{2}]


(* ::Text:: *)
(*There was a revers in the Subtract. I think that was wrong.*)


VertexEdgeDifferences[verts_,faces_]:=Map[Apply[Subtract,#]&,Map[verts[[#]]&,VertexEdges[faces],{2}],{2}]


(* ::Text:: *)
(*Vertex areas = Voroni / mixed area belonging to each vertex.*)


VertexVertexAreas[verts_,faces_]:=Module[{},
	fc = FaceCircumcenters[verts,faces];
	
	vf\[Theta]=VertexFaceAngles[verts,faces];
	vfa=VertexFaceAreas[verts,faces];
	vva=VertexVertexAngles[verts,faces];
	ve=VertexEdges[faces];
	f\[Theta]=FaceAngles[verts,faces];







(* ::Text:: *)
(*Center vertices about the centroid*)


CenterVertices[verts_]:=Module[{centroid},
	centroid=Mean/@Transpose[verts];
	#-centroid&/@verts]


(* ::Text:: *)
(*Right now, this only cleans up the extra vertices + face renumbering that happens as a result of the Geodesate routine-*)


ReduceVertices[verts_,faces_]:=Module[{actualIndexes,newVs,conversionRules,newFs},
	actualIndexes=faces//Flatten//Union;
	newVs=verts[[actualIndexes]];
	conversionRules=Dispatch[MapThread[Rule,{actualIndexes,Range[Length[actualIndexes]]}]];
	newFs=faces/.conversionRules;
	{newVs,newFs}]


(* ::Subsubsection:: *)
(*Edges*)


Edges[faces_]:=Flatten[TriangleToEdges/@faces,1]


EdgeAngleRules[verts_,faces_]:=
	Dispatch[Flatten[MapThread[Rule,{VertexEdges[faces],VertexEdgeAngles[verts,faces]},2]]]


EdgeLengthRules[verts_,faces_]:=
	Dispatch[Flatten[MapThread[Rule,{VertexEdges[faces],VertexEdgeLengths[verts,faces]},2]]]


EdgeSquaredLengthRules[verts_,faces_]:=
	Dispatch[Flatten[MapThread[Rule,{VertexEdges[faces],VertexEdgeSquaredLengths[verts,faces]},2]]]


EdgeDifferenceRules[verts_,faces_]:=
	Dispatch[Flatten[MapThread[Rule,{VertexEdges[faces],VertexEdgeDifferences[verts,faces]},2]]]


(* ::Subsubsection:: *)
(*Some handy tools for messing w/ WRI GraphicsComplex*)


GeometryTools::nographics = "Input doesn't contain a GraphicsComplex[]";


Vertices[obj_]:=Module[{gcpos,theGC},
	gcpos=Position[obj,GraphicsComplex,{0,\[Infinity]},Heads->True];
	If[gcpos=={},Message[GeometryTools::nographics];Return[]];
	theGC=Extract[obj,Drop[First[gcpos],-1]];
	theGC[[1]]]


Faces[obj_]:=Module[{gcpos,theGC,ppos},
	gcpos=Position[obj,GraphicsComplex,{0,\[Infinity]},Heads->True];
	If[gcpos=={},Message[GeometryTools::nographics];Return[]];
	
	theGC=Extract[obj,Drop[First[gcpos],-1]];
	ppos=Position[theGC,Polygon,{0,\[Infinity]},Heads->True];
	If[ppos=={},Return[{}]];

	Extract[theGC,Append[Drop[First[ppos],-1],1]]]


Normals[obj_]:=Module[{gcpos,theGC,npos},
	gcpos=Position[obj,GraphicsComplex,{0,\[Infinity]},Heads->True];
	If[gcpos=={},Message[GeometryTools::nographics];Return[]];
	
	theGC=Extract[obj,Drop[First[gcpos],-1]];
	npos=Position[theGC,VertexNormals,{0,\[Infinity]}];
	If[npos=={},Return[{}]];

	Extract[theGC,Append[Drop[First[npos],-1],2]]]



TrianglesQ[f_]:=Union[Length/@f]=={3}


(* ::Subsubsection:: *)
(*Normals*)


(* ::Text:: *)
(*Normal calculator (see above helpers for the real machinery)*)


Options[ApproximateVertexNormals]={Method->Automatic};


ApproximateVertexNormals::badmeth="Invalid Method - `1`";


ApproximateVertexNormals[verts_,faces_,OptionsPattern[]]:=Module[{meth},
meth=OptionValue[Method];

Switch[meth,
	Automatic,centroidAngleWeightedNormals[verts,faces],

	"CentroidWeighted", centroidWeightedNormals[verts,faces],
	"CentroidAngleWeighted", centroidAngleWeightedNormals[verts,faces],

	"AreaWeighted",areaWeightedNormals[verts,faces],
	"AreaAngleWeighted", areaAngleWeightedNormals[verts,faces],

	"Meyer", meyerNormals[verts,faces],

	"Simple",meanNormals[verts,faces],

	_,Message[ApproximateVertexNormals::badmeth,meth];{}]]



(* ::Subsubsection::Closed:: *)
(*Old Connectivty Stuff... depricated*)


ComplexConnectivity[verts_,faces_]:=Module[{edges, vfaces},
	edges=Table[{},{Length[verts]}];
	Map[Map[AppendTo[edges[[#[[1]]]],#[[2]]]&,Permutations[#,{2}]]&,faces];
	edges=Union/@edges;

	vfaces=Table[{},{Length[verts]}];
	Do[Map[AppendTo[vfaces[[#]],f]&,faces[[f]]],{f,1,Length[faces]}];
	vfaces=Union/@vfaces;

	{edges, vfaces}]


ComplexConnectivity[faces_]:=Module[{edges, vfaces},
	edges=Table[{},{Max[faces]}];
	Map[Map[AppendTo[edges[[#[[1]]]],#[[2]]]&,Permutations[#,{2}]]&,faces];
	edges=Union/@edges;

	vfaces=Table[{},{Max[faces]}];
	Do[Map[AppendTo[vfaces[[#]],f]&,faces[[f]]],{f,1,Length[faces]}];
	vfaces=Union/@vfaces;

	{edges, vfaces}]


(* ::Text:: *)
(*This computes both face and vertex normals, presuming you already have face connectivity.*)


ComplexNormals[verts_,faces_,vfaces_]:=Module[{vnormals,fnormals},
	fnormals=FaceNormals[verts,faces];
	vnormals=Map[Normalize[Total[fnormals[[#]]]]&,vfaces];

	{fnormals,vnormals}]


(* ::Text:: *)
(*Slowest, computes connectivity first.*)


ComplexNormals[verts_,faces_]:=Module[{vnormals,fnormals,vfaces,edges},
	{edges,vfaces}=ComplexConnectivity[faces];
	fnormals=FaceNormals[verts,faces];
	vnormals=Map[Normalize[Total[fnormals[[#]]]]&,vfaces];

	{fnormals,vnormals}]


(* ::Text:: *)
(*Smoother*)


SmoothVertexNormals[vnormals_,faces_,depth_:1]:=Module[{edges,vfaces},
	{edges,vfaces}=ComplexConnectivity[faces];
	Map[Normalize[Mean[vnormals[[#]]]]&,edges]]


ComplexGeometry[verts_,faces_]:=Module[{edges, vfaces,vnormals,fnormals},
	{edges,vfaces}=ComplexConnectivity[verts,faces];
	{fnormals,vnormals}=ComplexNormals[verts,faces,vfaces];
	{verts,faces,edges,vfaces,fnormals,vnormals}]


(* ::Subsection:: *)
(*End Stuff*)


End[];


Protect[{
	FacesToTriangles,TriangleToEdges,ObtuseQ,TrianglesQ,

	FaceNormals,FaceAngles,FaceAreas,FaceBarycenters,FaceCircumcenters,

	VertexFaces,VertexEdges,VertexNeighbors,
	VertexFaceNormals,VertexFaceAreas,VertexFaceAngles,VertexVertexAngles,
	VertexEdgeAngles,VertexEdgeLengths,VertexEdgeDifferences,VertexEdgeSquaredLengths,
	CenterVertices,ReduceVertices,

	Edges,EdgeAngleRules,EdgeLengthRules,EdgeDifferenceRules,EdgeSquaredLengthRules,

	Vertices,Faces,Normals,

	ApproximateVertexNormals
}];


EndPackage[]
