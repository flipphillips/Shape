(* ::Package:: *)

(* :Title: Shape *)

(* :Context: ExpTools`Shape1 *)

(* :Author: Flip Phillips *)

(* :Summary: Be quiet *)

(* :Package Version: 1.3 *)

(* :Copyright: Copyright (C) 1999-2014, Flip Phillips, All Rights Reserved. *)

(* :History: July13 - fixed data structure access and added options,
	May14 - added shape patches and ShapeComplex to do hard-core-from-really-raw data calcs *)

(* :Keywords: shape, statistics, metrics, geometry *)

(* :Limitations: Sky *)

(* :Discussion: *)



BeginPackage["ExpTools`Shape`",
	{"ExpTools`ShapeMetrics`","ExpTools`GeometryTools`"}];


Shape::usage="Shape is a package for computing various shape metrics, statistics, and their ilk.";


LocalShape::usage="LocalShape[v,vs,ns,edges,depth] given a vertex index 'v', a vertex list, 
	normal list, edge connectivity, and a neighiborhood radius distace give back k1, k2,
	and the frame. Uses a search through the tesselation depth 'depth' deep. Default = 1";


LocalShapeSampled::usage="LocalShapeSampled[v,vs,ns,edges,depth,neighborhoodSize] same as LocaShape
	but uses a subsampled 'neighborhoodSize'. This is great for dense meshes.";


LocalShapeExtreme::usage="LocalShapeExtreme[v,vs,ns,edges,meanEdgeLen,r,neighborhoodSize] uses a heuristic
	to subsample a little more wisely. Essentially picks out points at the periphery of the patch.";


LocalShapeOptimal::usage="LocalShapeExtreme[v,vs,ns,edges,meanEdgeLen,r,neighborhoodSize] uses a heuristic
	to subsample a little more wisely. A combination of ...Sampled and Extreme";


ObjectShape::usage="ObjectShape[obj,radius] given an imported 3D object, return a
	data structure with the local differential geometry defined at radius 'r' using local patch geometry.";


ObjectShapeSampled::usage="ObjectShapeSampled[obj,radius] given an imported 3D object, return a
	data structure with the local differential geometry at radius 'r' vis subsampling.";


ObjectShapeExtreme::usage="ObjectShapeExtreme[obj,radius] given an imported 3D object, return a
	data structure with the local differential geometry using a patch size defined by radius.";


ObjectShapeOptimal::usage="ObjectShapeOptimal[obj,radius] given an imported 3D object, return a
	data structure with the local differential geometry using a patch size defined by radius.";


ComplexShape::usage="ComplexShape[v,f,radius]  given an graphics complex, return a
	data structure with the local differential geometry using a patch size defined by radius.";


ObjectReport::usage="ObjectReport[obj] Just some simple object information.";


ShapeCategory::usage="ShapeCategory[shapeIndex] gives you a 1-8 shape category score as per the 
	old Koenderink stuff I adapted.";


ShapeColor::usage="ShapeColor[si] return the categorical color of shape index";


KoenderKuler::usage="KoenderKuler[si] returns an RGBColor consistent with the shape index";


Begin["`Private`"];


koenderKolor={
		RGBColor[0,1,0],
		RGBColor[0,1,1/2],
		RGBColor[0,1,1],
		RGBColor[1/2,1,1],
		RGBColor[1,1,1],
		RGBColor[1,1,1/2],
		RGBColor[1,1,0],
		RGBColor[1,1/2,0],
		RGBColor[1,0,0]
};


(* From Jan... can probably optimize a little bit... still, this has the best speed/accuracy *)


computeGeometry3[ locs_, norms_ ] :=  (* locs: the positions of the points to be fitted *)
							   (* norms: the normals at the points to be fitted  *)
							   (* The first item is the fiducial point.          *)
Module[
	{ 
		origin, originNormal, neighborVertices, normals,
		locations, (* Neighbor vertices with respect to "origin" vertex *)
		edgeLengths, (* The edgelengths for the neighbors *)
		eXsi, eEta={0,0,0}, eZeta, (* Local frame vectors *)
		transformation, (* {x,y,z} to {xsi,eta,zeta} *)
		transformedLocations, (* {xsi,eta,zeta} of neighbor vertices *)
		transformedNormals, (* {Nxsi,Neta,Nzeta} of idem *)
		coefficientMatrix, (* of set of linear equations for II fund. form *)
		constantVector, (* for this set of equations *)
		i, (* an index *)
		a20, a11, a02, (* Coefficients of quadratic part of Taylor series for zeta(xsi,eta) *)
		kappa1, kappa2, (* Principal curvatures *)
		e1, e2 (* Principal directions *)
	},
	
	origin           =    First[ locs ];
	neighborVertices =  Drop[ locs, 1 ];
	originNormal     =   First[ norms ];
	normals          = Drop[ norms, 1 ];
	
	(* Refer neighbors to origin vertex *)
	locations   = Map[  N[#-origin]&, neighborVertices ];
	edgeLengths = Map[ N[Sqrt[#.#]]&, locations ];
	
	(* Construct the local frame vectors *)
	eZeta = N[originNormal]; (* zeta direction is origin normal direction *)
		
	eXsi = N[ First[locations]]; (* xsi direction towards first neighbor vertex *)
	eXsi = eXsi - eZeta (eXsi.eZeta);
	eXsi = eXsi / Sqrt[eXsi.eXsi];
	
	eEta[[1]] = eZeta[[2]] eXsi[[3]] - eZeta[[3]] eXsi[[2]]; (* Complete frame *)
	eEta[[2]] = eZeta[[3]] eXsi[[1]] - eZeta[[1]] eXsi[[3]];
	eEta[[3]] = eZeta[[1]] eXsi[[2]] - eZeta[[2]] eXsi[[1]];
	
	transformation = { eXsi, eEta, eZeta }; (* {x,y,z} to {xsi,eta,zeta} *)
	
	transformedLocations = (* Express neighbor locations in local frame *)
		Map[ 
			(transformation.#)&, 
			locations
		];
		
	transformedNormals = (* idem for the normals *)
		Map[ 
			(transformation.#)&, 
			normals
		];

	(* Construct linear equations for the coefficients of the IInd fund.form *)
	
	(* Equations relating to deviation from tangent plane (xsi,eta)-plane *)
	coefficientMatrix =
		Table[
			{ 
				transformedLocations[[i,1]]^2,
				transformedLocations[[i,1]]*transformedLocations[[i,2]],
				transformedLocations[[i,2]]^2
			},
			{i,1,Length[neighborVertices]}
		];
		
	constantVector =
		Table[
			2 transformedLocations[[i,3]],
			{i,1,Length[neighborVertices]}
		];
				
	(* Add equations relating to normal deviations *)
	Do[
		AppendTo[
			coefficientMatrix,
			edgeLengths[[i]] { 
				transformedLocations[[i,1]],
				transformedLocations[[i,2]],
				0
			}
		];
		AppendTo[
			constantVector,
			- edgeLengths[[i]] transformedNormals[[i,1]]/
				If[transformedNormals[[i,3]]==0.,$MachineEpsilon,transformedNormals[[i,3]]]
		];
		AppendTo[
			coefficientMatrix,
			edgeLengths[[i]] { 
				0,
				transformedLocations[[i,1]],
				transformedLocations[[i,2]]
			}
		];
		AppendTo[
			constantVector,
			- edgeLengths[[i]] transformedNormals[[i,2]]/
				If[transformedNormals[[i,3]]==0.,$MachineEpsilon,transformedNormals[[i,3]]]
		],
		{i,1,Length[normals]}
	];
		
	(* Solve for the coefficients of the IInd fund.form *)
	{a20,a11,a02} = 
		PseudoInverse[ 
			coefficientMatrix,
			Tolerance->0.001
		].constantVector;
		
	(* Find principal directions and principal curvatures *)
	{{kappa1,kappa2},{e1,e2}} = Eigensystem[ {{a20,a11},{a11,a02}} ];
	If[
		kappa1<kappa2, (* Sort principal curvatures *)
		{kappa1,kappa2}={kappa2,kappa1};
		{e1,e2}={e2,e1};
	];
	If[
		e1[[1]] e2[[2]] - e1[[2]] e2[[1]] < 0, (* Orient principal frame *)
		e2 = -e2;
	];
	
	(* Prepare the output *)
	{
		{kappa2,kappa1}, (* Principal curvatures are differential invariants *)
		{
			e1[[1]] eXsi + e1[[2]] eEta, (* Express principal directions *)
			e2[[1]] eXsi + e2[[2]] eEta  (* in terms of Cartesian XYZ *)
		}
	}
];


Unprotect[{LocalShape,LocalShapeSampled,LocalShapeExtreme,LocalShapeOptimal,
		ObjectShape,ObjectShapeSampled,ObjectShapeExtreme,ObjectShapeOptimal,
		ComplexShape,
		ShapeCategory,ShapeColor,KoenderKuler,ObjectReport}];


(* straight local shape, using a depth of 'depth' here, uses all the neighbors *)


LocalShape[v_,vs_,ns_,edges_,depth_:1]:=Module[
	{al},
	al=VertexNeighbors[v,edges,Ceiling[depth]];
	computeGeometry3[Prepend[vs[[al]],vs[[v]]],Prepend[ns[[al]],ns[[v]]]]
]


(* subsample for speed... when dist gets big, strictly local shape gets slow, this 
	speeds it up,
	still uses 'depth' *)


LocalShapeSampled[v_,vs_,ns_,edges_,depth_:1,neighborhoodSize_:6]:=Module[
	{al,sample,nn},

	al=VertexNeighbors[v,edges,Ceiling[depth]];
	nn=RandomSample[al,Min[{Length[al],neighborhoodSize}]];

	computeGeometry3[Prepend[vs[[nn]],vs[[v]]],Prepend[ns[[nn]],ns[[v]]]]
]


(* finally, this does the resampling -and- re-checks the distances (not geodesic, alas) and only takes
    vertices that are within the r, this needs edgeLength instead of depth...
	we calsulate depth within, extreme, because it takes the 'edge' vertices of the patch *)


LocalShapeExtreme::toofew="Too few neighbors - `1` (need at least `2`), increase radius or use denser sampling.";


LocalShapeExtreme[v_,vs_,ns_,edges_,meanEdgeLen_,r_,neighborhoodSize_:12]:=Module[
	{al,sample,nn,depth,
	dists,candidates,nCandidates},
	
	(* get the adjacency list *)
	depth=Ceiling[r/meanEdgeLen]; (* note, multiply meanEdgeLen by some 'safety factor' to get more points *)
	al=VertexNeighbors[v,edges,depth];

	(* compute the distances of each element in al, from v, sadly, euclidian and not geodesic, soon *)
	dists=EuclideanDistance[vs[[v]],vs[[#]]]&/@al;
	candidates=al[[Flatten[Position[dists,x_/;x<r&&x>0.]]]];
	
	(*Sow[{Length[candidates],Length[dists]}]; Debugging to see what % actually get used*)

	(* do we have enough to even do the math, man? *)
	nCandidates=Length[candidates];
(*	If[nCandidates<neighborhoodSize,
		Message[LocalShapeExtreme::toofew,nCandidates,neighborhoodSize]];*)
	
	(* if there are no candidates, just take the adjacency list, this is the same as 'local' *)
	nn=If[nCandidates >= neighborhoodSize,
		Take[Sort[{dists[[candidates]],candidates}\[Transpose]][[All,2]],-neighborhoodSize],
		al];

	computeGeometry3[Prepend[vs[[nn]],vs[[v]]],Prepend[ns[[nn]],ns[[v]]]]
]


LocalShapeOptimal[v_,vs_,ns_,edges_,meanEdgeLen_,r_,neighborhoodSize_:12]:=Module[
	{al,sample,nn,depth,
	dists,candidates,nCandidates},
	
	(* get the adjacency list *)
	depth=Ceiling[r/meanEdgeLen]; (* note, multiply meanEdgeLen by some 'safety factor' to get more points *)
	al=VertexNeighbors[v,edges,depth];

	(* compute the distances of each element in al, from v, sadly, euclidian and not geodesic, soon *)
	dists=EuclideanDistance[vs[[v]],vs[[#]]]&/@al;
	candidates=al[[Flatten[Position[dists,x_/;x<r&&x>0.]]]];
	nCandidates=Length[candidates];
	Assert[!MemberQ[candidates,v]];

	(* if there are no candidates, just take the adjacency list, this is the same as 'local' *)
	nn=If[nCandidates >= neighborhoodSize,
		RandomSample[candidates,Min[{nCandidates,neighborhoodSize}]],
		al];

	computeGeometry3[Prepend[vs[[nn]],vs[[v]]],Prepend[ns[[nn]],ns[[v]]]]
]


(* these all return a set of {k1,k2,{e1x,e1y,e1z},{e2x,e2y,e2z}} curvatures and PDs that are 
	consistent with the vertices of the obj. *)


ObjectShape[obj_,rad_:0.001]:=Module[{oRep,vs,nv,ns,ps,verts,faces,
		meanLen,d,
		edges,vfaces,fnormals,vnormals},

	(* get the object data from the GraphicsComplex[] data structure *)
	oRep=ObjectReport[obj,"Data"->True];

	vs="Vertices"/.oRep;
	nv="VertexCount"/.oRep;
	ps="Polygons"/.oRep;
	ns="Normals"/.oRep;
	
	{verts,faces,edges,vfaces,fnormals,vnormals}=ComputeGeometry[vs,ps];
	
	(* compute mean edge length and use that as a measure of depth, eg, how
		many points away are we going to sample *)
	meanLen=Mean[EuclideanDistance@@vs[[#]]&/@
		Flatten[Partition[#,2,1,1]&/@ps,1]];
	d=Ceiling[rad/meanLen];

	Table[LocalShape[dav,vs,ns,edges,d],{dav,1,nv}]
]


ObjectShapeSampled[obj_,rad_:0.001]:=Module[{oRep,vs,nv,ns,ps,verts,faces,
		meanLen,d,
		edges,vfaces,fnormals,vnormals},

	(* get the object data from the GraphicsComplex[] data structure *)
	oRep=ObjectReport[obj,"Data"->True];

	vs="Vertices"/.oRep;
	nv="VertexCount"/.oRep;
	ps="Polygons"/.oRep;
	ns="Normals"/.oRep;

	{verts,faces,edges,vfaces,fnormals,vnormals}=ComputeGeometry[vs,ps];
	
	(* compute mean edge length and use that as a measure of depth, eg, how
		many points away are we going to sample *)
	meanLen=Mean[EuclideanDistance@@vs[[#]]&/@
		Flatten[Partition[#,2,1,1]&/@ps,1]];
	d=Ceiling[rad/meanLen];

	Table[LocalShapeSampled[dav,vs,ns,edges,d],{dav,1,nv}]
]


ObjectShapeExtreme[obj_,rad_:0.001]:=Module[{oRep,vs,nv,ns,ps,verts,faces,
		meanLen,d,
		edges,vfaces,fnormals,vnormals},

	(* get the object data from the GraphicsComplex[] data structure *)
	oRep=ObjectReport[obj,"Data"->True];

	vs="Vertices"/.oRep;
	nv="VertexCount"/.oRep;
	ps="Polygons"/.oRep;
	ns="Normals"/.oRep;

	{verts,faces,edges,vfaces,fnormals,vnormals}=ComputeGeometry[vs,ps];
	
	meanLen=Mean[EuclideanDistance@@vs[[#]]&/@
		Flatten[Partition[#,2,1,1]&/@ps,1]];
	
	Table[LocalShapeExtreme[dav,vs,ns,edges,meanLen,rad],{dav,1,nv}]
]


ObjectShapeOptimal[obj_,rad_:0.001]:=Module[{oRep,vs,nv,ns,ps,verts,faces,
		meanLen,d,
		edges,vfaces,fnormals,vnormals},

	(* get the object data from the GraphicsComplex[] data structure *)
	oRep=ObjectReport[obj,"Data"->True];

	vs="Vertices"/.oRep;
	nv="VertexCount"/.oRep;
	ps="Polygons"/.oRep;
	ns="Normals"/.oRep;

	{verts,faces,edges,vfaces,fnormals,vnormals}=ComputeGeometry[vs,ps];

	meanLen=Mean[EuclideanDistance@@vs[[#]]&/@
		Flatten[Partition[#,2,1,1]&/@ps,1]];
	
	Table[LocalShapeOptimal[dav,vs,ns,edges,meanLen,rad],{dav,1,nv}]
]


ComplexShape[v_,f_,rad_:0.001]:=Module[{verts,faces,
		meanLen,d,
		edges,vfaces,fnormals,vnormals},
	
	{verts,faces,edges,vfaces,fnormals,vnormals}=ComputeGeometry[v,f];
	
	(* compute mean edge length and use that as a measure of depth, eg, how
		many points away are we going to sample *)
	meanLen=Mean[EuclideanDistance@@verts[[#]]&/@
		Flatten[Partition[#,2,1,1]&/@faces,1]];
	d=Ceiling[rad/meanLen];

	Table[LocalShape[dav,verts,vnormals,edges,d],{dav,1,Length[verts]}]
]


ObjectReport::nographics="Object doesn't have a GraphicsComplex[] to parse.";


ObjectReport::nopolys="Object doesn't have any Polygons[] to parse.";


Options[ObjectReport]={"Data"->False};


ObjectReport[obj_,OptionsPattern[]]:=Module[{nv,np,vs,ps,ns,gcpos,ppos,theGC,npos},
	
	(* on quick observation, I've seen that some OBJ files generate different GraphicsComplex
		structures, so we've sorta gotta work around that *)
	gcpos=Position[obj,GraphicsComplex,{0,\[Infinity]},Heads->True];
	If[gcpos=={},Message[ObjectReport::nographics];Return[]];
	theGC=Extract[obj,Drop[First[gcpos],-1]];
	
	(* first element in the GraphicsComplex are the vertices *)
	vs=theGC[[1]];

	(* Find the polygon list.  *)
	ppos=Position[obj,Polygon,{0,\[Infinity]},Heads->True];
	If[ppos=={},Message[ObjectReport::nopolys]];
	ps=Extract[obj,Append[Drop[First[ppos],-1],1]];

	(* find the normals if there are any *)
	npos=Position[obj,VertexNormals,{0,\[Infinity]}];
	ns=Extract[obj,Append[Drop[First[npos],-1],2]];

	Join[
	{"VertexCount"->Length[vs],
	 "PolygonCount"->Length[ps],
	 "HasNormals"->Length[ns]!=0,
	 "MeanEdgeLength"->Mean[EuclideanDistance@@vs[[#]]&/@
		Flatten[Partition[#,2,1,1]&/@ps,1]],
	 "Triangulated"->Union[Length/@ps]=={3},
	 "BoundingBox"->(({Min[#],Max[#]})&/@Transpose[vs])},
	If[OptionValue["Data"],{"Vertices"->vs,"Polygons"->ps,"Normals"->ns},{}]]]


ShapeCategory[si_] := 
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


ShapeColor[si_]:=koenderKolor[[ShapeCategory[si]]]/;(-1<=si<=1)


KoenderKuler[si_]:=Blend[koenderKolor,(si+1)/2.0]


End[]


Protect[{LocalShape,LocalShapeSampled,LocalShapeExtreme,LocalShapeOptimal,
		ObjectShape,ObjectShapeSampled,ObjectShapeExtreme,ObjectShapeOptimal,
		ComplexShape,
		ShapeCategory,ShapeColor,KoenderKuler,ObjectReport}];


EndPackage[]
