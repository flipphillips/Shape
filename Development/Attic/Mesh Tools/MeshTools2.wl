(* ::Package:: *)

(* ::Title:: *)
(*Region Tools*)


(* ::Text:: *)
(*Flip Phillips \[Dash] OSU, Skidmore*)


(* ::Text:: *)
(*\[Copyright] 1992+*)


(* ::Text:: *)
(*This version current as of Winter 2016*)


(* ::Section:: *)
(*Here we go*)


(* ::Text:: *)
(*A set of tools that (d-)evolved from work on shape representation I've collected over the years.*)


(* ::Subsection:: *)
(*Revision Notes*)


(* ::Text:: *)
(*July 2015 - Packagized. Lord help us. Made changes to ToVFN for default 10.2 behavior. *)
(*Jan 2016 - Some additions to ToVFN, etc, to make accomidation for MeshRegion behavior.*)
(*Apr 2016 - This is a stripped down version that uses the MeshObject data structure available in Graphics`Region`*)


(* ::Section:: *)
(*Packaging*)


BeginPackage["RegionTools`" ];


MeshTools::usage = "A package for doing things to meshes.";


Unprotect[FacesToTriangles,TriangleToEdges,TriangleToEdgeVectors,TriangleToBasisVectors,
	TriangleArea,TriangleVoroniArea,TriangleMeyerArea,
	TriangleNormal,TriangleAngles,
	TriangleBarycenter,TriangleCircumcenter,TriangleCircumradius,
	TrianglesQ,ObtuseQ];
Unprotect[ApproximateVertexNormals];
Unprotect[VertexFrame,ApproximateVertexCurvatures];


Begin["`Private`"]


(* ::Section:: *)
(*Triangles*)


(* ::Text:: *)
(*Not quite as well programmed as I should, but basically assume a list of 3 coordinates.*)


ToTriangleList[o_]:=FacesToTriangles[verts_List,faces_List]:=verts[[#]]&/@faces


TriangleToEdges[t_List]:=Partition[t,2,1,1]


TriangleToEdgeVectors[t_List]:=Subtract@@@Reverse/@TriangleToEdges[t]


TriangleToBasisVectors[{p_,v1_,v2_}]:={(v1-p),(v2-p)}


TriangleArea[t_]:=Norm[Cross@@TriangleToBasisVectors[t]]/2


TriangleVoroniArea[t_]:=Module[{c,b,p},
	p=t[[1]];
	c=TriangleCircumcenter[t];
	b=p+TriangleToBasisVectors[t]/2;
	TriangleArea[{p,p+b[[1]],c}]+TriangleArea[{p,p+b[[2]],c}]]


TriangleMeyerArea[t_]:=
	If[!ObtuseQ[t],TriangleVoroniArea[t],
		If[!ObtuseQ[{First[TriangleAngles[t]]}],TriangleArea[t]/2,TriangleArea[t]/4]]


TriangleNormal[t_List]:=Normalize[Cross@@TriangleToBasisVectors[t]]


TriangleAngles[t_List]:=Module[{eds,vecs},
	eds=NestList[RotateLeft,t,2];
	vecs=Map[TriangleToBasisVectors,eds];
	VectorAngle@@@vecs]


TriangleBarycenter[t_]:=Mean[t]


TriangleCircumcenter[{aa_,bb_,cc_}]:=Module[{a,b,n},
	a=aa-cc;
	b=bb-cc;
	n=a\[Cross]b;

	(Norm[a]^2 b-Norm[b]^2 a)\[Cross]n/(2Norm[n]^2)+cc]


TriangleCircumradius[{aa_,bb_,cc_}]:=Module[{a,b,n},
	a=aa-cc;
	b=bb-cc;
	n=a\[Cross]b;

	(Norm[a]Norm[b]Norm[a-b])/(2Norm[n])]


TrianglesQ[f_]:=AllTrue[Length[#]==3&/@f]


ObtuseQ[t_]:=AnyTrue[t,(#>=\[Pi]/2)&]


(* ::Section:: *)
(*Normals*)


(* ::Subsection:: *)
(*Methods*)


(* ::Text:: *)
(*Private stuff*)


meanNormals[verts_List,faces_List]:=Module[{vf,fn,angleAreaNormal},
	vf=VertexFaces[faces];
	fn=FaceNormals[verts,faces];

	Normalize[Mean[fn[[#]]]]&/@vf
]


areaWeightedNormals[verts_List,faces_List]:=Module[{vf,fn,fa,angleAreaNormal},
	vf=VertexFaces[faces];
	fn=FaceNormals[verts,faces];
	fa=FaceAreas[verts,faces];

	Normalize[Mean[fa[[#]] fn[[#]]]]&/@vf
]


areaAngleWeightedNormals[verts_List,faces_List]:=Module[{vf,va,fn,fa},
	vf=VertexFaces[faces];
	va=VertexVertexAngles[verts,faces];
	fn=FaceNormals[verts,faces];
	fa=FaceAreas[verts,faces];
	
	Normalize/@MapThread[Mean[#2 fa[[#1]] fn[[#1]]]&,{vf,va}]]


centroidWeightedNormals[verts_List,faces_List]:=Module[{vf,fn,fc,cw,centroidNormal},
	vf=VertexFaces[faces];
	fn=FaceNormals[verts,faces];
	fc=FaceBarycenters[verts,faces];

	(* mean distance from the centroids *)
	centroidNormal[v_]:=Module[{cf,w},
		cf=vf[[v]];
		w=1/EuclideanDistance[verts[[v]],#]&/@fc[[cf]];
		Normalize[Mean[fn[[cf]]*w]]];

	centroidNormal/@Range[Max[faces]]]


centroidAngleWeightedNormals[verts_List,faces_List]:=Module[{vf,fn,fc,fx,cw,centroidNormal},
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


meyerNormals[verts_List,faces_List]:=Module[{fn,vf,fa},
	vf=VertexFaces[faces];
	fa=FaceMeyerAreas[verts,faces];
	fn=FaceNormals[verts,faces];

	Normalize[Mean[fa[[#]] fn[[#]]]]&/@vf]


maxNormals[verts_List,faces_List]:=Module[{vf,fa,fn,tris,w},
	vf=VertexFaces[faces];
	fa=FaceAreas[verts,faces];
	fn=FaceNormals[verts,faces];
	tris=FacesToTriangles[verts,faces];

	w=MapThread[(#1/ Total[(Norm[#]^2)&/@TriangleToBasisVectors[#2]])&,{fa,tris}];

	Normalize[Mean[w[[#]] fn[[#]]]]&/@vf]


(* ::Subsection:: *)
(*Wrappers*)


Options[ApproximateVertexNormals]={Method->Automatic};


ApproximateVertexNormals::badmeth="Invalid Method - `1`";


ApproximateVertexNormals[verts_List,faces_List,opts:OptionsPattern[]]:=Module[{meth},
	meth=OptionValue[Method];

	Switch[meth,
		Automatic, maxNormals[verts,faces],

		"CentroidWeighted", centroidWeightedNormals[verts,faces],
		"CentroidAngleWeighted", centroidAngleWeightedNormals[verts,faces],

		"AreaWeighted",areaWeightedNormals[verts,faces],
		"AreaAngleWeighted", areaAngleWeightedNormals[verts,faces],

		"Max", maxNormals[verts,faces],
		"Meyer", meyerNormals[verts,faces],

		"Simple",meanNormals[verts,faces],

		_,Message[ApproximateVertexNormals::badmeth,meth];{}]]


ApproximateVertexNormals[vfn_Association,opts:OptionsPattern[]]:=
	Append[vfn,"Normals"->ApproximateVertexNormals[vfn[["Vertices"]],vfn[["Faces"]],opts]]


(* ::Section:: *)
(*Differential Geometry*)


(* ::Subsection:: *)
(*Utility*)


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


(* ::Subsection:: *)
(*Methods*)


(* ::Text:: *)
(*Private stuff*)


oneRingCurvatures[verts_List,faces_List,normals_List]:=
	MapThread[VertexFrame,{verts,normals,verts[[#]]&/@VertexNeighborhoods[faces]}]


oneRingCurvatures[verts_List,faces_List,normals_List]:=
	Map[VertexFrame @@ #&,Transpose[{verts,normals,verts[[#]]&/@VertexNeighborhoods[faces]}]]


radiusBasedCurvatures[verts_List,faces_List,normals_List,r_]:=Module[{vn},
	vn=VertexNeighborhoodsByDistance[verts,faces,N[r]];
	MapThread[VertexFrame,
		{verts,normals,verts[[#]]&/@vn}]]


radiusBasedCurvatures[verts_List,faces_List,normals_List,r_]:=Module[{vn},
	vn=VertexNeighborhoodsByDistance[verts,faces,N[r]];
	Map[VertexFrame@@ #&,Transpose[
		{verts,normals,verts[[#]]&/@vn}]]]


radiusBasedCurvaturesSubsample::toodeep="Subsample too big - `1`";


radiusBasedCurvaturesSubsample[verts_List,faces_List,normals_List,r_Real,maxn_Integer:5]:=Module[{vn,ssize,shood,vnr},
	
	vn=VertexNeighborhoodsByDistance[verts,faces,N[r]];
	ssize=Min[Min[Length/@vn],maxn];
	shood= RandomSample[#,ssize]&/@vn;
	
	MapThread[VertexFrame,
		{verts,normals,verts[[#]]&/@shood}]]


radiusBasedCurvaturesSubsample[verts_List,faces_List,normals_List,r_Real,maxn_Integer:5]:=Module[{vn,ssize,shood,vnr},
	
	vn=VertexNeighborhoodsByDistance[verts,faces,N[r]];
	ssize=Min[Min[Length/@vn],maxn];
	shood= RandomSample[#,ssize]&/@vn;
	
	Map[VertexFrame@@ #&,Transpose[
		{verts,normals,verts[[#]]&/@shood}]]]


radiusBasedCurvaturesMaximum[verts_List,faces_List,normals_List,r_Real,maxn_Integer:5]:=Module[{vn,ssize,shood,vnr},
	
	vn=VertexNeighborhoodsFarthest[verts,faces,N[r],maxn];
	
	MapThread[VertexFrame,
		{verts,normals,verts[[#]]&/@vn}]]


radiusBasedCurvaturesMaximum[verts_List,faces_List,normals_List,r_Real,maxn_Integer:5]:=Module[{vn,ssize,shood,vnr},
	
	vn=VertexNeighborhoodsFarthest[verts,faces,N[r],maxn];
	
	Map[VertexFrame@@ #&,Transpose[
		{verts,normals,verts[[#]]&/@vn}]]]


(* ::Subsection:: *)
(*Wrapper*)


(* ::Text:: *)
(*Some quick tests show that MaxPoints > 5 is unnecessary*)


Options[ApproximateVertexCurvatures]={Method->Automatic, "Radius"->Automatic, "MaxPoints"->5};


ApproximateVertexCurvatures::badmeth="Invalid Method - `1`";


(* ::Text:: *)
(*If no radius is specifued, make it 1/10 the diagonal size of the object... for no precise reason other than, just because.*)


ApproximateVertexCurvatures[verts_List,faces_List,normals_List:{},opts:OptionsPattern[]]:=Module[{meth,ns,r},
	(* no normals, make some! *)
	ns=If[normals=={},ApproximateVertexNormals[verts,faces],normals];

	meth = OptionValue[Method];
	r=OptionValue["Radius"];
	r=If[r===Automatic,EuclideanDistance@@Transpose[BoundingBox[verts]]/10.0,r];

	(* how to do it? *)
	Switch[meth,
		Automatic,radiusBasedCurvaturesSubsample[verts,faces,ns,
			r,OptionValue["MaxPoints"]],
		"Local",oneRingCurvatures[verts,faces,ns],
		_,Message[ApproximateVertexCurvatures::badmeth,meth];{}]]


ApproximateVertexCurvatures[vfn_Association,opts:OptionsPattern[]]:=Module[{dg},
	dg=ApproximateVertexCurvatures[vfn[["Vertices"]],vfn[["Faces"]],vfn[["Normals"]],opts];
	Append[vfn,<|"Curvatures"->dg[[All,1]],"PrincipalDirections"->dg[[All,2]],
		"Method"->OptionValue[Method],"Radius"->OptionValue["Radius"]|>]]


(* ::Section:: *)
(*Packaging*)


End[];


Protect[FacesToTriangles,TriangleToEdges,TriangleToEdgeVectors,TriangleToBasisVectors,
	TriangleArea,TriangleVoroniArea,TriangleMeyerArea,
	TriangleNormal,TriangleAngles,
	TriangleBarycenter,TriangleCircumcenter,TriangleCircumradius,
	TrianglesQ,ObtuseQ];
Protect[ApproximateVertexNormals];
Protect[VertexFrame,ApproximateVertexCurvatures];


EndPackage[];


(* ::Section:: *)
(*The End*)
