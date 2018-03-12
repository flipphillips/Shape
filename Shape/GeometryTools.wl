(* ::Package:: *)

(* ::Section:: *)
(*To/From Representations*)


ToGraphicsComplex[verts_List,faces_List,normals_List:{}]:=GraphicsComplex[verts,{EdgeForm[],Polygon[faces]},VertexNormals->normals]


ToGraphicsComplex[vfn_Association]:=ToGraphicsComplex[vfn[["Vertices"]],vfn[["Faces"]],vfn[["Normals"]]]


ImportVFN[fname_]:=Module[{v,f,n},
	v=Import[fname,"VertexData"];
	f=Import[fname,"PolygonData"];
	n=Import[fname,"VertexNormals"];
	n=If[n=={},ApproximateVertexNormals[v,f],n];
	<|"Vertices"->v,"Faces"->f,"Normals"->n|>]


ToVFN[obj_]:=Module[{ns},
	ns=If[Normals[obj]=={},ApproximateVertexNormals[Vertices[obj],Faces[obj]],Normals[obj]];

	<|"Vertices"->Vertices[obj],"Faces"->Faces[obj],"Normals"->ns|>]


ToVFN[v_,f_,n_:{}]:=Module[{ns},
	ns=If[n=={},ApproximateVertexNormals[v,f],n];

	<|"Vertices"->v,"Faces"->f,"Normals"->ns|>]


ToNormalLines[verts_List,normals_List]:=Line/@Transpose[{verts,verts+normals}]


ToFrameLines[verts_List,normals_List,pds_,len_:0.33]:=
	MapThread[{{Red,Line[{#1,#1+len #3[[1]]}]},{Green,Line[{#1,#1+len #3[[2]]}]},{Blue,Line[{#1,#1+len #2}]}}&,
	{verts,normals,pds}]


(* ::Section::Closed:: *)
(*GraphicsComplex Tools*)


extractGC[obj_Graphics3D]:=Module[{gcpos},
	If[Head[obj]===GraphicsComplex,Return[obj],
	  gcpos=Position[obj,GraphicsComplex,{0,\[Infinity]},Heads->True];
	  If[gcpos=={},Message[MeshTools::nographics];Return[]];
	  Return[Extract[obj,Drop[First[gcpos],-1]]]]]


Vertices[obj_Graphics3D]:=Module[{theGC},
	theGC=extractGC[obj];
	theGC[[1]]]


Faces[obj_Graphics3D]:=Module[{theGC,ppos},
	theGC=extractGC[obj];

	(* 10.2 changed the default behavior of Import[], and even changed the type of
	GraphicsComplex returned *)

	ppos=Position[theGC,Polygon,{0,\[Infinity]},Heads->True];
	If[ppos=={},
		ppos=Position[theGC,Triangle,{0,\[Infinity]},Heads->True];
		If[ppos=={},Return[{}]]];

	Extract[theGC,Append[Drop[First[ppos],-1],1]]]


Normals[obj_Graphics3D]:=Module[{gcpos,theGC,npos},
	theGC=extractGC[obj];

	npos=Position[theGC,VertexNormals,{0,\[Infinity]}];
	If[npos=={},Return[{}]];

	Extract[theGC,Append[Drop[First[npos],-1],2]]]


(* ::Section:: *)
(*Global Geometry*)


ReduceVertices[verts_List,faces_List]:=Module[{actualIndexes,newVs,conversionRules,newFs},
	actualIndexes=faces//Flatten//Union;
	newVs=verts[[actualIndexes]];
	conversionRules=Dispatch[MapThread[Rule,{actualIndexes,Range[Length[actualIndexes]]}]];
	newFs=faces/.conversionRules;
	{newVs,newFs}]


Centroid[verts_List]:=Mean/@Transpose[verts];


Centroid[vfn_Association]:=Centroid[vfn[["Vertices"]]]


CenterVertices[verts_List]:=Module[{centroid},
	centroid=Centroid[verts];
	#-centroid&/@verts]


CenterVertices[vfn_Association]:=
	Append[vfn,"Vertices"->CenterVertices[vfn["Vertices"]]]


BoundingBox[verts_List]:=
	{Min[#],Max[#]}&/@Transpose[verts]


BoundingBox[vfn_Association]:=BoundingBox[vfn[["Vertices"]]]


(* ::Section::Closed:: *)
(*Transformation Utilities*)


RotateCoordinateSystem[{u_,v_},n_]:=RotationTransform[{Cross[u,v],n}]/@{u,v}


(* ::Section::Closed:: *)
(*Triangles*)


FacesToTriangles[verts_List,faces_List]:=verts[[#]]&/@faces


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


(* ::Section::Closed:: *)
(*Faces*)


FaceNormals[verts_List,faces_List]:=TriangleNormal /@ FacesToTriangles[verts,faces]


FaceNormals[vfn_Association]:=FaceNormals[vfn[["Vertices"]],vfn[["Faces"]]]


FaceAngles[verts_List,faces_List]:=TriangleAngles /@ FacesToTriangles[verts,faces]


FaceAngles[vfn_Association]:=FaceAngles[vfn[["Vertices"]],vfn[["Faces"]]]


FaceAreas[verts_List,faces_List]:=TriangleArea /@ FacesToTriangles[verts,faces]


FaceAreas[vfn_Association]:=FaceAreas[vfn[["Vertices"]],vfn[["Faces"]]]


FaceVoroniAreas[verts_List,faces_List]:=TriangleVoroniArea /@ FacesToTriangles[verts,faces]


FaceVoroniAreas[vfn_Association]:=FaceVoroniAreas[vfn[["Vertices"]],vfn[["Faces"]]]


FaceMeyerAreas[verts_List,faces_List]:=TriangleMeyerArea /@ FacesToTriangles[verts,faces]


FaceMeyerAreas[vfn_Association]:=FaceMeyerAreas[vfn[["Vertices"]],vfn[["Faces"]]]


FaceBarycenters[verts_List,faces_List]:=TriangleBarycenter/@ FacesToTriangles[verts,faces]


FaceBarycenters[vfn_Association]:=FaceBarycenters[vfn[["Vertices"]],vfn[["Faces"]]]


FaceCircumcenters[verts_List,faces_List]:=TriangleCircumcenter/@ FacesToTriangles[verts,faces]


FaceCircumcenters[vfn_Association]:=FaceCircumcenters[vfn[["Vertices"]],vfn[["Faces"]]]


(* ::Section::Closed:: *)
(*Vertices*)


(* ::Subsection::Closed:: *)
(*Face Neighbor Related*)


(* ::Text:: *)
(*AppendTo is slow, so there's probably a better, more functional way to do this, but alas...*)


VertexFaces[faces_List]:=Module[{vfaces},
	vfaces=Table[{},{Max[faces]}];
	Do[Map[AppendTo[vfaces[[#]],f]&,faces[[f]]],{f,1,Length[faces]}];
	DeleteDuplicates/@vfaces]


VertexFaceNormals[verts_List,faces_List]:=Module[{vf,fn},
	vf=VertexFaces[faces];
	fn=FaceNormals[verts,faces];

	fn[[#]]&/@vf]


VertexFaceAngles[verts_List,faces_List]:=Module[{vf,f\[Theta]},
	vf=VertexFaces[faces];
	f\[Theta]=FaceAngles[verts,faces];

	f\[Theta][[#]]&/@vf]


VertexFaceAreas[verts_List,faces_List]:=Module[{vf,fa},
	vf=VertexFaces[faces];
	fa=FaceAreas[verts,faces];

	fa[[#]]&/@vf]


VertexVertexAngles[verts_List,faces_List]:=Module[{vf, vf\[Theta], vpos},
	vf=VertexFaces[faces];
	vf\[Theta]=VertexFaceAngles[verts,faces];

	(* the vertex list position on each face connected to that vertex *)
	vpos=Function[v,Position[#,v]&/@faces[[vf[[v]]]]]/@Range[Max[faces]];
	(* extract the angles at each face there *)
	Flatten/@MapThread[Extract,{vf\[Theta],vpos},2]]


(* ::Subsection:: *)
(*Edge Neighbor related*)


VertexEdges[faces_List,depth_:1]:=
	MapThread[Function[{v,l},Map[{v,#}&,l]],{Range[Max[faces]],VertexNeighborhoods[faces,depth]}]


VertexEdges[vfn_Association,depth_:1]:=
	VertexEdges[vfn["Faces"],depth]


VertexEdgeAngles[verts_List,faces_List]:=Module[{ve,vf,vf\[Theta],AVertexEdgeAngle},
	ve=VertexEdges[faces];
	vf=VertexFaces[faces];
	vf\[Theta]=VertexFaceAngles[verts,faces];

	AVertexEdgeAngle[edges_,tris_,angles_]:=Module[{edgetris,missing,where},
		edgetris=faces[[tris]];
		missing=Flatten[MapThread[Complement,{edgetris,edges}]];
		where=MapThread[FirstPosition,{edgetris,missing}];
		Flatten[MapThread[Part,{angles,where}]]];

	MapThread[AVertexEdgeAngle,{ve,vf,vf\[Theta]}]]


VertexEdgeLengths[verts_List,faces_List]:=Map[Apply[EuclideanDistance,#]&,Map[verts[[#]]&,VertexEdges[faces],{2}],{2}]


VertexEdgeLengths[vfn_Association]:=VertexEdgeLengths[vfn["Vertices"],vfn["Faces"]]


VertexEdgeSquaredLengths[verts_List,faces_List]:=Map[Apply[SquaredEuclideanDistance,#]&,Map[verts[[#]]&,VertexEdges[faces],{2}],{2}]


VertexEdgeDifferences[verts_List,faces_List]:=Map[Apply[Subtract,#]&,Map[verts[[#]]&,VertexEdges[faces],{2}],{2}]


(* ::Text:: *)
(*Angles opposite the vertex*)


VertexEdgeAlphaBetas[verts_List,faces_List]:=Module[{ve,ear},
	ve=VertexEdges[faces];
	ear=EdgeAngleRules[verts,faces];

	(({#,Reverse[#]}/.ear)&/@ #)&/@ve ]


VertexEdgeCotSum[verts_List,faces_List]:=Module[{veab},
	veab=VertexEdgeAlphaBetas[verts,faces];
	Map[Total,Cot[veab],{2}]]


(* ::Subsection::Closed:: *)
(*Vertex Neighbor Related*)


GrowNeighbors[vx_,faces_List,vf_List]:=DeleteDuplicates[Flatten[faces[[Flatten[vf[[vx]]]]]]]


(* ::Text:: *)
(*n-ring*)


VertexNeighbors[vx_,faces_List,vf_List,depth_Integer:1]:=DeleteCases[Nest[GrowNeighbors[#,faces,vf]&,vx,depth],vx]


VertexNeighborhoods[faces_List,depth_Integer:1]:=Module[{vf},
	vf=VertexFaces[faces];
	Table[VertexNeighbors[i,faces,vf,depth],{i,1,Max[faces]}]]


(* ::Text:: *)
(*radius-based*)


VertexNeighborsByDistance[vx_Integer,verts_List,faces_List,vf_List,r_]:=
	NestWhile[GrowNeighbors[#,faces,vf]&,{vx},
	(Max[(EuclideanDistance[verts[[vx]],verts[[#]]])&/@#]<r)&]


VertexNeighborhoodsByDistance[verts_List,faces_List,r_]:=Module[{vf},
	vf=VertexFaces[faces];
	Table[VertexNeighborsByDistance[i,verts,faces,vf,r],{i,1,Max[faces]}]]


VertexNeighborsSorted[vx_Integer,hood_List,verts_List]:=
	Part[Sort[{EuclideanDistance[verts[[vx]],verts[[#]]],#}&/@hood],All,2]


VertexNeighborhoodsNearest[verts_List,faces_List,r_Real:0.0,n_Integer:0]:=Module[{vbr,vsd,ntake},
	vbr=VertexNeighborhoodsByDistance[verts,faces,r];
	vsd=Table[VertexNeighborsSorted[i,vbr[[i]],verts],{i,1,Max[faces]}];

	If[n<=0,Return[vsd],
		ntake=Min[Min[Length/@vsd],n];
		Return[Take[#,ntake]&/@vsd]]]


VertexNeighborhoodsFarthest[verts_List,faces_List,r_Real:0.0,n_Integer:0]:=Module[{vbr,vsd,ntake},
	vbr=VertexNeighborhoodsByDistance[verts,faces,r];
	vsd=Table[Reverse[VertexNeighborsSorted[i,vbr[[i]],verts]],{i,1,Max[faces]}];

	If[n<=0,Return[vsd],
		ntake=Min[Min[Length/@vsd],n];
		Return[Take[#,ntake]&/@vsd]]]


(* ::Subsection::Closed:: *)
(*Vertex Differential Geometry*)


VertexFaceTangentVectors[verts_List,faces_List]:=(VertexEdgeCotSum[verts,faces]*VertexEdgeDifferences[verts,faces])


VertexMeanCurvatureNormals[verts_List,faces_List]:=Total[#]/2&/@ VertexFaceTangentVectors[verts,faces]


VertexFaceVoroniAreas[verts_List,faces_List]:=(VertexEdgeCotSum[verts,faces]*VertexEdgeSquaredLengths[verts,faces])/8


VertexVoroniAreas[verts_List,faces_List]:=Total[#]/8&/@ VertexFaceVoroniAreas[verts,faces]


VertexMeyerAreas[verts_List,faces_List]:=Module[{vf\[Theta],vfva,vv\[Theta],vfa,meyerWeights},
	vf\[Theta]=VertexFaceAngles[verts,faces];
	vfva=VertexFaceVoroniAreas[verts,faces];
	vv\[Theta]=VertexVertexAngles[verts,faces];
	vfa=VertexFaceAreas[verts,faces];

	meyerWeights[face\[Theta]_,voroniA_,vertex\[Theta]_,faceA_]:=Total[Map[
		If[!ObtuseQ[#[[1]]],#[[2]],
			If[!ObtuseQ[{#[[3]]}],#[[4]]/2,#[[4]]/4]]&,{face\[Theta],voroniA,vertex\[Theta],faceA}\[Transpose]]];
	
	MapThread[meyerWeights,{vf\[Theta],vfva,vv\[Theta],vfa}]]


(* ::Text:: *)
(*According to Meyer et al. (these are normal to the curve, not the surface, fwiw, eg, they're not oriented properly)*)


VertexMeanCurvatureNormals[verts_List,faces_List]:=Module[{vma,sum},
	vma=VertexMeyerAreas[verts,faces];
	sum=Map[Total,VertexFaceTangentVectors[verts,faces],{1}];
	sum/(2vma)]


(* ::Text:: *)
(*Normalize = surface normal (duh) and divide magnitude / 2 = Mean Curvature.*)


VertexMeanCurvatures[verts_List,faces_List]:=Module[{normals},
	normals=VertexFaceTangentVectors[verts,faces];
	Norm[#]/2&/@normals]	


VertexGaussianCurvatures[verts_List,faces_List]:=Module[{vv\[Theta],vma},
	vv\[Theta]=VertexVertexAngles[verts,faces];
	vma=VertexMeyerAreas[verts,faces];
	MapThread[(2\[Pi]-Total[#1])/#2&,{vv\[Theta],vma}]]


VertexCurvatures[verts_List,faces_List]:=Module[{mcs,gcs,kFromHK},
	mcs=VertexMeanCurvatures[verts,faces];
	gcs=VertexGaussianCurvatures[verts,faces];
	
	kFromHK[h_,k_]:=Module[{del},
		del= If[h^2-k < 0, 0, Sqrt[h^2-k]];
		{k1->h-del,k2->h+del}];

	MapThread[kFromHK,{mcs,gcs}]]


(* ::Section::Closed:: *)
(*Edges*)


Edges[faces_List]:=DeleteDuplicates[Sort/@Flatten[TriangleToEdges/@faces,1]]


Edges[vfn_Association]:=Edges[vfn["Faces"]]


EdgeAngleRules[verts_List,faces_List]:=
	Dispatch[Flatten[MapThread[Rule,{VertexEdges[faces],VertexEdgeAngles[verts,faces]},2]]]


EdgeLengthRules[verts_List,faces_List]:=
	Dispatch[Flatten[MapThread[Rule,{VertexEdges[faces],VertexEdgeLengths[verts,faces]},2]]]


EdgeSquaredLengthRules[verts_List,faces_List]:=
	Dispatch[Flatten[MapThread[Rule,{VertexEdges[faces],VertexEdgeSquaredLengths[verts,faces]},2]]]


EdgeDifferenceRules[verts_List,faces_List]:=
	Dispatch[Flatten[MapThread[Rule,{VertexEdges[faces],VertexEdgeDifferences[verts,faces]},2]]]


(* ::Section::Closed:: *)
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


(* ::Section::Closed:: *)
(*Differential Geometry*)


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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
(*Introspection*)


VFNInformation[vfn_Association]:=Module[{nv,nf,nn,edges,ne,bb,fas,vlens},
	nv=Length[vfn["Vertices"]];
	nf=Length[vfn["Faces"]];
	nn=Length[vfn["Normals"]];

	bb=BoundingBox[vfn];

	edges=Edges[vfn];
	ne=Length[edges];

	fas=Mean[FaceAreas[vfn]];
	vlens=Mean[Flatten[VertexEdgeLengths[vfn]]];

	Dataset[<|"NVertices"->nv,"NFaces"->nf,
	  "NNormals"->nn,"BoundingBox"->bb,"NEdges"->ne,
	  "EulerCharacteristic"->nv-ne+nf,
	"MeanFaceArea"->fas,
	"MeanEdgeLength"->vlens|>]]


(* ::Section:: *)
(*Debugging*)


DebugNormals[verts_List,faces_List,normals_List:{},len_:0.5]:=
	Graphics3D[{ToGraphicsComplex[verts,faces,normals],{Green,Thick,ToNormalLines[verts,len*normals]}}]


DebugFrames[verts_List,faces_List,normals_List,pds_,density_:0.1,len_:0.33]:=Module[{s},
	s=RandomSample[Range[Length[verts]],Round[density*Length[verts]]];
	Graphics3D[{ToGraphicsComplex[verts,faces,normals],{Thick,
		ToFrameLines[verts[[s]],normals[[s]],pds[[s]],len]}}]]


DebugFrames[vfn_Association,density_:0.1,len_:1.0]:=
	DebugFrames[vfn[["Vertices"]],vfn[["Faces"]],vfn[["Normals"]],vfn[["Frames"]],density,len]


(* ::Section:: *)
(*Generate Geometry*)


Needs["PolyhedronOperations`"]


TessSphere[depth_:3,base_:"Icosahedron"]:=TessSphere[depth,base]=Module[{o,v,f},
	o=Geodesate[PolyhedronData[base],depth];
	{v,f}=ReduceVertices[N[Vertices[o]],Faces[o]];
	<|"Vertices"->v,"Faces"->f|>]


JitterySphere[noise_:0.01,depth_:3,base_:"Icosahedron"]:=Module[{vfn,vx},
	vfn=TessSphere[depth,base];
	vx=(#+RandomReal[{-noise,noise},{3}])&/@vfn[["Vertices"]];
	<|"Vertices"->vx, "Faces"->vfn[["Faces"]]|>]
