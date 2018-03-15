

cleanRegion[r_]:=Module[{bb,scale},
	BoundaryMeshRegion[MeshCoordinates[r],MeshCells[r,2]]]/;BoundaryMeshRegionQ[r]

cleanRegion[r_]:=Module[{bb,scale},
	MeshRegion[MeshCoordinates[r],MeshCells[r,2]]]/;MeshRegionQ[r]


Options[RegionToVolume]={ColorFunction->(1&),ColorFunctionScaling->True,Thickness->1.0};

RegionToVolume[r_,res_,opts:OptionsPattern[]]:=
 Module[{bb,start,stop,step,pad,pfDistance,theD,cf,samples,dom,h,zero,thick,thresh},

	bb=RegionBounds[r];
	pad=Subtract@@Reverse@MinMax[bb]/res;
	{start,stop}=MinMax[bb]+{-pad,pad};
	step=(stop-start)/(res-1);
	samples=N[Table[{x,y,z},{x,start,stop,step},{y,start,stop,step},{z,start,stop,step}]];

	pfDistance=RegionDistance[RegionBoundary[r]];

	theD=Map[Append[#,pfDistance[#]]&,samples,{3}];

	cf=OptionValue[ColorFunction];
	dom=Length[cf[0,0,0,0]];
	h=Head[cf[0,0,0,0]];

	zero=If[dom<=1,h@0,h@@ConstantArray[0,dom]];
	
	thick=OptionValue[Thickness];
	thresh=thick/1.5*step;

	Map[If[Last[#]<thresh,cf@@#,zero]&,theD,{3}]]


Options[PointCloudToVolume]={ColorFunction->(1&),ColorFunctionScaling->True};

PointCloudToVolume[p_,res_,opts:OptionsPattern[]]:= Module[{pp,bb,ranges,starts,stops,step,bins},
	(* convert coordinate system properly? *)
	pp=#*{-1,-1,1}&/@p[[All,{3,2,1}]];

	bb=MinMax/@Transpose[pp];
	ranges=Subtract@@Reverse@#&/@bb;
	step=Max[ranges/(res-1)];
	{starts,stops}=Transpose[bb];

	bins=(Round[(#-starts)/step]+1)&/@pp;
	Image3D[SparseArray[Normal[Counts[bins]]]]]
