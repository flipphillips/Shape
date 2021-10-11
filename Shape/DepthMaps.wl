(* ::Package:: *)

(* private functions, safearctan *)
sarcTan[x_,y_]:=0.0/;x==0.0
sarcTan[x_,y_]:=ArcTan[x,y]


RidgeMap[img_,\[Sigma]_:1] := Module[{Lxx,Lxy,Lyy},
	{Lxx,Lxy,Lyy}=DerivativeFilter[img,{{0,2},{1,1},{2,0}},\[Sigma]];
	Chop[\[Sigma]^(3/2)/2 (Sqrt[(Lxx - Lyy)^2+ 4 Lxy^2]-Lxx-Lyy)]
]


RidgeMap[img_Image,s_:1]:=Image[RidgeMap[ImageData[img],s]]


TJunctionMap[img_,\[Sigma]_:1] := 
Module[{Lx,Ly,Lxx,Lxy,Lyy,Lxxx,Lxxy,Lxyy,Lyyy},
	{Lx,Ly}=DerivativeFilter[img,{{0,1},{1,0}},\[Sigma]];
	{Lxx,Lxy,Lyy}=DerivativeFilter[img,{{0,2},{1,1},{2,0}},\[Sigma]];
	{Lxxx,Lxxy,Lxyy,Lyyy}=DerivativeFilter[img,{{0,3},{1,2},{2,1},{3,0}},\[Sigma]];
	Chop[-Lx^5 Lxyy+Ly^4 (2 Lxy^2-Lxxy Ly+Lxx Lyy)+Lx Ly^3 (6 Lxx Lxy-Lxxx Ly+2 Lxyy Ly-6 Lxy Lyy)+Lx^3 Ly (-6 Lxx Lxy-Lxxx Ly+Lxyy Ly+6 Lxy Lyy)+Lx^4 (2 Lxy^2+2 Lxxy Ly+Lxx Lyy-Ly Lyyy)+Lx^2 Ly^2 (3 Lxx^2-8 Lxy^2+Lxxy Ly-4 Lxx Lyy+3 Lyy^2-Ly Lyyy)]
]


TJunctionMap[img_Image,s_:1]:=Image[TJunctionMap[ImageData[img],s]]


VectorFieldMap[img_,\[Sigma]_:1]:=Module[{Lx,Ly,im,res},
	{Lx,Ly}=DerivativeFilter[img,{{0,1},{1,0}},\[Sigma]];
	Chop[Transpose[{Reverse[Ly],-Reverse[Lx]},{3,2,1}]]
]	


VectorFieldMap[img_Image,s_:1]:=Image[VectorFieldMap[ImageData[img],s]]


ImagePartials[img_,\[Sigma]_:1]:=Module[{dix,diy,dixx,diyy,dixy},
	dix=DerivativeFilter[img,{1,0},\[Sigma]];
	diy=DerivativeFilter[img,{0,1},\[Sigma]];
	dixx=DerivativeFilter[img,{2,0},\[Sigma]];
	diyy=DerivativeFilter[img,{0,2},\[Sigma]];
	dixy=DerivativeFilter[img,{1,1},\[Sigma]];
	{dix,diy,dixx,diyy,dixy}]


ImagePartials[img_Image,s_:1]:=Image[ImagePartials[ImageData[img],s]]


StructureTensor[img_,s_:1]:=Module[{dix,diy,j11,j22,j12},
	dix=DerivativeFilter[img,{1,0}];
	diy=DerivativeFilter[img,{0,1}];
	j11=dix*dix;
	j22=diy*diy;
	j12=diy*dix;
	{j11,j22,j12}]


StructureTensor[img_Image,s_:1]:=ColorCombine[structureTensor[ImageData[img],s]]


OrientationMap[img_,s_:1]:=Module[{j11,j22,j12},
	{j11,j22,j12} = StructureTensor[img,s];
	ArcTan[j22-j11, 2 j12]/2.0]


OrientationMap[img_Image,s_:1]:=Image[OrientationMap[ImageData[img],3]]


NormalMap[img_,\[Sigma]_:1]:=Module[{dix,diy},
	dix=DerivativeFilter[img,{1,0},\[Sigma]];
	diy=DerivativeFilter[img,{0,1},\[Sigma]];
	Chop[MapThread[{-#1,-#2,1}/Sqrt[1+#1^2+#2^2]&,{dix,diy},2]]]


NormalMap[img_Image,\[Sigma]_:1]:=Image[NormalMap[ImageData[img],\[Sigma]]]


FlowMap[img_,\[Sigma]_:1]:=Module[{dix,diy},
	dix=DerivativeFilter[img,{1,0},\[Sigma]];
	diy=DerivativeFilter[img,{0,1},\[Sigma]];
	Chop[MapThread[sarcTan[#1,#2]&,{dix,diy},2]]]


FlowMap[img_Image,\[Sigma]_:1]:=Image[FlowMap[ImageData[img],\[Sigma]]]


FormsMap[img_,\[Sigma]_:1]:=Module[{dix,diy,dixx,diyy,dixy,Emap,Fmap,Gmap,emap,fmap,gmap},
	{dix,diy,dixx,diyy,dixy} = ImagePartials[img,\[Sigma]];
	
	Emap=Map[1+#^2&,dix,{2}];
	Fmap=MapThread[#1 #2&,{dix,diy},2];
	Gmap=Map[1+#^2&,diy,{2}];
	
	emap=MapThread[#3/Sqrt[1+#1^2+#2^2]&,{dix,diy,dixx},2];
	fmap=MapThread[#3/Sqrt[1+#1^2+#2^2]&,{dix,diy,dixy},2];
	gmap=MapThread[#3/Sqrt[1+#1^2+#2^2]&,{dix,diy,diyy},2];
	
	{emap,fmap,gmap,Emap,Fmap,Gmap}]


FormsMap[img_Image,\[Sigma]_:1]:=Image[FormsMap[ImageData[img],\[Sigma]]]


CurvaturesMap[img_,\[Sigma]_:1]:=Module[{Emap,Fmap,Gmap,emap,fmap,gmap,kk1,kk2},
	{emap,fmap,gmap,Emap,Fmap,Gmap} = FormsMap[img,\[Sigma]];
	
	kk1=(-#3 #4+2 #2 #5-#1 #6+Sqrt[(#3 #4-2 #2 #5+#1 #6)^2-4 (#2^2-#1 #3) (#5^2-#4 #6)])/(2 (#5^2-#4 #6));
	kk2=-((#3 #4-2 #2 #5+#1 #6+Sqrt[(#3 #4-2 #2 #5+#1 #6)^2-4 (#2^2-#1 #3) (#5^2-#4 #6)])/(2 (#5^2-#4 #6)));

	Chop[MapThread[Evaluate[{kk1,kk2}]&,{emap,fmap,gmap,Emap,Fmap,Gmap},2]]
]


CurvaturesMap[img_Image,\[Sigma]_:1]:=Image[CurvaturesMap[ImageData[img],\[Sigma]]]

(* Utilities *)
MapToImage[map_,mask_:None]:=ImageMultiply[Image[map],If[mask===None,1,mask]]
