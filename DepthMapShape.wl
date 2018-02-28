(* ::Package:: *)

BeginPackage["Shape`DepthMapShape`"]


(* Exported symbols added here with SymbolName::usage *) 


imagePartials::usage = 
  "imagePartials[img,sigma] returns {dix,diy,dixx,diyy,dixy}";

structureTensor::usage = "structureTensor[img,sigma] computes 
	an image containing the structure tensor with {J11,J22,J12} in {R,G,B}";
orientationMap::usage = "orientationMap[img,sigma] returns an orientation map";

ridgeMap::usage = "ridgeMap[img,sigma] computes a ridge filter on img";
tJunctionMap::usage = 
  "tJunctionMap[img,sigma] computes a t-junction filter on img";

vectorFieldMap::usage = 
  "vectorFieldMap[img,sigma] computes vector flow field on img.";
  
normalMap::usage = 
  "normalMap[img,sigma] gives an RGB normal map from img.";

flowMap::usage = 
  "flowMap[img,sigma] gives a flow direction map from img.";
formsMap::usage = 
  "formsMap[img,sigma] returns {e,f,g,E,F,G} map from img.";

curvaturesMap::usage = 
  "curvaturesMap[img,sigma] returns kmin,kmax from image.";

mapToImage::usage = 
  "mapToImage[map,mask] turns a map into an image with optional mask";


Begin["`Private`"]


(* private functions, safearctan *)


sarcTan[x_,y_]:=0.0/;x==0.0
sarcTan[x_,y_]:=ArcTan[x,y]


(* Implementation of the package *)


ridgeMap[img_,\[Sigma]_:1] := Module[{Lxx,Lxy,Lyy},
	{Lxx,Lxy,Lyy}=DerivativeFilter[img,{{0,2},{1,1},{2,0}},\[Sigma]];
	Chop[\[Sigma]^(3/2)/2 (Sqrt[(Lxx - Lyy)^2+ 4 Lxy^2]-Lxx-Lyy)]
]


ridgeMap[img_Image,s_:1]:=Image[ridgeMap[ImageData[img],s]]


tJunctionMap[img_,\[Sigma]_:1] := 
Module[{Lx,Ly,Lxx,Lxy,Lyy,Lxxx,Lxxy,Lxyy,Lyyy},
	{Lx,Ly}=DerivativeFilter[img,{{0,1},{1,0}},\[Sigma]];
	{Lxx,Lxy,Lyy}=DerivativeFilter[img,{{0,2},{1,1},{2,0}},\[Sigma]];
	{Lxxx,Lxxy,Lxyy,Lyyy}=DerivativeFilter[img,{{0,3},{1,2},{2,1},{3,0}},\[Sigma]];
	Chop[-Lx^5 Lxyy+Ly^4 (2 Lxy^2-Lxxy Ly+Lxx Lyy)+Lx Ly^3 (6 Lxx Lxy-Lxxx Ly+2 Lxyy Ly-6 Lxy Lyy)+Lx^3 Ly (-6 Lxx Lxy-Lxxx Ly+Lxyy Ly+6 Lxy Lyy)+Lx^4 (2 Lxy^2+2 Lxxy Ly+Lxx Lyy-Ly Lyyy)+Lx^2 Ly^2 (3 Lxx^2-8 Lxy^2+Lxxy Ly-4 Lxx Lyy+3 Lyy^2-Ly Lyyy)]
]


tJunctionMap[img_Image,s_:1]:=Image[tJunctionMap[ImageData[img],s]]


vectorFieldMap[img_,\[Sigma]_:1]:=Module[{Lx,Ly,im,res},
	{Lx,Ly}=DerivativeFilter[img,{{0,1},{1,0}},\[Sigma]];
	Chop[Transpose[{Reverse[Ly],-Reverse[Lx]},{3,2,1}]]
]	


vectorFieldMap[img_Image,s_:1]:=Image[vectorFieldMap[ImageData[img],s]]


imagePartials[img_,\[Sigma]_:1]:=Module[{dix,diy,dixx,diyy,dixy},
	dix=DerivativeFilter[img,{1,0},\[Sigma]];
	diy=DerivativeFilter[img,{0,1},\[Sigma]];
	dixx=DerivativeFilter[img,{2,0},\[Sigma]];
	diyy=DerivativeFilter[img,{0,2},\[Sigma]];
	dixy=DerivativeFilter[img,{1,1},\[Sigma]];
	{dix,diy,dixx,diyy,dixy}]


imagePartials[img_Image,s_:1]:=Image[imagePartials[ImageData[img],s]]


structureTensor[img_,s_:1]:=Module[{dix,diy,j11,j22,j12},
	dix=DerivativeFilter[img,{1,0}];
	diy=DerivativeFilter[img,{0,1}];
	j11=dix*dix;
	j22=diy*diy;
	j12=diy*dix;
	{j11,j22,j12}]


structureTensor[img_Image,s_:1]:=ColorCombine[structureTensor[ImageData[img],s]]


orientationMap[img_,s_:1]:=Module[{j11,j22,j12},
	{j11,j22,j12} = structureTensor[img,s];
	ArcTan[j22-j11, 2 j12]/2.0]


orientationMap[img_Image,s_:1]:=Image[orientationMap[ImageData[img],3]]


normalMap[img_,\[Sigma]_:1]:=Module[{dix,diy},
	dix=DerivativeFilter[img,{1,0},\[Sigma]];
	diy=DerivativeFilter[img,{0,1},\[Sigma]];
	Chop[MapThread[{-#1,-#2,1}/Sqrt[1+#1^2+#2^2]&,{dix,diy},2]]]


normalMap[img_Image,\[Sigma]_:1]:=Image[normalMap[ImageData[img],\[Sigma]]]


flowMap[img_,\[Sigma]_:1]:=Module[{dix,diy},
	dix=DerivativeFilter[img,{1,0},\[Sigma]];
	diy=DerivativeFilter[img,{0,1},\[Sigma]];
	Chop[MapThread[sarcTan[#1,#2]&,{dix,diy},2]]]


flowMap[img_Image,\[Sigma]_:1]:=Image[flowMap[ImageData[img],\[Sigma]]]


formsMap[img_,\[Sigma]_:1]:=Module[{dix,diy,dixx,diyy,dixy,Emap,Fmap,Gmap,emap,fmap,gmap},
	{dix,diy,dixx,diyy,dixy} = imagePartials[img,\[Sigma]];
	
	Emap=Map[1+#^2&,dix,{2}];
	Fmap=MapThread[#1 #2&,{dix,diy},2];
	Gmap=Map[1+#^2&,diy,{2}];
	
	emap=MapThread[#3/Sqrt[1+#1^2+#2^2]&,{dix,diy,dixx},2];
	fmap=MapThread[#3/Sqrt[1+#1^2+#2^2]&,{dix,diy,dixy},2];
	gmap=MapThread[#3/Sqrt[1+#1^2+#2^2]&,{dix,diy,diyy},2];
	
	{emap,fmap,gmap,Emap,Fmap,Gmap}]


formsMap[img_Image,\[Sigma]_:1]:=Image[formsMap[ImageData[img],\[Sigma]]]


curvaturesMap[img_,\[Sigma]_:1]:=Module[{Emap,Fmap,Gmap,emap,fmap,gmap,kk1,kk2},
	{emap,fmap,gmap,Emap,Fmap,Gmap} = formsMap[img,\[Sigma]];
	
	kk1=(-#3 #4+2 #2 #5-#1 #6+Sqrt[(#3 #4-2 #2 #5+#1 #6)^2-4 (#2^2-#1 #3) (#5^2-#4 #6)])/(2 (#5^2-#4 #6));
	kk2=-((#3 #4-2 #2 #5+#1 #6+Sqrt[(#3 #4-2 #2 #5+#1 #6)^2-4 (#2^2-#1 #3) (#5^2-#4 #6)])/(2 (#5^2-#4 #6)));

	Chop[MapThread[Evaluate[{kk1,kk2}]&,{emap,fmap,gmap,Emap,Fmap,Gmap},2]]
]


curvaturesMap[img_Image,\[Sigma]_:1]:=Image[curvaturesMap[ImageData[img],\[Sigma]]]


mapToImage[map_,mask_:None]:=ImageMultiply[Image[map],If[mask===None,1,mask]]


End[]


EndPackage[]
