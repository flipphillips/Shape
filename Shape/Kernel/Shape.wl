(* ::Package:: *)

BeginPackage["Flip`Shape`"];


(* ::Subsection:: *)
(*Usages*)


(* ::Subsubsection:: *)
(*Conversion*)


PrincipalCurvaturesFromHK::usage="PrincipalCurvaturesFromHK[H,K] gives k1 and k2 from Mean and Gaussian curvatures."
PrincipalCurvaturesFromSC::usage="PrincipalCurvaturesFromSC[S,C] gives k1 and k2 from Koenderink's Shape Index and Curvedness."


MeanCurvatureFromGradients::usage="MeanCurvatureFromGradients[dx,dy,dxx,dyy,dxy] gives H from 1st and 2nd derivatives."
GaussianCurvatureFromGradients::usage="GaussianCurvatureFromGradientsComputes[dx,dy,dxx,dyy,dxy] K from 1st and 2nd derivates."


(* ::Subsubsection:: *)
(*Metrics - Traditional*)


GaussianCurvature::usage="GaussianCurvature[k1,k2] Gaussian curvature from principal curvatures k1,k2."
MeanCurvature::usage="MeanCurvature[k1,k2] Mean curvature of k1,k2."


(* ::Subsubsection:: *)
(*Metrics - Koenderink*)


ShapeIndex::usage = "ShapeIndex[k1,k2] Koenderink's Shape Index metric."
Curvedness::usage = "Curvedness[k1,k2] Koenderink's Curvedness metric."


(* ::Subsubsection:: *)
(*Visualization tools - Koenderink*)


ShapeIndexColor::usage="ShapeIndexColor[si] Color coding on [-1,1] ala Koenderink."
ShapeIndexCategory::usage="ShapeIndexCategory[si] Category coding on [-1,1] ala Koenderink."


(* ::Subsubsection:: *)
(*Metrics - Mine/Others*)


CurvatureContrast::usage="CurvatureContrast[k1,k2] Phillips/Perotti's contrast measure."
MichelsonContrast::usage="MichelsonContrast[k1,k2] Michaelson contrast measure."
NormalizedCurvatureContrast::usage="NormalizedCurvatureContrast[k1,k2] Normalized Phillips/Perotti contrast measure."
TotalCurvature::usage="TotalCurvature[k1,k2] Absolute total curvature."
MaximumCurvature::usage="MaximumCurvature[k1,k2] Absolute maximum curvature."
MaximumSignedCurvature::usage="MaximumSignedCurvature[k1,k2] Absolute maximum curvature with sign."


(* ::Subsubsection:: *)
(*Generators*)


CurvaturesPatch::usage="CurvaturesPatch[k1,k2][u,v] gives a function of principal curvatures in u,v that generate z."


ShapeCurvednessPatch::usage="ShapeCurvednessPatch[s,c][u,v] gives a function of Koenderink Shape and Curvedness in u,v that generate z."


MeanGaussianPatch::usage="MeanGaussianPatch[h,k][u,v] gives a function of u,v from mean and Gaussian curvatures that generates z."


(* ::Subsubsection:: *)
(*Maps*)


ImagePartials::usage="ImagePartials[img]"
RidgeMap::usage="RidgeMap[img] returns a map of differential ridges in img."
TJunctionMap::usage="TJunctionMap[img]"
VectorFieldMap::usage="VectorFieldMap[img]"
StructureTensor::usage="StructureTensor[img]"
OrientationMap::usage="OrientationMap[img]"
NormalMap::usage="NormalMap[img]"
FlowMap::usage="FlowMap[img]"
FormsMap::usage="FormsMap[img]"
CurvaturesMap::usage="CurvaturesMap[img]"
MapToImage::usage="MapToImage[map]"


(* ::Subsection:: *)
(*Whereami*)


Begin["`Private`"];


directory=DirectoryName[$InputFileName];


files = {"ShapeMetrics.wl", "ShapePatches.wl","Maps.wl"}; 
Map[Get[FileNameJoin[{directory, #}]]&, files];


End[];


EndPackage[];
