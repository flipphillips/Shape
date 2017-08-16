(* ::Package:: *)
BeginPackage["Shape`"]
EndPackage[]

(* these need fix'n *)
DeclarePackage["Shape`DepthMapShape`", {
    "ridgeMap",
    "tJunctionMap",
    "vectorFieldMap",
    "imagePartials",
    "normalMap",
    "flowMap",
    "formsMap",
    "curvaturesMap",
    "mapToImage"
}]

DeclarePackage["Shape`RegionShape`", {
    "Bleh"
}]

DeclarePackage["Shape`ShapeMetrics`", {
    "ToPrincipalCurvatures",
    "MeanCurvatureFromGradients",
    "GaussianCurvatureFromGradients",

    "GaussianCurvature",
    "MeanCurvature",

    "ShapeIndex",
    "Curvedness",

    "CurvatureContrast",
    "MichelsonContrast",
    "NormalizedCurvatureContrast",
    "TotalCurvature",
    "MaximumCurvature",
    "MaximumSignedCurvature",

    "ShapeIndexColor",
    "ShapeIndexCategory"
}]

DeclarePackage["Shape`ShapePatches`", {
    "curvaturesPatch",
    "shapeCurvednessPatch",
    "meanGaussPatch"
}]

DeclarePackage["Shape`Structure`", {
    "VertexFrame",
    "OBJStructure"
}]

Null
