(* ::Package:: *)

(* ::Section:: *)
(*DepthMaps*)


imagePartials::usage = "imagePartials[img,sigma] returns {dix,diy,dixx,diyy,dixy}";
structureTensor::usage = "structureTensor[img,sigma] computes an image containing the structure tensor with {J11,J22,J12} in {R,G,B}";
orientationMap::usage = "orientationMap[img,sigma] returns an orientation map";
ridgeMap::usage = "ridgeMap[img,sigma] computes a ridge filter on img";
tJunctionMap::usage = "tJunctionMap[img,sigma] computes a t-junction filter on img";
vectorFieldMap::usage = "vectorFieldMap[img,sigma] computes vector flow field on img.";
normalMap::usage = "normalMap[img,sigma] gives an RGB normal map from img.";
flowMap::usage = "flowMap[img,sigma] gives a flow direction map from img.";
formsMap::usage = "formsMap[img,sigma] returns {e,f,g,E,F,G} map from img.";
curvaturesMap::usage = "curvaturesMap[img,sigma] returns kmin,kmax from image.";
mapToImage::usage = "mapToImage[map,mask] turns a map into an image with optional mask";


(* ::Section:: *)
(*GeometryTools*)


ToGraphicsComplex::usage = "ToGraphicsComplex[vfn] or ToGraphicsComplex[vertices,faces,normals] creates a GraphicsComplex from a vfn representation.";
ImportVFN::usage = "ImportVFN[file] tries to import an object into a vfn representation.";
ToVFN::usage = "ToVFN[obj] or ToVFN[v,f] or ToVFN[v,f,n] converst from Graphics3D or other discrete representations to a vfn.";


ToNormalLines::usage = "ToNormalLines[vertices,normals] makes a set of lines representing normals.";
ToFrameLines::usage = "ToFrameLines[vertices,normals,principaldirections] makes a graphic representation of the frames at each vertex.";


Vertices::usage = "Vertices[obj] returns the vertices from a Graphics3D object.";
Faces::usage = "Faces[obj] returns the face connectivity from a Graphics3D object.";
Normals::usage = "Normals[obj] returns the vertex normals from a Graphics3D object.";


ReduceVertices::usage = "ReduceVertices[vertices,faces] tries to eliminate redundant vertices.";
Centroid::usage = "Centroid[vertices] or Centroid[vfn] computes the 3D centroid of the vertices or VFN object.";
CenterVertices::usage = "CenterVertices[vertices] or CenterVertices[vfn] puts the vertex list about {0,0,0}.";
BoundingBox::usage = "BoundingBox[vertices] or BoundingBox[vfn] returns the {{xmin,xmax},...} bounding box for the vertex list or VFN object.";


VFNInformation::usage = "VFNInformation[vfn] returns a Dataset with information about the vfn.";


DebugNormals::usage = "DebugNormals[vertices,faces,normals] draws the normals.";
DebugFrames::usage = "DebugFrames[vfn], DebugFrames[vertices,faces,normals,density,len] draws a small subset of frames from the VFN.";


TessSphere::usage = "TessSphere[] or TessSphere[depth] or TessSphere[depth,baseObj] does a recursive geodesation of some base polyhedron.";
JitterySphere::usage = "JitterySphere[noise] generates a noisy sphere. Same pther parameters as TessShpere[].";


(* ::Section:: *)
(*ShapeMetrics*)


(* ShapeMetrics *)
(* ShapeMetrics::usage="ShapeMetrics is a package containing a few curvature related functions to mathematica" *)
(* compute curvatures from mean/gaussian cvt *)
ToPrincipalCurvatures::usage="ToPrincipalCurvatures[H,K] gives kappa1 and kappa2 from mean and Gaussian curvatures. k1 <= k2";
(* derivative based*)
MeanCurvatureFromGradients::usage="MeanCurvatureFromGradients[dx,dy,dxx,dyy,dxy] gives H from 1st and 2nd derivates.";
GaussianCurvatureFromGradients::usage="GaussianCurvatureFromGradientsComputes[dx,dy,dxx,dyy,dxy] K from 1st and 2nd derivates.";
(* traditional, from curvatures *)
GaussianCurvature::usage="GaussianCurvature[k1,k2] Gaussian curvature from principal curvatures k1,k2. Positive are synclastic, negative are anticlastic.";
MeanCurvature::usage="MeanCurvature[k1,k2] Mean curvature of k1,k2.";
(* koenderink's variants *)
ShapeIndex::usage="ShapeIndex[k1,k2] Koenderink's Shape Index";
Curvedness::usage="Curvedness[k1,k2] Koenderink's Curvedness";
(* some other variants we developed *)
CurvatureContrast::usage="CurvatureContrast[k1,k2] Phillips/Perotti's contrast measure.";
MichelsonContrast::usage="MichelsonContrast[k1,k2] Michaelson contrast measure.";
NormalizedCurvatureContrast::usage="NormalizedCurvatureContrast[k1,k2] Normalized Phillips/Perotti contrast measure.";
TotalCurvature::usage="TotalCurvature[k1,k2] Absolute total curvature.";
MaximumCurvature::usage="MaximumCurvature[k1,k2] Absolute maximum curvature";
MaximumSignedCurvature::usage="MaximumSignedCurvature[k1,k2] Absolute maximum curvature with sign.";
(* utility *)
ShapeIndexColor::usage="ShapeIndexColor[si] Color coding on [-1,1] ala Koenderink. Green to red.";
ShapeIndexCategory::usage="ShapeIndexCategory[si] Category coding on [-1,1] ala Koenderink. 1-9.";


(* ::Section:: *)
(*ShapePatches*)


(* ShapePatches *)
(* Patches::usage="Patches is a set of routines to create parametrically defined patches from curvature information"; *)
curvaturesPatch::usage = "curvaturesPatch[u,v,k1,k2] is a parametric Monge-form patch from curvatures.";
shapeCurvednessPatch::usage = "shapeCurvednessPatch[u,v,s,c] creates a parametric Monge-form patch from Shape Index and Curvedness.";
meanGaussPatch::usage = "meanGaussPatch[u,v,h,k] creates a parametric Monge-form patch from mean and Gaussian curvatures.";


(* ::Section:: *)
(*Structure*)


(* Structure *)
VertexFrame::usage="VertexFrame[v,n,neighborhood] calculates the vertex frame for vertex v with normal n and neighbors.";
OBJStructure::usage="OBJStructure[filename] calculates the differential structure of the OBJ file. This file should have vertex normals.";


(* ::Section:: *)
(*Volumes*)


(* Volume *)
RegionToVolume::usage="RegionToVolume[r] converts regions into volumes";
PointCloudToVolume::usage="PointCloudToVolume[p] converts a list of points into a volume";
