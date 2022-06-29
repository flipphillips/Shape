# Shape

v3.0.0

![](icon.png)

# Shape
Shape analysis and classification routines for Mathematica

© 1997-2017 Flip Phillips, All Rights Reserved.

See [`CHANGELOG.md`](CHANGELOG.md) for details.

> A package for computing differential geometry using _Mathematica_.

> Based on a bunch of work I've done since about 1995 or so. Bits and pieces borrowed from Koenderink, Gray, &c.

## Overview

Right now, `Shape` is broken into five sub-modules:

* DepthMapShape
* RegionShape
* ShapeMetrics
* ShapePatches
* Structure

### DepthMap

Contains routines for calculating depth-map based shape using structure tensor shenanigans. All `sigma` default to 1.0 pixels.


`imagePartials[img,sigma]` returns `{dix,diy,dixx,diyy,dixy}` using a filter size of sigma.

`structureTensor[img,sigma]` computes an image containing the structure tensor with `{J11,J22,J12}` in `{R,G,B}`

`orientationMap[img,sigma]` returns an `xyz` orientation map in `{R,G,B}`.

`ridgeMap[img,sigma]` computes a ridge filter on img using a filter size of sigma.

`tJunctionMap[img,sigma]` computes a t-junction filter on img using a filter size of sigma.

`vectorFieldMap[img,sigma]` computes vector flow field on img using a filter size of sigma.

`imagePartials[img,sigma]` returns `{dix,diy,dixx,diyy,dixy}` on img using a filter size of sigma.

`normalMap[img,sigma]` gives an RGB normal map from img using a filter size of sigma.

`flowMap[img,sigma]` gives a flow direction map from img using a filter size of sigma.

`formsMap[img,sigma]` returns `{e,f,g,E,F,G}` map from img using filter size sigma.

`curvaturesMap[img,sigma]`returns a 2-channel kmin, kmax map from img using filter size sigma.

`mapToImage[map,mask]` turns a map into an image with optional mask.

### Shape Metrics

A whole barrel of shape reparameterizations of $k_min,k_max$

`ToPrincipalCurvatures[H,K]` gives kappa1 and kappa2 from mean and Gaussian curvatures. k1 <= k2

`MeanCurvatureFromGradients[dx,dy,dxx,dyy,dxy]` gives H from 1st and 2nd derivatives.

`GaussianCurvatureFromGradientsComputes[dx,dy,dxx,dyy,dxy]` K from 1st and 2nd derivatives.

`GaussianCurvature[k1,k2]` Gaussian curvature from principal curvatures k1,k2. Positive are synclastic, negative are anticlastic.

`MeanCurvature[k1,k2]` Mean curvature of k1,k2.

`ShapeIndex[k1,k2]` Koenderink's Shape Index.

`Curvedness[k1,k2]` Koenderink's Curvedness.

`CurvatureContrast[k1,k2]` Phillips/Perotti's contrast measure.

`MichelsonContrast[k1,k2]` Michaelson contrast measure.

`NormalizedCurvatureContrast[k1,k2]` Normalized Phillips/Perotti contrast measure.

`TotalCurvature[k1,k2]` Absolute total curvature.

`MaximumCurvature[k1,k2]` Absolute maximum curvature.

`MaximumSignedCurvature[k1,k2]` Absolute maximum curvature with sign.

`ShapeIndexColor[si]` Color coding on [-1,1] ala Koenderink. Green to red.

`ShapeIndexCategory[si]` Category coding on [-1,1] ala Koenderink. 1-9.

### Shape Patches

Generate simple parameterized shapes constrained by curvatures.

`curvaturesPatch[u,v,k1,k2]` is a parametric Monge-form patch from curvatures.

`shapeCurvednessPatch[u,v,s,c]` creates a parametric Monge-form patch from Shape Index and Curvedness.

`meanGaussPatch[u,v,h,k]` creates a parametric Monge-form patch from mean and Gaussian curvatures.

### Structure

A start at stuff that derives structural information for objects. They make some assumptions about the representation. This is slowly being migrated from the 1.x `Shape` stuff.

`VertexFrame[v,n,neighborhood]` calculates the vertex frame for vertex v with normal n and neighbors.

`OBJStructure[filename]` calculates the differential structure of the OBJ file. This file should have vertex normals.
 

### RegionShape

Will contain methods for applying shape calculations to `Region`
