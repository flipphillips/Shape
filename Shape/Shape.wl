(* ::Package:: *)

(* As borrowed from Arnoud's templates *)


BeginPackage["Shape`"];

directory = Directory[];

Get[FileNameJoin[{directory,"Globals.wl"]];
Get[FileNameJoin[{directory,"Usage.wl"}]];

If[$ShapeDebug,PrintTemporary["Loading ",ToString[Length[Names["Shape`*"]]], " Shape package functions"]];

Begin["`Private`"];

Module[{files},

  (* load in any session *)
  files={"DepthMaps.wl","GeometryTools.wl",
         "RegionShape.wl","ShapeMetrics.wl",
         "ShapePatches.wl","Structure.wl","Volumes.wl"};

  Map[Get[FileNameJoin[{directory, #}]] &, files];

  (* only load in a notebook session *)
(*
  If[ Head[$FrontEnd] === FrontEndObject,
    files = {"Dock.wl","Notebook.wl"};
    Map[ Get[ FileNameJoin[{directory, #}] ] &, files ];
  ]
*)

];

End[];
EndPackage[];
