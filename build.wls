#!/usr/bin/env wolframscript
#
# paclet builder
#

directory = Directory[];
Print["Building from directory: ", directory];

(* bump build *)
FPVersionBumpBuild[directory];

(* gerbils *)
$packageName="Shape";
$versionFile="version.json";
$globalsFile="Globals.wl";

$ShapeDebug=False;
$ShapeVersion=FPVersionString[directory];
(* $ShapeBuild=FPVersionInfo[directory]["build_number"]; *)

(* create global file *)
gfile=FileNameJoin[{directory,$packageName,$globalsFile}];
If[FileExistsQ[gfile], DeleteFile[gfile]];

Save[gfile, {$ShapeDebug, $ShapeVersion}];

(* delete old paclet *)
files = FileNames["*.paclet", directory];
Print["Found paclets (to be deleted): ", files];
DeleteFile /@ files;

(* build new paclet *)
Print["Building new paclet"];
paclet = PackPaclet[FileNameJoin[{directory, "Shape"}]];
Print["New paclet: ", paclet];

(* install new paclet *)
Print["Installing new paclet"]
PacletInstall[paclet, IgnoreVersion -> True];

Print["Done"]
