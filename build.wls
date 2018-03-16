#!/usr/bin/env wolframscript
#
# paclet builder
#

packageName = "Shape";
directory = Directory[];
pacletDirectory = FileNameJoin[{directory, packageName}];

Print["Building from: ", directory];

(* bump build *)
FPVersionBumpBuild[directory];

(* gerbils *)
$packageName="Shape";
$versionFile="version.json";
$globalsFile="Globals.wl";

$ShapeDebug=False;
$ShapeVersion=FPVersionString[directory,"Build"->True];

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
UpdatePacletFile[pacletDirectory,directory];
paclet = PackPaclet[pacletDirectory];
Print["New paclet: ", paclet];

paclets = PacletFind[packageName];
Print["Found the following old paclets (uninstalling): ", Map[ #["Location"]&, paclets]];
Map[ PacletUninstall, paclets ];


(* install new paclet *)
Print["Installing new paclet"]
PacletInstall[paclet, IgnoreVersion -> True];

Print["Done"]
