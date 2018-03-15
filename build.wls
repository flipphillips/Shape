#!/usr/bin/env wolframscript


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

(* global file *)
gfile=FileNameJoin[{directory,$packageName,$globalsFile}];
If[FileExistsQ[gfile], DeleteFile[gfile]];

Save[gfile, {$ShapeDebug, $ShapeVersion}];


pkgDir = FileNameJoin[{$UserBaseDirectory, "Applications", $packageName}];
Print["Package directory: ", pkgDir];

Print["Deleting old..."]
If[FileNames[pkgDir]!={},DeleteDirectory[pkgDir,DeleteContents->True]];

Print["Copying..."]
CopyDirectory[$packageName,pkgDir];
CopyFile[$versionFile,FileNameJoin[{pkgDir,$versionFile}]];

Print["Done"]
