#!/usr/bin/env wolframscript

$packageName="Shape"
$versionFile="version.json"

(* this is buggy and doesn't do what I'd expect *)
(* directory = DirectoryName[$InputFileName]; *)

directory = Directory[];
FPVersionBumpBuild[directory];

Print["Building from directory: ", directory];

pkgDir = FileNameJoin[{$UserBaseDirectory, "Applications", $packageName}];
Print["Package directory: ", pkgDir];

Print["Deleting old..."]
If[FileNames[pkgDir]!={},DeleteDirectory[pkgDir,DeleteContents->True]];

Print["Copying..."]
CopyDirectory[$packageName,pkgDir];
CopyFile[$versionFile,FileNameJoin[{pkgDir,$versionFile}]];

Print["Done"]
