#!/usr/bin/env wolframscript

(*
mogrify -crop 450x450+75+0 -path . pov_*.png
mogrify -crop 300x300+50+0 -path . explode*.gif

magick convert -duplicate 1,-2-1 -quiet +repage -alpha remove -loop 0 -delay 5 -dispose 2 *.png out.gif
magick convert out.gif -coalesce -duplicate 1,-2-1 -quiet -layers OptimizePlus  -loop 0 cycle.gif
python -m SimpleHTTPServer 8000

*)

(* global config *)

(* frameCount = 2; *)
(* frameCount = 5; *)
(* frameCount = 10; *)
frameCount = 100;
(* frameCount = 60; *)

(* topView = False; *)
topView = True;

singleTile = False;
(* singleTile = True; *)

(* fullRotation = False; *)
fullRotation = True;

(* shape = "chiral_tiling"; *)
shape = "nonchiral_tiling";

meshMethod = "";
(* meshMethod = "design1"; *)
(* meshMethod = "design2"; *)
(* meshMethod = "noFaces"; *)

rotationDirection = "";
(* rotationDirection = "3rdDim"; *)

(* default is based on cell *)
selectionMethod = "2face";
(* selectionMethod = "3face"; *)
(* selectionMethod = "4face"; *)

intersectionIncludeBoundary = False;
(* intersectionIncludeBoundary = True; *)

exportToPov = True;
dataFolder = "data";
imageFolder = "output";
imageSize = 720/3 * {4, 3};

truncateThreshold = 2;

range = 10;
(* If[singleTile, 3, If[shape == "spectre", 10, 8]]; (* change to 10 *) *)
thickness = 0.02;

shapeWithParams = shape <> If[topView, "", "_sideView"] <> If[fullRotation, "", "_quarterTurn"] <> If[singleTile, "_singleTile", ""] <> If[meshMethod == "", "", "_" <> meshMethod] <> If[rotationDirection == "", "", "_" <> rotationDirection] <> If[selectionMethod == "", "", "_" <> selectionMethod] <> "_" <> IntegerString[truncateThreshold];

rangeFactor = 3.0;
originalProjectionMatrixH4 = {
    {0.00, 0, 2 Sin[2 Pi/15], (1 + Sqrt[5]) Sin[Pi/15]},
    {1, -(1 + Sqrt[5]) Sin[Pi/30], 0, 0.00},
    {1, 3, 4, 1},
    {0, 0, 0, 1}
} * 1.0;

originalProjectionMatrixF4 = {
	{1, 1, Sqrt[3] - 1, 0},
	{1, -1, 0, Sqrt[3] - 1},
    {1, 3, 4, 1},
    {0, 0, 0, 1}
} * 1.0;

lightSource = {50, -50, 50};
lighting = {{"Point", White, lightSource * 10}};

lightingRotation = RotationMatrix[{{50, -35, 40}, {0, 0, 1.}}];

roundPrecision = 0.000001;
phi = (1 + Sqrt[5])/2;

Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

epsilon = 0.00000001;
ApproxSamePoint[point1_, point2_] := Round[point1, epsilon] == Round[point2, epsilon];

reflect[point_, mirror_] := FullSimplify[point - 2 * (point.mirror)/(mirror.mirror)*mirror];
doubleReflect[point_, mirror1_, mirror2_] := reflect[reflect[point, mirror1], mirror2];
getEdgesFromFace[face_] := Table[{face[[i+1]], face[[Mod[i+1, Length[face]] + 1]]}, {i, 0, Length[face] - 1}];
explodedFace[face_, explosionFactor_] := Map[(# + Mean[face] * explosionFactor)&, face];
sameVertex[point1_, point2_] := point1 == point2;
getFaceCenter[face_] := Simplify[Total[face]];
getCellCenter[cell_] := Mean[Map[Mean, cell]];
sameCellCenter[cell1_, cell2_] := ApproxSamePoint[getCellCenter[N[cell1]], getCellCenter[N[cell2]]];
explodedFace[face_, explosionFactor_] := Map[(# + Mean[face] * explosionFactor)&, face];
explodedCell[cell_, explosionFactor_] := Map[
	(# + Mean[Map[Mean,cell]]*(Norm[First[First[cell]]//N]/Norm[Mean[Map[Mean,cell]]//N])^1.5 * explosionFactor)&, 
	cell, {2}];
normalizedAngle[u_, v_] := ArcSin[u.v/Norm[u]/Norm[v]]/Pi + 0.5;
projectToDirection[vertex4d_, projectionMatrix_] := vertex4d.Transpose[projectionMatrix];
(* perspectiveProjection[point_, fullProjectionMatrix_, wDistance_ ] := projectToDirection[point, fullProjectionMatrix[[1;;3]]] / (1 + point.(fullProjectionMatrix[[4]])/wDistance); *)
perspectiveProjection[point_, fullProjectionMatrix_, wDistance_ ] := projectToDirection[point, fullProjectionMatrix[[1;;3]]];
normalizedInner[u_, v_] := u.v/Norm[u]/Norm[v];
doubleRotationMatrix[theta_] := ArrayFlatten[{{RotationMatrix[-theta], 0}, {0, RotationMatrix[-theta]}}];
getProjectionMatrix[direction_] := Simplify[Orthogonalize[{direction, {2, -1.4, 0.3, 0.9}, {0, 1, 2.3, 0.8}, {0.5, 0.3, 1, 0.1}}]][[2;;4]];
sameCenter[set1_, set2_] := ApproxSamePoint[Total[set1], Total[set2]];

mapHopfFibration[v4d_] := Module[
	{z0, z1},
	z0 = v4d[[1]] + I v4d[[2]];
	z1 = v4d[[3]] + I v4d[[4]];
	x1 = Re[(2 z0 Conjugate[z1])];
	x2 = Im[(2 z0 Conjugate[z1])];
	x3 = Re[z0 Conjugate[z0] - z1 Conjugate[z1]];
	Round[{x1, x2, x3}, roundPrecision]
];

getHue[v3d_] := If[Norm[v3d[[{1, 2}]]] < 0.001, 
						If[v3d[[3]] > 0, Darker[Blue, 0.8], Lighter[Red, 0.8]],
						(* If[v3d[[3]] > 0,
							Darker[Hue[Mod[Arg[v3d[[1]] + I v3d[[2]]]/2/Pi , 1]], 0.3],
							Lighter[Hue[Mod[Arg[v3d[[1]] + I v3d[[2]]]/2/Pi , 1]], 0.3]
						] *)
						(* *0.9 to avoid to similar colors of purple *)
						(* Hue[Mod[Arg[v3d[[1]] + I v3d[[2]]]/2/Pi , 1] * 0.9] *)
						Hue[Mod[Arg[v3d[[1]] + I v3d[[2]]]/2/Pi , 1]]
];


exportImage[cells_, theta_, sigma_, serialNumber_, frameFolder_] := Module[
	{
		dataFileName,
		shapeTokens,
		color,
		cellShape,
		cellsByLayers,
		layerKeys,
		outputFolder,
		outputFileName,
		cropCommand,
		inactiveCells,
		inactiveFaces,
		inactiveEdges,
		inactiveVertices,
		activeCells,
		activeFaces,
		touchingIndices,
		notTouchingIndices,
		touchingActiveFaces,
		notTouchingActiveFaces,
		allIndices,
		activeEdges,
		activeVertices,
		graph,
		faceTemp,faceNormalVector,faceCenter,projectedFaceCenter,faceVisible
	},

	outputFolder = FileNameJoin[{imageFolder, frameFolder}];
	If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

	outputFileName = shapeWithParams <> "_" <> IntegerString[serialNumber, 10, 3] <> ".png";
	outputFileName = FileNameJoin[{outputFolder, outputFileName}];

	cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

	projectionMatrix = Transpose[lightingRotation].projection43[theta, sigma];

	projectedCells = Map[projectionMatrix.#&, cells, {3}];

	faces3d = Flatten[projectedCells, 1];
	edges3d = Flatten[Map[getEdgesFromFace, faces3d], 1];
	(* uniqueFaces3d = Union[faces3d, SameTest -> sameCenter]; *)
	(* uniqueEdges3d = Union[edges3d, SameTest -> sameCenter]; *)
	(* Print[Map[Length, {faces3d, uniqueFaces3d, edges3d, uniqueEdges3d}]]; *)

	(* facesByPolygons = Table[Map[projectedPolygons[[k]][[#]]&, triangleIds ], {k, 1, Length[projectedPolygons]}]; *)

	If[meshMethod == "noFaces", faces3d = {}];
	edgeColor = If[meshMethod == "noFaces", Blue, White];
	faceColor = Blue;

	graphElements = {
		{
			FaceForm[edgeColor], Map[Cylinder[#, thickness]&, edges3d]
		},
		{
			FaceForm[edgeColor], Map[Sphere[#, 2 thickness]&, Flatten[edges3d, 1]]
		},
		Table[
			{
				FaceForm[faceColor ], Opacity[1], Polygon[faces3d[[k]] ]
			},
		{k, Length[faces3d]}]
	};

	graph = Graphics3D[
		graphElements,
		ViewPoint -> If[topView, {0, 0, 1}, {1.0, -1.0, 0.1}].lightingRotation * 1000,
		ViewVertical -> {0, 1, 0}.lightingRotation,
		PlotRange -> {{-range, range}, {-range, range}, {-range, range}},
		(* Lighting -> lighting, *)
		Boxed -> False
		(* ViewAngle -> range/Norm[viewPoint]/100/6.7 *)
	];

	If[exportToPov,
		If[FileExistsQ[outputFileName], DeleteFile[outputFileName]];
		POVRayRender[graph, Method -> "Triangle", OutputPath -> outputFileName, ImageSize -> imageSize, RenderOptions -> "+A0.001 -J +UA"];
		(* Run[cropCommand]; *)
		,
		Export[outputFileName, graph, ImageSize -> imageSize[[2]]];
	]
	Print["Saved the image to " <> outputFileName];
];

smoothFall[x_, start_, duration_] := If[x < start, 1, If[x > start + duration, 0, (1 + Cos[(x - start)/duration*Pi])/2]];
smoothFallFromTo[x_, start_, duration_, from_, to_] := smoothFall[x, start, duration] (from - to) + to;

exportAnimation[] := Module[
	{
		frameFolder,
		outputFolder,
		gifFileName,
		gifFolder,
		explosionConfig,
		maxExplosionFactor,
		explodeStep,
		explosionFactors,
		frameIndex,
		explosionFactor,
		gifCommand
	},

	frameFolder = shapeWithParams <> "_frames";
	outputFolder = FileNameJoin[{imageFolder, frameFolder}];
	If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

	gifFileName = shapeWithParams;
	gifFolder = FileNameJoin[{imageFolder, shape}];
	If[!DirectoryQ[gifFolder], CreateDirectory[gifFolder]];

	angles = Join[
		Table[smoothFallFromTo[x, 0, 1, 0, 30]//N, {x, 0, 1, 1/(frameCount - 1)}],
		Table[smoothFallFromTo[x, 0, 1, 30, 45]//N, {x, 0, 1, 1/(frameCount - 1)}],
		Table[smoothFallFromTo[x, 0, 1, 45, 60]//N, {x, 0, 1, 1/(frameCount - 1)}],
		Table[smoothFallFromTo[x, 0, 1, 60, 90]//N, {x, 0, 1, 1/(frameCount - 1)}]		
	];

	If[fullRotation,
		angles = Join[angles, 
			Table[smoothFallFromTo[x, 0, 1, 90, 180]//N, {x, 0, 1, 1/(2 * frameCount)}]
		];
		angles = Join[angles, angles + 180];
	];

	If[rotationDirection == "3rdDim",
		If[fullRotation,
			angles = Range[12 * frameCount]/12/frameCount * 360;
			, angles = Range[3 * frameCount]/3/frameCount * 90;
		];
	];
	(* Print[angles];	Exit[]; *)

	For[frameIndex = 1, frameIndex <= Length[angles], frameIndex++,
		(* Continue[]; *)
		(* If[frameIndex < 31, Continue[]]; *)
		(* If[Mod[frameIndex, 2] == 1 && frameIndex > 1, Continue[]]; *)
		(* If[frameIndex != 2, Continue[]]; *)
		angle = angles[[frameIndex]];
		Print["exporting frame " <> IntegerString[frameIndex] <> " / " <> IntegerString[Length[angles]] <> " angle: " <> ToString[angle] ];
		If[rotationDirection == "3rdDim",
			exportImage[polygons, Pi/4, angle/180.0 * Pi, frameIndex, frameFolder]
			, exportImage[polygons, angle/180.0 * Pi, Pi/4, frameIndex, frameFolder]
		];
		(* Exit[]; *)
	];

	(* Exit[]; *)
    delay = If[frameCount > 10, 3, 50];

    (* gifCommand = "magick convert -duplicate 1,-2-1 -quiet +repage -alpha remove -loop 0 -delay "   *)
    (* loop back*)
    gifCommand = "magick convert -quiet +repage -alpha remove -loop 0 -delay " 
    (* no looping back *)
		<> IntegerString[delay] <> " -dispose 2 " <> outputFolder <> "\\*animation*.png " <> gifFolder <> "\\" <> gifFileName <> ".gif";

	Print[gifCommand];
	Run[gifCommand];
	Print["Saved the animation to " <> gifFolder <> "\\" <> gifFileName <> ".gif"];

    mp4Command = "ffmpeg -i " <> gifFolder <> "\\" <> gifFileName <> ".gif" 
    	<> " -movflags faststart -pix_fmt yuv420p -vf \"scale=trunc(iw/2)*2:trunc(ih/2)*2\" "
    	<> gifFolder <> "\\" <> gifFileName <> ".mp4";

    mp4FileName = gifFolder <> "\\" <> gifFileName <> ".mp4";
    If[FileExistsQ[mp4FileName], DeleteFile[mp4FileName]];
    Print[mp4Command];
    Run[mp4Command];
    Print["Saved the video to " <> gifFolder <> "\\" <> gifFileName <> ".mp4"];
];

move[cells_, offset_] := Map[# + offset &, cells, {1}];
regionDisjointByArea[region1_, region2_] := Abs[ Area[region1] + Area[region2] - Area[RegionUnion[region1, region2]] ] < epsilon;
getPoints[region_] := Apply[List, RegionBoundary[region]][[1]];
getEdgesFromPoints[face_] := Table[{face[[i]], face[[i + 1]]}, {i, 1, Length[face] - 1}];
getEdgesFromRegion[region_] := getEdgesFromPoints[getPoints[region]];

projection42 = Transpose[{{1, 0, 1, 0}, {0, 1, 0, 1}}];

directions4d = {};
For[clock = 1, clock <= 12, clock++,
	vector2d = 2 {Sin[2 Pi / 12 * clock], Cos[2 Pi / 12 * clock]};
	vector4d = If[Mod[clock, 2] == 0, 
		Join[vector2d, {0, 0}],
		Join[{0, 0}, vector2d]
	];
	AppendTo[directions4d, vector4d];
];

directions2d = directions4d.projection42;

direction4dMap = <||>;
direction2dMap = <||>;
For[did = 1, did <= Length[directions4d], did++,
	direction4dMap[directions4d[[did]]] = did;
	direction2dMap[directions2d[[did]]] = did;
];

directionsInPolygon = {6, 4, 7, 5, 2, 4, 1, 3, 12, 10, 10, 8, 11, 9, 6};

polygon4d = {{0, 0, 0, 0}};
For[vid = 2, vid <= 14, vid++,
	lastPoint = Last[polygon4d];
	newPoint = lastPoint + directions4d[[ directionsInPolygon[[vid]] ]];
	AppendTo[polygon4d, newPoint];
];

polygon2d = polygon4d.projection42//N;

polygon2dshifted = move[polygon2d, (polygon2d[[11]] - polygon2d[[5]]) ];



graph = Graphics[ Polygon[polygon2d] ];
region1 = Region[Polygon[polygon2d]];
region2 = Region[Polygon[polygon2dshifted]];
regionUnion = RegionUnion[region1, region2];


Print[regionDisjointByArea[region1, region2] ];

getAnglesFromRegion[region_] := Module[
	{edges, relativeEdges, edgeDirections, angles, anglesMod},
	edges = getEdgesFromRegion[region];
	relativeEdges = Map[#[[2]] - #[[1]] &, edges];
	edgeDirections = Map[Arg[#[[1]] + I #[[2]]]/Pi * 180.0&, relativeEdges];
	angles = Round[Map[#[[2]] - #[[1]]&, getEdgesFromFace[ edgeDirections ] ]];
	anglesMod = Map[Mod[# + 180, 360]-180&, angles];
	anglesMod
];

angles1 = getAnglesFromRegion[region1];

Print[angles1 ];
Print[Total[angles1]];

anglesUnion = getAnglesFromRegion[regionUnion];
Print[anglesUnion ];
Print[Total[anglesUnion]];



(* Exit[]; *)
Export["output/test/grow_test.png", graph ];
Export["output/test/grow_test_region.png", region1 ];
Export["output/test/grow_test_region2.png", regionUnion ];

Exit[];

projection44[t_] := {
	{Cos[t], 0, Sin[t], 0},
	{0, Cos[t], 0, Sin[t]},
	{-Sin[t], 0, Cos[t], 0},
	{0, -Sin[t], 0, Cos[t]}
};

(* projection43[t_] := projection44[t][[{1, 2, 3}]]; *)
projection43[t_, s_] := {
	projection44[t][[1]],
	projection44[t][[2]],
	(projection44[t][[3]] Cos[s] + projection44[t][[4]] Sin[s])
};

flipDimensions = {
	{0, 0, 1, 0},
	{0, 0, 0, 1},
	{1, 0, 0, 0},
	{0, 1, 0, 0}
};

mirror4D = DiagonalMatrix[{1,-1,1,-1}];

parallelogramIds[ startIds_, d1_, d2_] := {
	startIds,
	Join[startIds, d1],
	Join[startIds, d1, d2],
	Join[startIds, d2]
};

cellsIds = {
	parallelogramIds[{}, {4}, {5}],
	parallelogramIds[{1}, {4}, {5}],
	parallelogramIds[{8}, {4}, {5}],
	parallelogramIds[{1, 8}, {4}, {5}],

	parallelogramIds[{}, {4}, {1}],
	parallelogramIds[{5}, {4}, {1}],
	parallelogramIds[{8}, {4}, {1}],
	parallelogramIds[{5, 8}, {4}, {1}],

	parallelogramIds[{}, {4}, {8}],
	parallelogramIds[{5}, {4}, {8}],
	parallelogramIds[{1}, {4}, {8}],
	parallelogramIds[{5, 1}, {4}, {8}],

	parallelogramIds[{}, {5}, {1}],
	parallelogramIds[{4}, {5}, {1}],
	parallelogramIds[{8}, {5}, {1}],
	parallelogramIds[{4, 8}, {5}, {1}],

	parallelogramIds[{}, {5}, {8}],	
	parallelogramIds[{4}, {5}, {8}],	
	parallelogramIds[{1}, {5}, {8}],	
	parallelogramIds[{4, 1}, {5}, {8}],	

	parallelogramIds[{}, {1}, {8}],
	parallelogramIds[{4}, {1}, {8}],
	parallelogramIds[{5}, {1}, {8}],
	parallelogramIds[{4, 5}, {1}, {8}]
};

cell1 = Map[If[# == {}, {0,0,0,0},Total[directions[[#]]] ]& , cellsIds, {2}];

intersectingFaces[fid_, vectorId_] := Total[Map[Length[Intersection[#, {vectorId}]]&, cellsIds[[fid]] ]];

faceIdsInThreeFaces = {};
AppendTo[faceIdsInThreeFaces, Select[Range[Length[cellsIds]], intersectingFaces[#, 1] == 0 &] ];
AppendTo[faceIdsInThreeFaces, Select[Range[Length[cellsIds]], intersectingFaces[#, 1] == 4 &] ];
AppendTo[faceIdsInThreeFaces, Select[Range[Length[cellsIds]], intersectingFaces[#, 4] == 0 &] ];
AppendTo[faceIdsInThreeFaces, Select[Range[Length[cellsIds]], intersectingFaces[#, 4] == 4 &] ];
AppendTo[faceIdsInThreeFaces, Select[Range[Length[cellsIds]], intersectingFaces[#, 5] == 0 &] ];
AppendTo[faceIdsInThreeFaces, Select[Range[Length[cellsIds]], intersectingFaces[#, 5] == 4 &] ];
AppendTo[faceIdsInThreeFaces, Select[Range[Length[cellsIds]], intersectingFaces[#, 8] == 0 &] ];
AppendTo[faceIdsInThreeFaces, Select[Range[Length[cellsIds]], intersectingFaces[#, 8] == 4 &] ];

rotate[angle1_, angle2_] := ArrayFlatten[{
	{RotationMatrix[angle1/180*Pi], 0}, 
	{0, RotationMatrix[angle2/180*Pi]}}
];

move[cells_, offset_] := Map[# + offset &, cells, {3}];
rotateCell[cell_, angle1_, angle2_] := Map[rotate[angle1, angle2].# &, cell, {2} ];

gridThreshold = Round[truncateThreshold * 1.5];
triangleGrid = Flatten[Table[{a + b Cos[Pi/3], b Sin[Pi/3]}, {a, -gridThreshold, gridThreshold}, {b, -gridThreshold, gridThreshold}], 1];

triangleGrid = Select[triangleGrid, Norm[#] < truncateThreshold + epsilon & ];

cells = {cell1};
cells = Join[cells, Map[rotateCell[#, 120, 0]&, cells], Map[rotateCell[#, 240, 0]&, cells]];
cells = Join[cells, Map[rotateCell[#, 0, 120]&, cells], Map[rotateCell[#, 0, 240]&, cells]];
Print[Length[cells]];

cells = Apply[Join, Map[move[cells, { 2 Sqrt[3] #[[1]], 2 Sqrt[3] #[[2]], 0, 0 } ]&, triangleGrid]];
Print[Length[cells]];

cells = Apply[Join, Map[move[cells, {0, 0, 2 Sqrt[3] #[[2]], 2 Sqrt[3] #[[1]] } ]&, triangleGrid]];
Print["Generated cells: " <> IntegerString[Length[cells]]];

cells = move[cells, {0.4324, 0.168, 0.22, 0.347}/10];

If[selectionMethod == "2face", 
	faces = N[Flatten[cells, 1]];
	Print[faces//Length];
	(* uniqueFaces = Union[faces, SameTest -> sameCenter]; *)
	uniqueFaces = faces;
	Print[uniqueFaces//Length];
	cells = Map[{#}&, uniqueFaces];
];

If[selectionMethod == "3face",
	outputCells = {};
	For[cid = 1, cid <= Length[cells], cid++,
		cell = cells[[cid]];
		threeFaces = {};
		For[fid = 1, fid <= Length[faceIdsInThreeFaces], fid++,
			AppendTo[outputCells, cell[[ faceIdsInThreeFaces[[fid]] ]] ];
		];
	];

	Print[Length[outputCells]];
	cells = outputCells;

	(* uniqueOutputCells = Union[outputCells, SameTest -> sameCellCenter]; *)
	(* Print[Length[uniqueOutputCells]]; *)
	(* cells = uniqueOutputCells; *)

];

Print["After applying selectionMethod " <> selectionMethod <> " the count of selection unit is " <> IntegerString[Length[cells]]];

cellCenters = Map[getCellCenter[#]&, cells];
(* cellCenters = Map[getCellCenter[#] - getCellCenter[cell1]&, cells]; *)

(* for spectre tiling *)
(* 0.45887043288175683 - 0.615639288473018*I *)
(* targetComplexSlope = ( (5 Sqrt[3] - Sqrt[5]) + I * (3 - 3 * Sqrt[15]) ) /14;
targetComplexSlope = 1/targetComplexSlope;
slopeVector1 = {1, 0, - Re[targetComplexSlope], Im[targetComplexSlope]}//N;
slopeVector2 = {0, 1, - Im[targetComplexSlope], -Re[targetComplexSlope]}//N;
 *)
targetSlope = If[ shape == "chiral_tiling", ( (5 Sqrt[3] - Sqrt[5]) + I * (3 - 3 * Sqrt[15]) ) /14, (Sqrt[3] - Sqrt[5] I)/4];
(* targetSlope = 1/targetSlope; *)
sr = Re[targetSlope];
si = Im[targetSlope];
slopeVector1 = {sr, -si, -1, 0}/Norm[{1, sr, si}]//N;
slopeVector2 = {si, sr, 0, -1}/Norm[{1, sr, si}]//N;
(* slopeVector1 = {1, 0, - sr, si}//N; *)
(* slopeVector2 = {0, 1, -si, -sr}//N; *)


(* z + wi = (sr + i si)(x+iy) = sr x - si y + i(si x + sr y)
sr x - si y - z = 0
si x + sr y - w = 0
[sr, -si, -1, 0]
[si, sr, 0, -1 ]
 *)

cellsToKeep = {};
(* complexPerturbation = Table[0.1, {k, 4}]; *)
(* complexPerturbation = {0, 1., -1, 0}; *)
complexPerturbation = {0, 0, 0, 0};

epsilonFactor = If[intersectionIncludeBoundary, -1, 1];

For[cid = 1, cid <= Length[cells], cid++,
	If[Mod[cid, 1000] == 0, Print["Processing cell " <> IntegerString[cid]]];
	cell = cells[[cid]];
	anyFaceCrossingTarget = False;
	For[fid = 1, fid <= Length[cell], fid++,
		face = Map[# + complexPerturbation &, cell[[fid]]]//N;
		matrix22 = {
			{(face[[2]] - face[[1]]).slopeVector1, (face[[4]] - face[[1]]).slopeVector1},
			{(face[[2]] - face[[1]]).slopeVector2, (face[[4]] - face[[1]]).slopeVector2}
		};
		offsets = -{
			{face[[1]].slopeVector1},
			{face[[1]].slopeVector2}
		};

		If[Abs[Det[matrix22]] < epsilon, Print["skip"]; Continue[]];

		lambdas = Inverse[matrix22].offsets;
		lambdas = Transpose[lambdas][[1]];
		rhombusIntersects = lambdas[[1]] > epsilonFactor epsilon && lambdas[[1]] < 1 - epsilonFactor epsilon && lambdas[[2]] > epsilonFactor epsilon && lambdas[[2]] < 1 - epsilonFactor epsilon;

		If[rhombusIntersects,
			(* Print["face " <> IntegerString[fid] <> " crossing cell " <> IntegerString[cid]]; *)
			anyFaceCrossingTarget = True;
			Break[];
		];
	];
	If[anyFaceCrossingTarget,
		AppendTo[cellsToKeep, cid];
	];
];

Print["cells to keep: " <> IntegerString[Length[cellsToKeep]]];
cells = cells[[cellsToKeep]];

exportImage[cells, Pi/4, Pi/4, 45, "intersection_selection"];
exportImage[cells, Pi/6, Pi/4, 30, "intersection_selection"];
exportImage[cells, Pi/3, Pi/4, 60, "intersection_selection"];


