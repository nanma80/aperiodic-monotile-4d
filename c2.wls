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

(* singleTile = False; *)
singleTile = True;

(* fullRotation = False; *)
fullRotation = True;

shape = "spectre_4d";
(* shape = "hat_4d"; *)

(* meshMethod = ""; *)
meshMethod = "design1";
(* meshMethod = "design2"; *)
(* meshMethod = "noFaces"; *)

rotationDirection = "";
(* rotationDirection = "3rdDim"; *)

exportToPov = True;
dataFolder = "data";
imageFolder = "output";
imageSize = 720/3 * {4, 3};

truncateThreshold = 1;

range = 10;
(* If[singleTile, 3, If[shape == "spectre", 10, 8]]; (* change to 10 *) *)
thickness = 0.02;

shapeWithParams = shape <> If[topView, "", "_sideView"] <> If[fullRotation, "", "_quarterTurn"] <> If[singleTile, "_singleTile", ""] <> If[meshMethod == "", "", "_" <> meshMethod] <> If[rotationDirection == "", "", "_" <> rotationDirection] <> "_" <> IntegerString[frameCount];

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

	outputFileName = "animation_" <> shape <> "_" <> IntegerString[serialNumber, 10, 4] <> ".png";
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
		Run[cropCommand];
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



(* exportImage[shape, 1, 9, 2.9, 1 deltaTheta, 11]; *)
(* exportImage[shape, 1, 9, 2.9, 2 deltaTheta, 12]; *)
(* exportImage[shape, 1, 9, 2.9, 3 deltaTheta, 13]; *)
(* exportImage[shape, 1, 9, 2.9, 4 deltaTheta, 14]; *)

directions = {};
For[clock = 1, clock <= 12, clock++,
	vector2d = 2 {Sin[2 Pi / 12 * clock], Cos[2 Pi / 12 * clock]};
	vector4d = If[Mod[clock, 2] == 0, 
		Join[vector2d, {0, 0}],
		Join[{0, 0}, vector2d]
	];
	AppendTo[directions, vector4d];
];
directionMap = <||>;
For[did = 1, did <= Length[directions], did++,
	directionMap[directions[[did]]] = did;
];

directionsInPolygon = {6, 4, 7, 5, 2, 4, 1, 3, 12, 10, 10, 8, 11, 9, 6};


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

cellCenters = Map[getCellCenter[#]&, cells];
(* cellCenters = Map[getCellCenter[#] - getCellCenter[cell1]&, cells]; *)

(* for spectre tiling *)
(* 0.45887043288175683 - 0.615639288473018*I *)
targetComplexSlope = ( (5 Sqrt[3] - Sqrt[5]) + I * (3 - 3 * Sqrt[15]) ) /14;
slopeVector1 = {1, 0, - Re[targetComplexSlope], Im[targetComplexSlope]}//N;
slopeVector2 = {0, 1, - Im[targetComplexSlope], -Re[targetComplexSlope]}//N;

cellsToKeep = {};
(* complexPerturbation = Table[0.1, {k, 4}]; *)
(* complexPerturbation = {0, 1., -1, 0}; *)
complexPerturbation = {0, 0, 0, 0};

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
		rhombusIntersects = lambdas[[1]] > epsilon && lambdas[[1]] < 1 - epsilon && lambdas[[2]] > epsilon && lambdas[[2]] < 1 - epsilon;

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
(* Exit[]; *)

cells = cells[[cellsToKeep]];

exportImage[cells, Pi/4, Pi/4, 45, "test"];
(* exportImage[cells, Pi/6, Pi/4, 30, "test"]; *)
(* exportImage[cells, Pi/3, Pi/4, 60, "test"]; *)


Exit[];

polygon4d = {{0, 0, 0, 0}};
For[vid = 2, vid <= 14, vid++,
	lastPoint = Last[polygon4d];
	newPoint = lastPoint + directions[[ directionsInPolygon[[vid]] ]];
	AppendTo[polygon4d, newPoint];
];

triangleIds = {
	{14, 1, 2},
	{2, 14, 12},
	{12, 13, 14},
	{2, 3, 4},
	{2, 4, 5},
	{5, 6, 7},
	{7, 8, 9},
	{7, 9, 11},
	{7, 11, 12}
};


getSpectreTiling[polygon4d_] := Module[
	{flippedPolygon, turtle1, hat1},

	flippedPolygon = polygon4d.flipDimensions;
	turtle1 = polygon4d;
	hat1 = rotateAt[offset[flippedPolygon, - flippedPolygon[[6]] + polygon4d[[14]]], 30, polygon4d[[14]]];
	hat2 = offset[ rotateAt[hat1, -60, polygon4d[[14]]], polygon4d[[12]] - polygon4d[[14]] ];
	hat3 = offset[ rotateAt[hat1, 60, polygon4d[[14]]], polygon4d[[2]] - polygon4d[[14]] ];
	hat4 = offset[ hat3, hat3[[12]] - hat3[[4]] ];
	hat5 = offset[rotateAt[hat3, 60, polygon4d[[1]] ], hat3[[3]] - hat3[[7]] ];
	hat6 = offset[hat1, polygon4d[[4]] - hat1[[14]]];
	hat7 = offset[ rotateAt[hat2, -60, polygon4d[[10]]  ], polygon4d[[8]] - polygon4d[[10]]];
	hat8 = offset[hat2, hat2[[12]] - hat2[[4]]];
	hat9 = offset[hat3, hat6[[8]] - hat3[[12]] ];
	turtle2 = offset[rotateAt[turtle1, 60, turtle1[[5]] ], -turtle1[[5]] + hat4[[13]]];
	hat10 = offset[hat2, -hat2[[7]] + turtle2[[9]] ];
	hat11 = rotateAt[hat4, 120, hat4[[3 ]] ];
	hat12 = offset[hat5, -hat5[[5]] + turtle2[[3]]];
	hat13 = offset[hat12, hat12[[12]]-hat12[[4]] ];
	hat14 = offset[hat4, -hat4[[5]] + hat12[[9]] ];
	hat15 = offset[hat1, -hat1[[5]] + turtle2[[13]]];
	hat16 = offset[hat15, hat15[[12]] - hat15[[4]] ];
	hat17 = offset[hat5, hat5[[12]] - hat5[[4]] ];
	hat18 = offset[hat1, -hat1[[12]] + hat12[[14]]];

	turtle3 = offset[rotateAt[turtle1, 120, turtle1[[5]] ], -turtle1[[5]] + hat17[[13]]];
	hat19 = offset[ hat3, turtle3[[13]] - hat3[[5]] ];
	hat20 = offset[hat19, hat19[[12]]-hat19[[4]] ];
	hat21 = offset[hat6, hat6[[4]]-hat6[[12]] ];
	hat22 = offset[hat21, hat21[[4]]-hat21[[12]] ];
	hat23 = offset[hat7, hat22[[14]] - hat7[[12]] ];
	hat24 = offset[hat11, - hat11[[5]] + turtle3[[3]] ];
	hat25 = offset[hat24, hat24[[12]] - hat24[[4]] ];
	hat26 = offset[hat12, hat24[[9]]-hat12[[5]] ];

	turtle4 = offset[turtle2, -turtle2[[13]] + hat22[[5]]];
	hat27 = offset[hat19, -hat19[[12]] + hat24[[14]] ];
	hat28 = offset[hat26, -hat26[[5]] + turtle4[[3]] ];
	hat29 = offset[hat28, hat28[[12]]-hat28[[4]] ];
	hat30 = offset[hat2, -hat2[[5]]+hat22[[9]] ];
	hat31 = offset[hat9, hat9[[4]]-hat9[[12]] ];
	hat32 = offset[hat31, -hat31[[12]]+turtle4[[6]]];
	hat33 = offset[hat24, -hat24[[5]]+hat32[[1]]];
	hat34 = offset[hat6, -hat6[[14]]+hat7[[12]]];

	turtle5 = offset[turtle1, -turtle1[[1]]+hat31[[7]]];
	hat35 = offset[hat2, -hat2[[5]] + turtle5[[13]]];
	hat36 = offset[hat35, hat35[[12]] - hat35[[4]]];
	hat37 = offset[hat7, -hat7[[5]] + hat35[[9]]];
	hat38 = offset[hat28, -hat28[[12]] + hat32[[8]]];
	hat39 = offset[hat6, -hat6[[12]] + turtle5[[6]]];

	polygons = {
		turtle1,	hat1,	hat2,	hat3,	hat4,	hat5,	hat6,	hat7,	hat8,	hat9,
		turtle2,	hat10,	hat11,	hat12,	hat13,	hat14,	hat15,	hat16,	hat17,	hat18,
		turtle3,	hat19,	hat20,	hat21,	hat22,	hat23,	hat24,	hat25,	hat26,
		turtle4,	hat27,	hat28, 	hat29, hat30, hat31, hat32, hat33, hat34,
		turtle5, hat35, hat36, hat37, hat38, hat39
	};
	polygons
];

getHatTiling[polygon4d_] := Module[
	{flippedPolygon, hat1},

	flippedPolygon = polygon4d.flipDimensions;
	hat1 = rotateAt[flippedPolygon, 30, flippedPolygon[[1]]];
	mirror1 = rotateAt[ hat1.mirror4D, -120, hat1[[1]] ];
	mirror1 = offset[mirror1, -mirror1[[11]] + hat1[[5]] ];
	hat2 = rotateAt[hat1, -120, hat1[[1]] ];
	hat2 = offset[hat2, -hat2[[5]] + mirror1[[1]]];
	hat3 = offset[hat1, -hat1[[12]] + mirror1[[6]]];
	hat4 = rotateAt[hat2, 60, hat2[[1]]];
	hat4 = offset[hat4, -hat4[[5]] + hat1[[9]]];
	hat5 = offset[hat1, -hat1[[5]] + hat1[[11]]];

	hat6 = rotateAt[hat4, 120, hat4[[1]]];
	hat6 = offset[hat6, -hat6[[5]] + mirror1[[9]]];
	hat7 = rotateAt[hat5, 120, hat5[[1]]];
	hat7 = offset[hat7, -hat7[[5]] + hat3[[1]]];

	hat8 = offset[hat6, -hat6[[12]] + hat3[[8]]];
	hat9 = offset[hat1, -hat1[[2]] + hat8[[8]] ];

	hat10 = offset[hat7, -hat7[[12]] + hat2[[14]]];
	hat11 = rotateAt[hat10, 60, hat10[[1]]];
	hat11 = offset[hat11, -hat11[[8]] + hat10[[2]] ];

	mirror2 = rotateAt[mirror1, -60, mirror1[[1]]];
	mirror2 = offset[mirror2, -mirror2[[9]] + hat9[[5]] ];

	hat12 = offset[hat4, -hat4[[5]] + mirror2[[11]] ];
	hat13 = offset[hat4, -hat4[[12]] + mirror2[[6]]];

	hat14 = offset[hat13, -hat13[[11]] +hat13[[5]]];
	hat15 = offset[hat3, -hat3[[11]] +hat3[[5]]];
	hat16 = offset[hat2, -hat2[[12]] + hat1[[14]]];
	hat17 = offset[hat11, -hat11[[14]] + hat6[[12]]];
	hat18 = offset[hat13, -hat13[[12]] + hat6[[14]]];

	mirror3 = rotateAt[mirror1, 60, mirror1[[1]] ];
	mirror3 = offset[mirror3, -mirror3[[2]] + hat18[[6]] ];
	hat19 = offset[hat6, -hat6[[12]] + mirror3[[6]] ];
	hat20 = offset[hat19, -hat19[[11]] +hat19[[5]]];
	hat21 = offset[hat20, -hat20[[11]] +hat20[[5]]];

	mirror4 = offset[mirror3, -mirror3[[12]] +hat21[[6]]];
	hat22 = offset[hat21, -hat21[[12]] + mirror4[[6]] ];
	hat23 = offset[hat3, -hat3[[12]] + hat13[[8]] ];




	polygons = {
		hat1, hat2, hat3, hat4, hat5, hat6, hat7, hat8, hat9, hat10, hat11,
		hat12, hat13, hat14, hat15, hat16, hat17, hat18, hat19, hat20, hat21,
		hat22, hat23,
		mirror1, mirror2, mirror3, mirror4
	};	
	polygons
];

getTiling[polygon4d_] := If[shape == "spectre_4d",
	getSpectreTiling[polygon4d],
	getHatTiling[polygon4d]
];

polygons = getTiling[polygon4d];

If[singleTile, polygons = polygons[[{1}]]];

(* polygons = Map[offset[#, - If[singleTile, Mean[turtle1], hat6[[3]] ] ]&, polygons]; *)
vector12s = Map[(Transpose[{{1, 0}, {0, 1}, {1, 0}, {0, 1}}].(#[[2]] - #[[1]]))&, polygons];
colors = Map[ Hue[ Mod[Arg[#[[1]] + I #[[2]]]/2/Pi , 1] ]&, vector12s];

center = Mean[Map[Mean, polygons]];
polygons = Map[offset[#, - center ]&, polygons];

polygonCenters = Map[Mean, polygons];
polygonComplexCenters = Map[ {#[[1]] + I #[[2]], #[[3]] + I #[[4]]}&, polygonCenters ];

polygonComplexCentersOffset = Map[# - polygonComplexCenters[[1]] &, polygonComplexCenters ];
polygonComplexCentersOffset = Select[polygonComplexCentersOffset, Norm[#] > epsilon&];

(* Map[Print[N[#]]&, polygonComplexCentersOffset]; *)
slopes = Map[ #[[1]]/#[[2]] &, polygonComplexCentersOffset ];

(* for spectre tiling *)
(* 0.45887043288175683 - 0.615639288473018*I *)
complexSlope = ( (5 Sqrt[3] - Sqrt[5]) + I * (3 - 3 * Sqrt[15]) ) /14;


(* for hat tiling, avg slope is 0.47 - 0.42 I *)
Print[Mean[slopes//N]];
Print[N[complexSlope]];

(* For[id = 0, id <= 6 * 4, id++,
	exportImage[polygons, id/6*Pi/2, id, "test"];
];
 *)

(* exportAnimation[]; *)
