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

range = If[singleTile, 3, If[shape == "spectre", 10, 8]]; (* change to 10 *)
thickness = 0.05;

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

reflect[point_, mirror_] := FullSimplify[point - 2 * (point.mirror)/(mirror.mirror)*mirror];
doubleReflect[point_, mirror1_, mirror2_] := reflect[reflect[point, mirror1], mirror2];
getEdgesFromFace[face_] := Table[{face[[i+1]], face[[Mod[i+1, Length[face]] + 1]]}, {i, 0, Length[face] - 1}];
explodedFace[face_, explosionFactor_] := Map[(# + Mean[face] * explosionFactor)&, face];
sameVertex[point1_, point2_] := point1 == point2;
sameCellCenter[cell1_, cell2_] := getCellCenter[cell1] == getCellCenter[cell2];
getFaceCenter[face_] := Simplify[Total[face]];
getCellCenter[cell_] := Simplify[Mean[Flatten[cell, 1]]];
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
epsilon = 0.00000001;
ApproxSamePoint[point1_, point2_] := Round[point1, epsilon] == Round[point2, epsilon];
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


exportImage[polygons_, theta_, sigma_, serialNumber_, frameFolder_] := Module[
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

	projectedPolygons = Map[projectionMatrix.#&, polygons, {2}];
	edges3d = Flatten[Map[getEdgesFromFace, projectedPolygons], 1];
	uniqueEdges3d = Union[edges3d, SameTest -> sameCenter];
	(* Print[Map[Length, {edges3d, uniqueEdges3d}]]; *)

	facesByPolygons = {};
	For[pid = 1, pid <= Length[projectedPolygons], pid++,
		projectedPolygon = projectedPolygons[[pid]];
		faces = {};
		If[meshMethod == "",
			faces = Map[projectedPolygon[[#]]&, triangleIds ];
			polygonCenter = Mean[projectedPolygon[[ {1, 2, 4, 5, 6, 7, 9, 10, 11, 13, 14} ]] ];
			faces = Join[faces, {
					{polygonCenter, projectedPolygon[[2]], projectedPolygon[[5]]},
					{polygonCenter, projectedPolygon[[5]], projectedPolygon[[7]]},
					{polygonCenter, projectedPolygon[[7]], projectedPolygon[[12]]},
					{polygonCenter, projectedPolygon[[12]], projectedPolygon[[2]]}
				}];
		];

		If[meshMethod == "design1",
			p = projectedPolygon;
			q1 = p[[12]] + p[[14]] - p[[13]];
			q2 = p[[14]] + p[[2]] - p[[1]];
			q3 = q1 + q2 - p[[14]];
			q4 = p[[2]] + p[[4]] - p[[3]];
			q5 = q4 + p[[5]] - p[[4]];
			q6 = p[[8]] + p[[10]] - p[[9]];
			q7 = q3 + q5 - q4;
			q8 = q3 + p[[12]] - q1;

			faces = {
				{p[[1]], p[[2]], q2, p[[14]]},
				{p[[12]], p[[13]], p[[14]], q1},
				{p[[14]], q1, q3, q2},
				{p[[2]], q4, q3, q2},
				{p[[2]], p[[3]], p[[4]], q4},
				{p[[4]], p[[5]], q5, q4},
				{p[[5]], p[[6]], p[[7]], q5},
				{q4, q5, q7, q3},
				{q1, q3, q8, p[[12]]},
				{p[[10]], p[[11]], p[[12]], q8},
				{p[[10]], q7, q5, q6},
				{p[[7]], p[[8]], q6, q5},
				{p[[8]], p[[9]], p[[10]], q6},
				{p[[10]], q7, q3, q8}
			};
		];

		If[meshMethod == "design2",
			p = projectedPolygon;
			q1 = p[[1]] + p[[13]] - p[[14]];
			q2 = p[[2]] + q1 - p[[1]];
			q3 = q1 + p[[12]] - p[[13]];
			q4 = p[[2]] + p[[4]] - p[[3]];
			q5 = q2 + q3 - q1;
			q6 = p[[5]] + q4 - p[[4]];
			q7 = p[[1]] + q3 - p[[12]];
			q8 = p[[8]] + p[[10]] - p[[9]];

			faces = {
				{p[[1]], q1, p[[13]], p[[14]]},
				{p[[1]], q1, q2, p[[2]]},
				{q1, q3, p[[12]], p[[13]]},
				{q1, q3, q5, q2},
				{p[[2]], q4, q5, q2},
				{p[[2]], p[[3]], p[[4]], q4},
				{p[[4]], p[[5]], q6, q4},
				{p[[5]], p[[6]], p[[7]], q6},
				{p[[7]], p[[8]], q8, q6},
				{q4, q5, q8, q6},
				{p[[8]], p[[9]], p[[10]], p[[11]], p[[12]], q3, q5, q8}
			};
		];
		(* faces = {projectedPolygon}; *)
		AppendTo[facesByPolygons, faces];
	];

	(* facesByPolygons = Table[Map[projectedPolygons[[k]][[#]]&, triangleIds ], {k, 1, Length[projectedPolygons]}]; *)

	(* faces = Map[projectedPolygon[[#]]&, triangleIds ]; *)
	(* faces = {projectedPolygon}; *)
	(* faces = Map[{#[[1]], #[[2]], polygonCenter}&, edges3d]; *)

	edgeColor = If[meshMethod == "noFaces", Blue, White];

	graphElements = {
		{
			FaceForm[edgeColor], Map[Cylinder[#, thickness]&, edges3d]
		},
		{
			FaceForm[edgeColor], Map[Sphere[#, 2 thickness]&, Flatten[projectedPolygons, 1]]
		},
		Table[
			{
				FaceForm[colors[[k]] ], Opacity[1], Map[Polygon, facesByPolygons[[k]] ]
			},
		{k, Length[facesByPolygons]}]
		
	};


	graph = Graphics3D[
		graphElements,
		ViewPoint -> If[topView, {0, 0, 1}, {0.3, -0.4, 1}].lightingRotation * 1000,
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

offset[polygon_, move_] := Map[# + move &, polygon];
rotate[angle_] := ArrayFlatten[{
	{RotationMatrix[angle/180*Pi], 0}, 
	{0, RotationMatrix[angle/180*Pi]}}
];

rotateAt[polygon_, angle_, axis_] := Map[rotate[angle].(# - axis) + axis &, polygon];

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
(* polygons = Map[offset[#, - center ]&, polygons]; *)

targetComplexSlope = If[shape == "spectre_4d",
	( (5 Sqrt[3] - Sqrt[5]) + I * (3 - 3 * Sqrt[15]) ) /14,
	(Sqrt[3] - Sqrt[5] I)/4
];

targetComplexSlope = 1/targetComplexSlope;
Print["shape: " <> shape <> " target: "];
Print[targetComplexSlope//N];
Print[];

sr = Re[targetComplexSlope];
si = Im[targetComplexSlope];

orthProjection44 = {
	{-sr, si, 1, 0}, 
	{-si, -sr, 0, 1},
	{1, 0, sr, si},
	{0, 1, -si, sr}
}/Sqrt[1 + sr^2 + si^2];
orthProjection42 = orthProjection44[[{1,2}]]//N;

complexMeanError[w01_, w02_, w03_, w04_, w05_, w06_, w07_, w08_, w09_, w10_, w11_, w12_, w13_] := Module[
	{polygonCenters, complexCenters, complexError, complexErrorNorms},
	(* Print[{w01, w02, w03, w04, w05, w06, w07, w08, w09, w10, w11, w12, w13, 1-(w01+w02+w03+w04+w05+w06+w07+w08+w09+w10+w11+w12+w13)}]; *)
	polygonCenters = Map[Dot[{w01, w02, w03, w04, w05, w06, w07, w08, w09, w10, w11, w12, w13, 1-(w01+w02+w03+w04+w05+w06+w07+w08+w09+w10+w11+w12+w13)}, #]&, N[polygons]];
	orthErrors = Map[ Norm[#.Transpose[orthProjection42]]&, polygonCenters ];
	orthErrorNorms = Map[Norm[#]&, orthErrors];
	Total[orthErrorNorms]/Length[polygons]
];

(* Print[complexError//N];
Print[Mean[complexError//N]];
 *)

Print["average case"];
Print[ Apply[complexMeanError, Table[1/14, {k, 13}] ] ];
(* Exit[]; *)

(* vector = {0.518131, 0.566107, 0.842635, 0.117211, -0.391416, 0.0852854, 0.409671, 0.576152, 0.598847, 0.356776, -0.203696, 0.964536, 0.998889}
vector = Join[ vector, { 1 - Total[vector]} ];
Print[vector];
Print[ complexMeanError[ vector ] ];
 *)

Print["minimize"];

(* minSolution = NMinimize[ complexMeanError[ w01, w02, w03, w04, w05, w06, w07, w08, w09, w10, w11, w12, w13 ], {w01, w02, w03, w04, w05, w06, w07, w08, w09, w10, w11, w12, w13} ]; *)

minSolution = FindMinimum[ complexMeanError[w01, w02, w03, w04, w05, w06, w07, w08, w09, w10, w11, w12, w13 ], { {w01,1/14.},{ w02,1/14.},{ w03,1/14.},{ w04,1/14.},{ w05,1/14.},{ w06,1/14.},{ w07,1/14.},{ w08,1/14.},{ w09,1/14.},{ w10,1/14.},{ w11,1/14.},{ w12,1/14.},{ w13,1/14.} } ];

Print[minSolution];

(* 
average case
2.1919454171570107
minimize
{2.061305293077254, {w01 -> 0.03236986736060123, w02 -> 0.07799316476924252, w03 -> -0.05911191295628166, w04 -> -0.17176748426831062, w05 -> -0.08708542852278012, w06 -> -0.04146222358595754, w07 -> 0.09564273392713142, w08 -> 0.12009210847871328, w09 -> 0.15915097318708082, w10 -> 0.11352767411505187, w11 -> 0.06790425612793533, w12 -> -0.016777888519672258, w13 -> 0.09587778350167393}}
 *)