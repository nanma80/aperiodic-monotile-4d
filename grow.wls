#!/usr/bin/env wolframscript

(*
mogrify -crop 450x450+75+0 -path . pov_*.png
mogrify -crop 300x300+50+0 -path . explode*.gif

magick convert -duplicate 1,-2-1 -quiet +repage -alpha remove -loop 0 -delay 5 -dispose 2 *.png out.gif
magick convert out.gif -coalesce -duplicate 1,-2-1 -quiet -layers OptimizePlus  -loop 0 cycle.gif
python -m SimpleHTTPServer 8000

*)

(* global config *)

verbose = False;
(* verbose = True; *)

(* frameCount = 2; *)
(* frameCount = 5; *)
(* frameCount = 10; *)
frameCount = 100;
(* frameCount = 60; *)

(* topView = False; *)
topView = True;

(* filterBy4d = False; *)
filterBy4d = True;
filePrefix = If[filterBy4d, "4d_plane", "naive_grow"];

singleTile = False;
(* singleTile = True; *)

(* fullRotation = False; *)
fullRotation = True;

shape = "chiral_tiling";
(* shape = "nonchiral_tiling"; *)

targetSlope = If[ shape == "chiral_tiling", ( (5 Sqrt[3] - Sqrt[5]) + I * (3 - 3 * Sqrt[15]) ) /14, (Sqrt[3] - Sqrt[5] I)/4];
(* targetSlope = 1/targetSlope; *)

sr = Re[targetSlope];
si = Im[targetSlope];

orthProjection44 = {
	{-sr, si, 1, 0}, 
	{-si, -sr, 0, 1},
	{1, 0, sr, si},
	{0, 1, -si, sr}
}/Sqrt[1 + sr^2 + si^2];
orthProjection42 = orthProjection44[[{1,2}]];

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

range = 35;
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
regionDisjointByArea[region1_, region2_] := Abs[ Area[region1] + Area[region2] - Area[RegionUnion[region1, region2]] ] < Sqrt[epsilon];

(* regionDisjointByArea[region1_, region2_] := Abs[ Area[region1] + Area[region2] - Area[RegionUnion[BoundaryDiscretizeRegion[region1], BoundaryDiscretizeRegion[region2]]] ] < Sqrt[epsilon]; *)

(* regionDisjointByArea[region1_, region2_] := Module[
	{regionUnion},
	Print["region1"];
	Print[region1];
	Print["region2"];
	Print[region2];
	regionUnion = RegionUnion[region1, region2];
	Print["finished union " <> ToString[regionUnion]];
	result = (Abs[ Area[region1] + Area[region2] - Area[RegionUnion[region1, region2]] ] < Sqrt[epsilon]);
	Print["calculated result " <> ToString[result]];
	result
];
 *)



(* regionDisjointByIntersection[region1_, region2_] := Area[ RegionIntersection[region1, region2] ] < Sqrt[epsilon]; *)

getPoints[region_] := Apply[List, RegionBoundary[region]][[1]];
getEdgesFromPoints[face_] := Table[{face[[i]], face[[i + 1]]}, {i, 1, Length[face] - 1}];
getEdgesFromRegion[region_] := getEdgesFromPoints[getPoints[region]];
getDirection[vector2d_] := Mod[Round[Arg[vector2d[[1]] + I vector2d[[2]]]/Pi * 180.0], 360];

getConvexParts[polygon_] := {
	polygon[[{1, 2, 12, 13, 14}]],
	polygon[[{2,3,4,5,11,12}]],
	polygon[[{5,6,7,11}]],
	polygon[[{7,8,9,10,11}]]
};

regionDisjointByTile[onePolygon_, manyPolygons_] := Module[
	{nearbyPolygons},
	nearbyPolygons = Select[manyPolygons, EuclideanDistance[ Mean[#], Mean[onePolygon] ] < 10 &];
	AllTrue[nearbyPolygons, regionDisjointByArea[ Region[Polygon[onePolygon]], Region[Polygon[#]] ]&]
];

regionDisjointByConvexParts[onePolygon_, manyPolygons_] := Module[
	{nearbyPolygons, partsFromOnePolygon, partsFromNearbyPolygons, pid, part1, disjointSubResult},
	nearbyPolygons = Select[manyPolygons, EuclideanDistance[ Mean[#], Mean[onePolygon] ] < 10 &];
	partsFromOnePolygon = getConvexParts[onePolygon];
	partsFromNearbyPolygons = Flatten[Map[getConvexParts, manyPolygons], 1];
	For[pid = 1, pid<=Length[partsFromOnePolygon], pid++,
		part1 = partsFromOnePolygon[[pid]];
		For[pid2 = 1, pid2 <= Length[partsFromNearbyPolygons], pid2++,
			Print["starting"];
			Print[{pid, Length[partsFromOnePolygon], pid2, Length[partsFromNearbyPolygons] }];

			disjointSubResult = regionDisjointByArea[ Region[Polygon[part1]], Region[Polygon[ partsFromNearbyPolygons[[pid2]] ]] ];
			Print[{pid, Length[partsFromOnePolygon], pid2, Length[partsFromNearbyPolygons] }];
			Print["disjointSubResult: " <> ToString[disjointSubResult]];
			If[ disjointSubResult == False,
				Return[False];
			];
		];
	];
	Print["no false, returning True"];

	Return[True]
];

projection42 = Transpose[{{1, 0, 1, 0}, {0, 1, 0, 1}}];
flipDimensions = {
	{0, 0, 1, 0},
	{0, 0, 0, 1},
	{1, 0, 0, 0},
	{0, 1, 0, 0}
};

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

polygon2d = polygon4d.projection42;
polygon4d = N[polygon4d];
polygon2d = N[polygon2d];

polygonTemplates = <||>;
polygonTemplates4d = <||>;

rotate4d[angle_] := ArrayFlatten[{
	{RotationMatrix[angle], 0}, 
	{0, RotationMatrix[angle]}}
];

For[clock = 1, clock <= 12, clock++,
	rotationMatrix = RotationMatrix[2 Pi / 12 * clock];
	rotatedPolygon = polygon2d.rotationMatrix;

	rotationMatrix4d = rotate4d[2 Pi / 12 * clock];
	rotatedPolygon4d = polygon4d.rotationMatrix4d;
	If[ Mod[clock, 2] == 1, rotatedPolygon4d = rotatedPolygon4d.flipDimensions ];

	For[ pointId = 1, pointId <= Length[rotatedPolygon], pointId++,
		previousPointId = Mod[pointId - 1 -1, Length[rotatedPolygon]]+1;
		nextPointId = Mod[pointId + 1 -1, Length[rotatedPolygon]]+1;
		focusPoint = rotatedPolygon[[pointId]];
		previousEdge = rotatedPolygon[[pointId]] - rotatedPolygon[[previousPointId]];
		nextEdge = rotatedPolygon[[nextPointId]] - rotatedPolygon[[pointId]];
		edgeDirections = Map[getDirection, {previousEdge, nextEdge}];

		shiftedPolygon = move[rotatedPolygon, -focusPoint];
		shiftedPolygon4d = move[rotatedPolygon4d, -rotatedPolygon4d[[pointId]]];

		If[ !KeyExistsQ[ polygonTemplates, edgeDirections ],
			polygonTemplates[edgeDirections] = {};
			polygonTemplates4d[edgeDirections] = {}
		];
		AppendTo[polygonTemplates[edgeDirections], shiftedPolygon];
		AppendTo[polygonTemplates4d[edgeDirections], shiftedPolygon4d];
	];
];

(* For[keyId = 1, keyId <= Length[Keys[polygonTemplates]], keyId++,
	key = Keys[polygonTemplates][[keyId]];
	value = polygonTemplates[key];
	Print[{key, Length[value] }];
];
 *)

getColor[polygon_] := If[ Mod[ getDirection[polygon[[2]] - polygon[[1]]], 60 ] == 0, Black, Lighter[RandomColor[], 0.2] ];
(* getColor[polygon_] := RandomColor[]; *)

(* initialize *)
mergedTiles = { polygon2d };
mergedTiles4d = { polygon4d };
mergedPatchPolygon = mergedTiles[[1]];
mergedRegion = Region[ Polygon[ mergedPatchPolygon ]];
mergedGraphics = {};
AppendTo[mergedGraphics, {getColor[mergedTiles[[1]]], Polygon[ mergedTiles[[1]] ]} ];

stepLimit = 100;
stepId = 1;
graph = Graphics[ mergedGraphics , PlotRange -> {{-range, range}, {-range, range}} ];
Export["output/test/" <> filePrefix <> "_" <> IntegerString[stepId, 10, 3]<> ".png", graph ];

optimalWeights = {0.03236986736060123, 0.07799316476924252, -0.05911191295628166, -0.17176748426831062, -0.08708542852278012, -0.04146222358595754, 0.09564273392713142, 0.12009210847871328, 0.15915097318708082, 0.11352767411505187, 0.06790425612793533, -0.016777888519672258, 0.09587778350167393};
AppendTo[ optimalWeights, 1 - Total[optimalWeights]];

getCenter[tile_] := Mean[tile];
(* getCenter[tile_] := (tile[[3]] + tile[[11]])/2; *)
(* getCenter[tile_] := optimalWeights.tile; *)

(* Exit[]; *)

candidateDisjointCheck[candidate_, angles_, minAngleId_] := Module[
	{},
	candidateVertexId = Select[Range[Length[candidate]], Norm[candidate[[#]]] < epsilon &][[1]];
	candidateAngleId = Mod[candidateVertexId - 2, Length[candidate]] + 1;
	
	candidateEdges = getEdgesFromFace[candidate];
	candidateRelativeEdges = Map[#[[2]] - #[[1]] &, candidateEdges];
	candidateEdgeDirections = Map[getDirection, candidateRelativeEdges];
	candidateEdgeDirectionPairs = getEdgesFromFace[ candidateEdgeDirections ];
	candidateAngles = Map[#[[2]] - #[[1]]&, candidateEdgeDirectionPairs ];
	candidateAngles = Map[Mod[# + 180, 360]-180&, candidateAngles];

	If[verbose,
		Print["candidate ids"];
		Print[candidateAngleId];
		Print[candidateAngles];
	];
	
	disjointTest = True;
	For[nextAngleId = 1, nextAngleId <= Length[candidate], nextAngleId++,
		mergedAngle = angles[[ Mod[minAngleId + nextAngleId - 1, Length[angles]] + 1 ]];
		candidateAngle = candidateAngles[[ Mod[candidateAngleId - nextAngleId - 1, Length[candidateAngles]] + 1 ]];
		If[verbose,Print[{nextAngleId, mergedAngle, candidateAngle}]];
		If[mergedAngle + candidateAngle > 0, If[verbose,Print["break nextAngleId: " <> IntegerString[nextAngleId]]]; Break[]]; (* edges diverge, no need to track *)
		If[mergedAngle + candidateAngle == 0, If[verbose,Print["continue nextAngleId: " <> IntegerString[nextAngleId]]]; Continue[]]; (* edges match, continue to check *)
		If[mergedAngle + candidateAngle < 0, If[verbose,Print["set false nextAngleId: " <> IntegerString[nextAngleId]]]; disjointTest = False; Break[]; ]; (* overlapping *)
	];
	If[disjointTest == True,
		For[previousAngleId = 1, previousAngleId <= Length[candidate], previousAngleId++,
			mergedAngle = angles[[ Mod[minAngleId - previousAngleId - 1, Length[angles]] + 1 ]];
			candidateAngle = candidateAngles[[ Mod[candidateAngleId + previousAngleId - 1, Length[candidateAngles]] + 1 ]];
			If[verbose,Print[{previousAngleId, mergedAngle, candidateAngle}]];
			If[mergedAngle + candidateAngle > 0, If[verbose,Print["break previousAngleId: " <> IntegerString[previousAngleId]]]; Break[]]; (* edges diverge, no need to track *)
			If[mergedAngle + candidateAngle == 0, If[verbose,Print["continue previousAngleId: " <> IntegerString[previousAngleId]]]; Continue[]]; (* edges match, continue to check *)
			If[mergedAngle + candidateAngle < 0, If[verbose,Print["set false previousAngleId: " <> IntegerString[previousAngleId]]]; disjointTest = False; Break[]; ]; (* overlapping *)
		];
	];
	disjointTest
];

(* example:
{contact, p1, *start to keep* p2, p3, p4, n5, n4, n3, n2, n1*end of keep*}
{contact, n1, *start to keep* n6, p6 *end of keep*, p2, p1}

result:
{p2, p3, p4, n5, n4, n3, n2, n1,    n6, p6}
 *)

mergePolygons[polygon1_, polygon2_, contactPoint_] := Module[
	{},
	pids1 = Select[Range[Length[polygon1]], Norm[polygon1[[#]] - contactPoint] < epsilon& ];
	If[verbose, Print[pids1]];
	pids2 = Select[Range[Length[polygon2]], Norm[polygon2[[#]] - contactPoint] < epsilon& ];
	If[verbose, Print[pids2]];
	If[Length[pids1] != 1 || Length[pids2] != 1,
		Print["unexpected number of pids1 or pids2:"];
		Print[pids1];
		Print[pids2];
		Exit[];
	];
	pid1 = pids1[[1]];
	pid2 = pids2[[1]];
	rotatedPolygon1 = RotateLeft[polygon1, pid1 - 1];
	rotatedPolygon2 = RotateLeft[polygon2, pid2 - 1];

	startIndex1 = 1;
	endIndex1 = Length[rotatedPolygon1];
	startIndex2 = 1;
	endIndex2 = Length[rotatedPolygon2];

	For[nextPointId = 1, nextPointId <= Length[rotatedPolygon2], nextPointId++,
		pointMatching = ApproxSamePoint[ rotatedPolygon1[[nextPointId + 1 ]], rotatedPolygon2[[ Length[rotatedPolygon2] - nextPointId + 1 ]] ];
		If[pointMatching == False, Break[]];
	];
	If[verbose, Print["nextPointId: " <> IntegerString[nextPointId] ]];
	
	(* next point id = 3 *)

	For[previousPointId = 1, previousPointId <= Length[rotatedPolygon2], previousPointId++,
		pointMatching = ApproxSamePoint[ rotatedPolygon2[[previousPointId + 1 ]], rotatedPolygon1[[ Length[rotatedPolygon1] - previousPointId + 1 ]] ];
		If[pointMatching == False, Break[]];
	];
	If[verbose, Print["previousPointId: " <> IntegerString[previousPointId] ]];
	(* prevoius point id = 2 *)
	
	mergedPolygon = Join[
		rotatedPolygon1[[ Range[ nextPointId, Length[rotatedPolygon1] + 2 - previousPointId ] ]],
		rotatedPolygon2[[ Range[ previousPointId + 1, Length[rotatedPolygon2] - nextPointId + 1 ] ]]
	];
	If[verbose || True, Print["mergedPolygon length: " <> IntegerString[Length[mergedPolygon]] ]];
	mergedPolygon
];

For[stepId = 2, stepId <= stepLimit, stepId++,
	Print[];
	Print["Starting stepId " <> IntegerString[stepId]];

	(* originalRegionPoints = getPoints[mergedRegion];
	mergedRegionPoints = { originalRegionPoints[[1]] };
	For[pointId = 2, pointId <= Length[originalRegionPoints], pointId++,
		edgeLength = Norm[ originalRegionPoints[[pointId - 1]] - originalRegionPoints[[pointId]] ];
		If[ Abs[ edgeLength - 2 ] < epsilon, 
			AppendTo[mergedRegionPoints, originalRegionPoints[[pointId]]],
			If[ Abs[ edgeLength - 4 ] < epsilon, 
				AppendTo[mergedRegionPoints, (originalRegionPoints[[pointId-1]] + originalRegionPoints[[pointId]])/2 ];
				AppendTo[mergedRegionPoints, originalRegionPoints[[pointId]]],
				Print["Unexpected edge length: " <> ToString[edgeLength]];
				Exit[];
			];
		];
	]; *)

	(* edges = getEdgesFromFace[mergedPatchPolygon]; *)
	mergedRegionPoints = Join[ mergedPatchPolygon, mergedPatchPolygon[[{1}]] ];
	edges = getEdgesFromPoints[mergedRegionPoints];
	relativeEdges = Map[#[[2]] - #[[1]] &, edges];
	Print[Tally[Round[Norm/@relativeEdges, epsilon]]];
	edgeDirections = Map[getDirection, relativeEdges];
	edgeDirectionPairs = getEdgesFromFace[ edgeDirections ];
	angles = Map[#[[2]] - #[[1]]&, edgeDirectionPairs ];
	angles = Map[Mod[# + 180, 360]-180&, angles];

	If[Total[angles] != 360,
		Print["unexpected total angles: " <> IntegerString[Total[angles]] <> " Processing terminated. StepId: " <> IntegerString[stepId]];
		Exit[];
	];

	(* Print[angles]; *)
	negativeAngleIds = Select[Range[Length[angles]], angles[[#]] < 0 &];
	(* negativeAngleIds = Select[Range[Length[angles]], angles[[#]] == - 60 &]; *)
	(* Print[negativeAngleIds]; *)
	minNormRefPoint = {0, 0};
(* 	If[stepId >= 59,
		minNormRefPoint = {0, -15};
	]; *)
	negativeAnglePointNorms = Map[ Norm[mergedRegionPoints[[# + 1]] - minNormRefPoint]&, negativeAngleIds ];
	minNorm = Min[negativeAnglePointNorms];

	(* pick point closest to origin *)
	minAngleId = Select[ Range[Length[angles]], Norm[mergedRegionPoints[[# + 1]] - minNormRefPoint] < minNorm + epsilon &  ][[1]];

	(* random *)
	(* minAngleId = RandomChoice[negativeAngleIds]; *)
	edgeDirectionPair = edgeDirectionPairs[[minAngleId]];
	minAnglePoint = mergedRegionPoints[[minAngleId + 1]];
	matchingDirectionPair = Reverse[Map[Mod[# + 180, 360]&, edgeDirectionPair ]];

	Print[minAnglePoint];
	Print[edgeDirectionPair];
	Print[matchingDirectionPair];

	If[verbose,
		Print["merged region angles"];
		Print[minAngleId];
		Print[angles];
	];
	
	If[!KeyExistsQ[polygonTemplates[matchingDirectionPair]],
		Print["matchingDirectionPair not found. Processing terminated. StepId: " <> IntegerString[stepId]];
		Print[matchingDirectionPair];
		Exit[]
	];

	matchingCandidates = polygonTemplates[matchingDirectionPair];
	disjointCandidates = {};

	matchingCandidates4d = polygonTemplates4d[matchingDirectionPair];
	disjointCandidates4d = {};

	Print["matchingCandidates count " <> IntegerString[ Length[matchingCandidates] ]];

	For[cid = 1, cid <= Length[matchingCandidates], cid++,
		candidate = matchingCandidates[[cid]];
		
		matchingPolygon = move[candidate, minAnglePoint];
		matchingRegion = Region[Polygon[matchingPolygon]];

		If[verbose,Print["testing region disjoint testing " <> IntegerString[stepId] <> " " <> IntegerString[cid] ];];
(* 		If[(stepId == 45 && cid == 2), 
			Print[matchingRegion];
			Print[mergedRegion];
			regionDisjoint = regionDisjointByArea[mergedRegion, matchingRegion ],
			regionDisjoint = regionDisjointByArea[matchingRegion, mergedRegion];
		];
 *)
		(* regionDisjoint = regionDisjointByArea[matchingRegion, mergedRegion]; *)
		(* Print["original disjoint test: " <> ToString[regionDisjoint]]; *)
		(* regionDisjoint = regionDisjointByConvexParts[matchingPolygon, mergedTiles]; *)

(* 		If[ regionDisjoint != disjointTest,
			Print["old and new disjoint tests do not match"];
			Print[{regionDisjoint, disjointTest}];
			(* Exit[]; *)
		];
 *)(* 
		If[stepId == 38,
			regionDisjoint = True;
		];
 *)		
 		regionDisjoint = candidateDisjointCheck[candidate, angles, minAngleId];

 		If[verbose, Print["finished region disjoint testing " <> ToString[regionDisjoint] <> " " <> IntegerString[stepId] <> " " <> IntegerString[cid]];];

		If[regionDisjoint,
			AppendTo[disjointCandidates, matchingPolygon];

			candidate4d = matchingCandidates4d[[cid]];

			all4dPoints = Flatten[mergedTiles4d, 1];
			minAnglePoints4d = Union[ Select[all4dPoints, Norm[ #.projection42 - minAnglePoint] < Sqrt[epsilon] & ], SameTest -> ApproxSamePoint];
			If[Length[minAnglePoints4d] != 1,
				Print["Fail to identify 4d points corresponding to minAnglePoint. Abort. Number of 4d candidates: " <> IntegerString[Length[minAnglePoints4d]] ];
				Print[minAnglePoint];
				Print[minAnglePoints4d];
			
				Print[edgeDirectionPair];
				Print[matchingDirectionPair];

				Print["tile by tile"];
				Print[Length[mergedTiles4d]];

				For[tid = 1, tid <= Length[mergedTiles4d], tid++,
					Print[tid];
					tile4d = mergedTiles4d[[tid]];
					inTilePoints4d = Union[ Select[tile4d, Norm[ #.projection42 - minAnglePoint] < Sqrt[epsilon] & ], SameTest -> ApproxSamePoint];
					Print[inTilePoints4d];
				];

				Exit[];
			];

			minAnglePoint4d = minAnglePoints4d[[1]];
			matchingPolygon4d = move[candidate4d, minAnglePoint4d];
			AppendTo[disjointCandidates4d, matchingPolygon4d];
		];
	];

	Print["disjointCandidates count " <> IntegerString[ Length[disjointCandidates] ]];

	If[Length[disjointCandidates] == 0,
		Print["All candidate regions overlap with merged region. Processing terminated. StepId: " <> IntegerString[stepId]];
		Exit[]
	];

	(* mergedTileCenters = Map[getCenter, mergedTiles4d]; *)
	(* mergedTileCentersProjection = mergedTileCenters.Transpose[orthProjection42]; *)

	merged4dAllPoints = Flatten[mergedTiles4d, 1];
	mergedPointsProjected = merged4dAllPoints.Transpose[orthProjection42];
	boundingRegion = BoundingRegion[mergedPointsProjected, "MinDisk"];

	(* boundingRegion = BoundingRegion[mergedTileCentersProjection, "MinDisk"]; *)
	diskCenter = RegionCentroid[boundingRegion];
	(* Print["mergedTileCentersProjection"]; *)
	(* Print[mergedTileCentersProjection]; *)
	radiusMinDisk = Max[Map[Norm[# - diskCenter]&, mergedPointsProjected]];
	Print["diskCenter and radius: " <> ToString[{diskCenter, radiusMinDisk}]];

	(* merged4dCenter = Mean[mergedTileCenters]; *)

	(* merged4dCenter = Mean[Flatten[mergedTiles4d, 1]]; *)
	(* merged4dCenter = {-0.487012987012987, 1.602709351159513, 0.3374124949809501, 1.0422077922077921}; *)
	(* {-0.337729, 0.00551049} *)
	(* Print["merged4dCenter: " <> ToString[merged4dCenter]]; *)
	(* merged4dCenterProjection = merged4dCenter.Transpose[orthProjection42]; *)
	merged4dCenterProjection = diskCenter;
	(* merged4dCenterProjection = {-0.32, -0.1}; *)
	(* merged4dCenterProjection = {0.609717, -0.727913}; *)
	(* merged4dCenterProjection = {-0.337729, 0.00551049}; *)
	Print["merged4dCenterProjection: " <> ToString[merged4dCenterProjection]];
	(* merged4dCenter = {0, 0, 0, 0}; *)

(* 	disjointCandidates4dCenterOffset = Map[(getCenter[#].Transpose[orthProjection42])&, disjointCandidates4d];
	Print["disjointCandidates4dCenterOffset"];
	Print[disjointCandidates4dCenterOffset];
 *)
	(* Print[disjointCandidates4dCenterOffset//Length]; *)
	(* Print[disjointCandidates4dCenterOffset.Transpose[orthProjection42]]; *)
	(* orthErrors = Map[ Norm[# - merged4dCenterProjection]&, disjointCandidates4dCenterOffset ]; *)
	orthErrors = {};
	For[cid = 1, cid <= Length[disjointCandidates4d], cid++,
		candidate4d = disjointCandidates4d[[cid]];
		projected2d = candidate4d.Transpose[orthProjection42];
		max2dDistance = Max[Map[Norm[# - merged4dCenterProjection] &, projected2d]];
		AppendTo[orthErrors, max2dDistance];
	];
	Print["orthErrors"];
	Print[orthErrors];
	candidatePick = Select[ Range[Length[orthErrors]], orthErrors[[#]] < Min[orthErrors] + epsilon & ][[1]];

	(* naive random pick *)
	If[filterBy4d == False,
		candidatePick = RandomInteger[ {1, Length[disjointCandidates]} ];
	];

	(* override *)
(* 	If[stepId == 38,
		candidatePick = 5;
	];
 *)

	Print["stepId: " <> IntegerString[stepId]
		<> " Number of candidates: " <> IntegerString[Length[matchingCandidates]] 
		<> " Number of Disjoint candidates: " <> IntegerString[Length[disjointCandidates]]
		<> " Picking #" <> IntegerString[candidatePick]
		<> " orth error: " <> ToString[orthErrors[[candidatePick]] ]
	];

	matchingPolygon = disjointCandidates[[candidatePick]];
	matchingPolygon4d = disjointCandidates4d[[candidatePick]];
	matchingRegion = Region[Polygon[matchingPolygon]];

	(* need my own region union function *)
	Print["starting region union of selected tile"];
	(* mergedRegion = RegionUnion[matchingRegion, mergedRegion]; *)
	mergedPatchPolygon = mergePolygons[mergedPatchPolygon, matchingPolygon, minAnglePoint];

	Print["Finished region union of selected tile"];
	AppendTo[mergedTiles, matchingPolygon];
	AppendTo[mergedTiles4d, matchingPolygon4d];
	AppendTo[mergedGraphics, {getColor[matchingPolygon], Polygon[ matchingPolygon ]} ];
	Print["Completing step id " <> IntegerString[stepId] ];
	graph = Graphics[ mergedGraphics , PlotRange -> {{-range, range}, {-range, range}} ];
	Export["output/test/" <> filePrefix <> "_" <> IntegerString[stepId, 10, 3]<> ".png", graph ];
];


(* Print[ polygonTemplates[matchingDirectionPair] ]; *)


(* 
magick convert -quiet +repage -alpha remove -loop 0 -delay 30 .\output\test\*.png -delay 300 .\output\test\4d_plane_036.png grow.gif
*)