BeginPackage["Scrabble`"]

UpdateTileBag::usage = "Updates the tile bag association after playing a bingo, accounting for blanks used if necessary.";
BattleshipNew::usage = "Identify squares used up by a 'bingo' placed at '(row, col)'.";
ForbiddenSquaresNew::usage = "Identify squares in the 'forbidden zones' around already played words.";
FindBingoSpots::usage = "Find spots to play the next bingo through previous horizontal/vertical turns.";
FindViablePlays::usage = "Find all possible locations to play a bingo given a set of previous turns.";
CanFormWord::usage = "Check if a word can be formed from the given tiles.";
CullBingoList::usage = "Cull the list of possible bingos based on the tiles remaining.";
ScoringBonus::usage = "Apply letter and word multipliers to calculate the total score of a turn.";
ScoreTurn::usage = "Calculate the score of a given turn.";

Begin["`Private`"]

UpdateTileBag[tileBag_Association, bingo_String /; StringLength[bingo] == 8, blanksRemaining_Integer, overlap_Association] :=
Module[
	{
		modifiedBingo, 
		overlapTile = overlap["tile"],
		overlapIndex = overlap["index"],
		response, 
		adjustedIndices
	},
	modifiedBingo = StringDrop[bingo, {overlapIndex}];
	response = Association[UpdateTileBag[tileBag, modifiedBingo, blanksRemaining]];
	If[
		response["success"] && response["blanks"] =!= Null,
		response = MapAt[Association, response, Key["blanks"]];
		adjustedIndices = Map[If[# < overlapIndex, #, # + 1] &, response["blanks"]["indices"]];
		response["blanks"]["indices"] = adjustedIndices;
	];
	Normal[MapAt[Normal, response, Key["blanks"]]]
];


UpdateTileBag[tileBag_Association, bingo_String /; StringLength[bingo] == 7, blanksRemaining_Integer] :=
Module[
	{
		tilesInBingo = Characters[bingo], charCounts, newTileBag, blanksNeeded,
		blankTile, blankIndices
	},
	charCounts = Counts[tilesInBingo];
	newTileBag=
	Merge[
		{
			tileBag,
			AssociationMap[(tileBag[#] - charCounts[#]) &, tilesInBingo ]
		},
		Last
	];
	blanksNeeded=
	Abs[
		Total[
			Select[
				Values[newTileBag],
				# < 0 &
			]
		]
	];
	Which[
		blanksNeeded == 0,
		Return[
			{
				"success" -> True,
				"tileBag" -> Normal[newTileBag],
				"blanks" -> Null
			}
		],
		blanksNeeded =!= 0 && blanksNeeded <= blanksRemaining && Min[Values[newTileBag]] >= -blanksRemaining,
		newTileBag["?"] = newTileBag["?"] - blanksNeeded;
		blankTile = First[Flatten[KeyValueMap[Table[#1, -#2] &, Select[newTileBag, # < 0 &]]]]; (* Only one blank allowed for now! *)
		blankIndices = Position[tilesInBingo, blankTile][[All, 1]];
		Return[
			{
				"success" -> True,
				"tileBag" -> Normal[newTileBag] /. x_Integer /; x < 0 :> 0,
				"blanks" -> 
				Normal[
					<|
					"tile" -> blankTile,
					"indices" -> blankIndices
					|>
				]
			}
		],
		True,
		Return[
			{
				"success" -> False,
				"error" -> "CANNOT-FORM-WORD"
			}
		]
	]
];

BattleshipNew[spot_ /; spot["direction"] == "H"] :=
Module[
	{row, col, length = StringLength[spot["bingo"]]},
	{row, col} = {spot["row"], spot["col"]};
	Table[
		{row, col + i},
		{i, 0, length - 1}
	]
];
BattleshipNew[spot_ /; spot["direction"] == "V"] :=
Module[
	{row, col, length = StringLength[spot["bingo"]]},
	{row, col} = {spot["row"], spot["col"]};
	Table[
	{row + i, col},
	{i, 0, length - 1}
	]
];

ForbiddenSquaresNew[turn_Association /; turn["direction"] == "H"] :=
Module[
	{row, col, length = StringLength[turn["bingo"]]},
	{row, col} = {turn["row"], turn["col"]};
	Join[
		Table[{row, col + i}, {i, -1, length}],
		Table[{row - 1, col + i}, {i, 0, length - 1}],
		Table[{row + 1, col + i}, {i, 0, length - 1}]
	]
];
ForbiddenSquaresNew[turn_Association /; turn["direction"] == "V"] :=
Module[
	{row, col, length = StringLength[turn["bingo"]]},
	{row, col} = {turn["row"], turn["col"]};
	Join[
		Table[{row + i, col}, {i, -1, length}],
		Table[{row + i, col - 1}, {i, 0, length - 1}],
		Table[{row + i, col + 1}, {i, 0, length - 1}]
	]
];

FindBingoSpots[bingo_String, turn_Association /; turn["direction"] == "H"] :=
Module[
	{
	word = turn["bingo"], row = turn["row"], col = turn["col"], 
	id = turn["id"], overlaps
	},
	overlaps = Intersection[Characters[word], Characters[bingo]];
	Flatten[
		Map[
			With[
				{
					toTile = StringPosition[word, #][[All, 1]] - 1, 
					retrace = StringPosition[bingo, #][[All, 1]] - 1 (* Gives the 0-based indices of the overlap tile '#' in the bingo string. *)
				},
				Flatten[
					Table[
						<|
							"bingo" -> bingo, "row" -> row - r, "col" -> col + t, "direction" -> "V", "playThroughTurn" -> id, 
							"overlap" -> 
							<|
								"tile" -> #, "index" -> r + 1
							|>
						|>,
						{r, retrace}, {t, toTile}
					],
					1
				]
			] &,
			overlaps
		],
		1
	]
];
FindBingoSpots[bingo_String, turn_Association /; turn["direction"] == "V"] :=
Module[
	{
		word = turn["bingo"], row = turn["row"], col = turn["col"], 
		id = turn["id"], overlaps
	},
	overlaps = Intersection[Characters[word], Characters[bingo]];
	Flatten[
		Map[
			With[
				{
					toTile = StringPosition[word, #][[All, 1]] - 1,
					retrace = StringPosition[bingo, #][[All, 1]] - 1 (* Gives the 0-based indices of the overlap tile '#' in the bingo string. *)
				},
				Flatten[
					Table[
						<|
							"bingo" -> bingo, "row" -> row + t , "col" -> col - r, "direction" -> "H", "playThroughTurn" -> id, 
							"overlap" -> 
							<|
								"tile" -> #, "index" -> r + 1
							|>
						|>,
						{r, retrace}, {t, toTile}
					],
					1
				]
			] &,
			overlaps
		],
		1
	]
];

(* For every turn in 'turns' I need the bingo, col, row, direction, and overlap info. *)
FindViablePlays[bingo_String, turns_List, currentTileBag_Association] :=
Module[
	{
		plays = {}, spots, avoidCols, playDown, avoidRows, playAcross, playsWithTileBag, playsWithForbiddenSquares, validPlays
	},
	plays =
	Flatten[
		Map[
			If[
				#["direction"] === "H",
				spots = FindBingoSpots[bingo, #];
				avoidCols = Select[turns, #["direction"] == "V" &][[All, "col"]];
				playDown =
				Select[
					spots,
					! MemberQ[avoidCols, #["col"]] && #["row"] <= 7 && #["row"] >= 0 &
				];
				Join[plays, playDown],
				spots = FindBingoSpots[bingo, #];
				avoidRows = Select[turns, #["direction"] == "H" &][[All, "row"]];
				playAcross =
				Select[
					spots,
					! MemberQ[avoidRows, #["row"]] && #["col"] <= 7 && #["col"] >=	0 &
				];
				Join[plays, playAcross]
			] &,
			turns
		],
		1
	];
	playsWithTileBag =
	Map[
		Join[
			#,
			With[
				{possibleBingo = #["bingo"], overlap = #["overlap"]},
				{
					response =
					Enclose[
						ConfirmMatch[
							With[
								{
									blanksIntervene = If[Length[turns] < 12, 0, Min[currentTileBag["?"], 1]]
								},
								UpdateTileBag[
									currentTileBag, 
									possibleBingo, 
									blanksIntervene,
									overlap
								]
							],
							{"success" -> True, "tileBag" -> _List, "blanks" -> _List | Null}
						]
					]
				},
				<|
					"tileBag" -> 
					If[
						FailureQ[response], 
						$Failed, 
						Association[response]["tileBag"]
					],
					"tilesLeft" -> 
					If[
						FailureQ[response], 
						$Failed, 
						Total[Values[Association[response]["tileBag"]]]
					],
					"blanks" -> 
					If[
						FailureQ[response], 
						$Failed, 
						Association[response]["blanks"]
					]
				|>
			]
		] &,
	plays
	];
	playsWithForbiddenSquares =
	Map[
		With[
			{play = #, playThroughTurn = #["playThroughTurn"]},
			Append[
				play,
				"forbiddenSquares" ->
				Flatten[
					Map[
						With[
							{turn = #},
							If[
								turn["id"] == playThroughTurn,
								BattleshipNew[turn],
								ForbiddenSquaresNew[turn]
							]
						] &,
					turns
					],
				1
				]
			]
		] &,
		playsWithTileBag
	];
	validPlays =
	Select[
		playsWithForbiddenSquares,
		ListQ[#["tileBag"]] && Length[Intersection[BattleshipNew[#], #["forbiddenSquares"]]] == 1 &
	];
	If[
		Length[validPlays] > 0,
		Return[
			{
				"success" -> True,
				"viablePlays" -> Normal[KeyDrop[validPlays, {"forbiddenSquares", "playThroughTurn"}]]
			}
		],
		Return[
			{
				"success" -> False,
				"error" -> "NO-VIABLE-PLAYS"
			}
		]
	]
];

CanFormWord[tiles_String, word_String] := AllTrue[Tally[Characters[word]], (Count[Characters[tiles], #[[1]]] >= #[[2]]) &];

CullBingoList[bingoList_List, tiles_String, blanks_Integer /; blanks < 2] :=
If[
	blanks == 0,
	Flatten[
		Table[
			Select[
				bingoList,
				CanFormWord[StringJoin[tiles, overlapTile], #] &
			],
			{overlapTile, ToUpperCase[Alphabet[]]}
		]
	],
	Flatten[
		Table[
			Select[
				bingoList,
				CanFormWord[StringJoin[tiles, overlapTilePlusBlank], #] &
			],
			{overlapTilePlusBlank, StringJoin @@@ Subsets[ToUpperCase[Alphabet[]], {2}]}
		]
	]
];

ScoringBonus[scoringInfo_List] :=
Module[
	{letterMultiply, totalLetterScore, wordMultipliers, wordMultiply},
	letterMultiply =
	Map[
		With[
			{points = #["points"], bonus = #["bonus"]},
			Switch[
				bonus,
				"DL", points * 2,
				"TL", points * 3,
				_, points
			]
		] &,
		scoringInfo
	];
	totalLetterScore = Total[letterMultiply];
	wordMultipliers = Cases[scoringInfo[[All, "bonus"]], "DW" | "TW"];
	wordMultiply =
	Fold[
		Switch[
			#2,
			"DW", #1 * 2,
			"TW", #1 * 3,
			_, #1
		] &,
		totalLetterScore,
		wordMultipliers
	];
	wordMultiply + 50	(* 50 point bingo bonus *)
];

ScoreTurn[turn_Association, blankPositions_List:{}] :=
Module[
	{	
		word = turn["bingo"], row = turn["row"], col = turn["col"],
		scoringInfo, score, blanksIndices
	},
	blanksIndices = 
	Which[
		turn["blanks"] === Null, 
		{},
		MemberQ[turn["blanks", "indices"], turn["overlap", "index"]],
		DeleteElements[turn["blanks", "indices"], 1 -> {turn["overlap", "index"]}],
		True,
		turn["blanks", "indices"]
	];
	scoringInfo = 
	Which[
		turn["direction"] === "H",
		MapIndexed[
			<|
				"tile" -> #1,
				"points" -> If[MemberQ[blanksIndices, First[#2]], 0, tiles[#1]["Points"]],
				"bonus" -> board[[row + 1, col + First[#2]]]
			|> &,
			Characters[word]
		],
		turn["direction"] === "V",
		MapIndexed[
			<|
				"tile" -> #1,
				"points" -> If[MemberQ[blanksIndices, First[#2]], 0, tiles[#1]["Points"]],
				"bonus" -> board[[row + First[#2], col + 1]]
			|> &,
			Characters[word]
		]
	];
	If[
		turn["overlap"] =!= Null,
		With[
			{
				overlapIndex = turn["overlap", "index"]
			},
			{
				overlapPos = 
				If[
					turn["direction"] === "H", {row, col + overlapIndex - 1}, {row + overlapIndex - 1, col}
				]
			},
			scoringInfo = 
			ReplacePart[
				scoringInfo, 
				{
					{overlapIndex, "bonus"} -> "SL", 
					{overlapIndex, "points"} -> If[MemberQ[blankPositions, overlapPos], 0, scoringInfo[[overlapIndex, "points"]]]
				}
			]
		],
		Nothing
	];
	score = 
	Enclose[
		ConfirmMatch[
			ScoringBonus[scoringInfo],
			_Integer
		]
	];
	Return[
		If[
			FailureQ[score],
			{
				"success" -> False,
				"error" -> "SCORING-ERROR"
			},
			{
				"success" -> True,
				"score" -> score
			}
		]
	]
];

End[]

EndPackage[]