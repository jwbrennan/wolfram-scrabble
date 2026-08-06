With[
	{
		kernelDir =
		FileNameJoin[
			{PacletObject["PerfectScrabbleGames"]["Location"], "Kernel"}
		]
	},
	Map[
		Get[FileNameJoin[{kernelDir, #}]] &,
		{
			"Utilities.wl",
			"PriorVersions.wl", 
			"Scrabblegorithm.wl",
			"APIFunctions.wl",
			"Auth.wl"
		}
	]
];


(*https://www.reddit.com/r/scrabble/comments/my5tie/the_419_words_erased_from_csw/*)