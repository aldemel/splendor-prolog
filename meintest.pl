% (list of players, number of runs, number of player1 winning)
testRuns([_,_],0).
testRuns(Players, NoGames):-
	NoGames > 0,
	show(0, 'NoGames ~w ~n', NoGames),
	setVerbose(0),
	not(runGame(Players)),
	NoGamesLeft is NoGames - 1,
	testRuns(Players, NoGamesLeft).
