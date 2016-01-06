%%% Format of actions:
%	Action = buyCard(CardId)
%	Action = reserveCard(ReservedCard)
%	Action = getGems(RandGems, BackGems)





% calcState(Player, Oponents, StateProxy, Action):-
calcState(Player, _, StateProxy, Action):-
	call(StateProxy, Player, score, Score),
	%show(0, '~n MyScore ~w ~n', Score),
	call(StateProxy, Player, gems, MyGems),
	%show(0, 'MyGems ~w ~n', [MyGems]),
	call(StateProxy, Player, bonuses, MyBonuses),
	%show(0, 'MyBonuses ~w ~n', [MyBonuses]),
	call(StateProxy, Player, reserves, Reserves),
	%show(0, 'Reserves ~w ~n', [Reserves]),
	call(StateProxy, game, cards, Cards),
	%show(0, 'Cards ~w ~n', [Cards]),
	call(StateProxy, game, nobles, AvNobles),
	%show(0, 'AvNobles ~w ~n', [AvNobles]),
	call(StateProxy, game, tokens, Tokens),
	show(1, 'Tokens ~w ~n', [Tokens]),
	append(Reserves, Cards, AllCards),
	canBuyCards(MyGems, MyBonuses, AllCards, CanBuyCards),
	length(CanBuyCards,HowMany),
	show(1, 'HowMany CanBuyCards ~w ~n', HowMany),
	
	calcAndStoreBuyStates(MyGems, MyBonuses, Score, AvNobles, AllCards, CanBuyCards),

	possibleTakeGems(Tokens, MyGems, MyBonuses, Score, AvNobles, AllCards),

	possibleReserve(Tokens, Score, MyBonuses, MyGems, AvNobles, Cards, Reserves),

	%listing(stateValue),
	%write('finish'),
	(
		findWinningmove(Action)
		%,write('foundwinning')
		;
		%write('lets sort'),
		sortByStateValue([Action|_])
		%sortByStateValue([Action|Sorted])
		% filter loosing moves
		% sort by stateValue
		% if not empty choose, if empty random
	),
	retractall(stateValue(_,_,_)).



findWinningmove(Action):-
	stateValue(Action, _, win).

filterLoosingMoves:-
	retractall(stateValue(_,_,loose)).

sortByStateValue(Sorted):-
	findall(Action, stateValue(Action, _, neutral), Actions),
	% sort descending
	predsort(compareMove, Actions, Sorted).

compareMove(Pred, Action1, Action2):-
	% only use this method if none of the moves can win
	% dont consider loosing moves if possible
	stateValue(Action1, Value1, _),
	stateValue(Action2, Value2, _),
	compValue(Pred, Value1, Value2).

compValue(>, Value1, Value2):- Value1 < Value2.
compValue(<, _, _).








calcAndStoreBuyStates(_, _, _, _, _, []).
calcAndStoreBuyStates(MyGems, MyBonuses, MyScore, AvNobles, AllCards, [CardToBuyID|Left]):-
	calcAndStoreSingleBuyState(MyGems, MyBonuses, MyScore, AvNobles, AllCards, CardToBuyID),
	calcAndStoreBuyStates(MyGems, MyBonuses, MyScore, AvNobles, AllCards, Left).

calcAndStoreSingleBuyState(MyGems, MyBonuses, MyScore, AvNobles, AllCards, CardToBuyID):-
	% Update Gems
	%write('start '),
	card(CardToBuyID, RequiredGems-BonusColor-Points),
	%write('gotcard '),
	%trace,
	maplist(subtractTillZero, RequiredGems, MyBonuses, SubtractedBonus),
	%write('subtractTillZero '),
	%trace,
	maplist(simpleMinus, MyGems, SubtractedBonus, GemsInterm),
	%nodebug,
	% if less then 0, make it zero and subtract from gold
	%write('subtract '),
	foldl(addMinusOcc, GemsInterm, 0, GoldNeeded),
	%write('addMinusOcc '),
	maplist(subtractTillZero, [0,0,0,0,0,GoldNeeded],GemsInterm, GemsLeft),
	%write('subtractTillZero \n'),
	
	% Update Score
	NewScore is MyScore + Points,
	%write('NewScore '),
	% Update Bonuses
	addBonus(BonusColor, MyBonuses, NewBonuses),
	%write('Update Bonuses '),
	% Update CardList
	exclude(equal(CardToBuyID), AllCards, CardsLeft),
	%write('exclude '),
	
	% evaluate state!
	% pass: GemsLeft, NewBonuses, NewScore, CardsLeft
	calcStateValue(NewScore, NewBonuses, GemsLeft, AvNobles, CardsLeft, STATEVALUE, WinLoose),
	%write('eval '),
	Action = buyCard(CardToBuyID),
	%write('Action '),
	assert(stateValue(Action, STATEVALUE, WinLoose))
	%,write('asserted\n')
	.

addBonus(1, [B|Bonuses], [New|Bonuses]):- New is B + 1.
addBonus(N, [B|Bonuses], [B|NewBonuses]):- M is N - 1, addBonus(M, Bonuses, NewBonuses).

addMinusOcc(A, B, C):-
	(
		A < 0,
		C is B - A
		;
		C = B
	).

subtractTillZero(A,B,C):-
	(
		A >=B,
		C is A - B
		;
		C =0
	).

simpleMinus(A,B,C):- C is A - B.
equal(A,A).


% possible combinations of possibleTakeTwoGems and possibleTakeThreeGems. if no other move possible, try take less

% list of Gems to take
:- dynamic takeGemComb/1.

possibleTakeGems(Tokens, MyGems, MyBonuses, MyScore, AvNobles, AllCards):-
	possibleTakeThreeGems(1, Tokens),
	possibleTakeTwoGems(1, Tokens),

	%% TODO: check if taking gems like this is even possible!
	%% TODO: check if too many gems!
	%listing(takeGemComb),
	!,
	%show(0, '~n MyGems ~w ~n', [MyGems]),
	%show(0, '~n Tokens ~w ~n', [Tokens]),

	evAndstoreTakeGemStates(MyScore, MyBonuses, MyGems, AvNobles, AllCards)
	.


% (Position, ...)
possibleTakeThreeGems(_, GoldAndTwoMore):- length(GoldAndTwoMore, 3).
possibleTakeThreeGems(N, [First|Tokens]):-
	M is N +1,
	(
		First > 0,
		possibleTakeThreeGems(M, N, Tokens)
		;
		true
	),
	possibleTakeThreeGems(M, Tokens).

possibleTakeThreeGems(_, _, GoldAndOneMore):- length(GoldAndOneMore, 2).
possibleTakeThreeGems(N, First, [Second|Tokens]):-
	M is N+1,
	(
		Second > 0,
		possibleTakeThreeGems(M, First, N, Tokens)
		;
		true	
		),
	possibleTakeThreeGems(M, First, Tokens).

possibleTakeThreeGems(_, _, _, OnlyGold):- length(OnlyGold, 1).
possibleTakeThreeGems(N, First, Second, [Third|Tokens]):-
	(
		Third > 0,
		assert(takeGemComb([First, Second, N]))
		;
		true
	),
	M is N +1,
	possibleTakeThreeGems(M, First, Second, Tokens).

possibleTakeTwoGems(_, OnlyGold):- length(OnlyGold, 1).
possibleTakeTwoGems(N, [First|Tokens]):-
	(
		First > 3,
		assert(takeGemComb([N, N]))
		;
		true
	),
	M is N +1,
	possibleTakeTwoGems(M,Tokens).


evAndstoreTakeGemStates(MyPP, MyBonuses, MyGems, AvNobles, AllCards):-
	(
		retract(takeGemComb(Comb)),
		%show(0, '~n Comb ~w ~n', [Comb]),
		%show(0, '~n MyGems ~w ~n', [MyGems]),
		adjustMyGems(Comb, MyGems, NewGems),
		%write('adjusted my gems\n'),
		calcStateValue(MyPP, MyBonuses, NewGems, AvNobles, AllCards, STATEVALUE, WinLoose),
		%write('calculated\n'),
		adjustGemFormat(Comb, [0,0,0,0,0,0], Take),
		%write('adjusted format\n'),
		assert(stateValue(getGems(Take, [0,0,0,0,0,0]), STATEVALUE, WinLoose)),
		%write('asserted\n'),
		evAndstoreTakeGemStates(MyPP, MyBonuses, MyGems, AvNobles, AllCards)
		;
		%write('just true\n'),
		true
	).

adjustGemFormat([], Same, Same).
adjustGemFormat([First|Tail], Old, New):-
	adjustGemFormat(Tail, Old, Temp),
	addBonus(First, Temp, New)
	.

adjustMyGems([], Same, Same).
adjustMyGems([Add|Tokens], OldGems, NewGems):-
	adjustMyGems(Tokens, OldGems, TempGems),
	addBonus(Add, TempGems, NewGems)
	.








%TODO: which one shall be reserved?
possibleReserve([_,_,_,_,_,Gold], MyPP, MyBonuses, MyGems, AvNobles, OpenCards, Reserves):-
	(
		length(Reserves, L),
		L < 3,	
		(
			Gold > 0,
			addBonus(6, MyGems, NewGems)
			;
			NewGems = MyGems
		),
		append(Reserves, OpenCards, AllCards),
		calcStateValue(MyPP, MyBonuses, NewGems, AvNobles, AllCards, _, WinLoose),
		%calcStateValue(MyPP, MyBonuses, NewGems, AvNobles, AllCards, STATEVALUE, WinLoose),
		% TODO: STORE STATE VALUE!
		length(OpenCards, CardsLength),
		CardsLength1 is CardsLength+1,
		random(1, CardsLength1, ReserveId),
		nth1(ReserveId, OpenCards, ReservedCard),
		Action = reserveCard(ReservedCard),
		% TODO: USE REAL STATEVALUE
		assert(stateValue(Action, 1, WinLoose))
		;
		true
	).