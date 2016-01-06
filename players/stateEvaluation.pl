% Parameter: 



% winloose?: loose/win/neutral
% #prestigePoints: int
% Bonusvalues: double
% gemValues: double
% alles in einer liste [winloose, pp, bonusval,gemval, anything else?]

%bonusvalue-funktion:
%	brauche ich das für noble
%		SEHR wichtig, wenn ich damit den nobel in dieser runde bekomme!
%		Noch besser wenn ich damit gewinne!
%	habe ich schon viele andere bonusse von dieser farbe?
%	hilft mir das eine andere offene (und wertvolle) karte zu kaufen? (könnte loopen zwischen den karten, aufpassen!)
%--Bonusvalue = PAR1*NobleValue + PAR2*balanceBonus


% TODO: schneller machen indem man nicht weiter berechnet sobald es ein winning state ist!
calcStateValue(MyScore, MyBonuses, MyGems, AvNobles, AllCards, STATEVALUE, WinLoose):-
		%write('start calcval\n'),
	nobleEarned(MyBonuses, AvNobles, MyScore, NewScore),
		%write('nobleEarned\n'),
	calcBonusvalue(MyBonuses, AvNobles, BONUSVALUE),
		%write('calcBonusvalue\n'),
		%!,
		%trace,
	maplist(plus, MyGems, MyBonuses, MyGemsAndBonuses),
		%write('maplist\n'),
	calcBuyAbility(AllCards, MyGemsAndBonuses, BUYABILITY),
		%write('calcBuyAbility\n'),
		%nodebug,
	STATEVALUE is NewScore + BONUSVALUE + BUYABILITY,
		%write('STATEVALUE\n'),
	(
		NewScore > 14,
		%write('winning\n'),
		WinLoose = win
		;
		WinLoose = neutral
		%,write('neutral\n')
		%% TODO: check if oponent can win!
	).

nobleEarned(_, [], Old, Old).
nobleEarned(MyBonuses, [Av|Nobles], Old, New):-
	colorsMissingForOneNoble(Av, MyBonuses, MissingBonuses),
	(
		MissingBonuses = [0,0,0,0,0,0],
		New is Old + 3
		;
		nobleEarned(MyBonuses, Nobles, Old, New)
	).

calcBonusvalue(Bonuses, AvNobles, BONUSVALUE):-
	calcNobleValue(Bonuses, AvNobles, NobleValue),
	calcBonusBalance(Bonuses, BonusBalance),
	% TODO: insert parameters!
	BONUSVALUE is NobleValue + BonusBalance.


%--NobleValue = PAR1*numberOfNobles (- PAR4*opponentIsCloserToNoble)
%
% zählt nur wieviele bonusse noch für nobles fehlen - falls ein nobel hier gewonnen wird, dann zählt das NUR in die pp?
%
calcNobleValue(Bonuses, AvNobles, NobleValue):-
	colorsMissingForAnyNoble(AvNobles, Bonuses, Colorlist),
	foldl(plus, Colorlist, 0, Missing),
	(
		Missing = 0,!,
		NobleValue is 1
		;
		NobleValue is 1/Missing
	)
	.

calcBonusBalance(Bonuses, BonusBalance):-
	% FEHLER! in den bonusen ist gold mit drin, sprich das ist immer 0! abschneiden!
	max_member(Max, Bonuses),
	min_member(Min, Bonuses),
	(
		% if There is less than 4 of all bonuses, balance doesnt matter
		% chose arbitrary value here...
		Max < 4,
		Diff is 1
		;
		Diff is Max - Min
	),
	(
		Diff = 0,
		!,
		BonusBalance is 1
		;
		BonusBalance is 1/Diff
	).


% berechne den wert der karten vorher, damit das nicht wiederholt werden muss. hier soll nur noch kosten und wert der karte kommen
% each Card: [Value, Colors], GemsAndBonuses: [W, B, G, R, S, Gold]
% for starters: check cards that can be bought
calcBuyAbility(Cards, GemsAndBonuses, BuyAbility):-
		%write('startcalcbuyab\n'),
	%trace,
	cardShares(GemsAndBonuses, Cards, Shares),
		%write('cardShares\n'),
	%nodebug,
	include(=:=(1), Shares, CanBuy),
		%write('include\n'),
	length(Shares, NumAll),
	length(CanBuy, NumCanBuy),
		%write('both lengths\n'),
	%trace,
	BuyAbility is 1/(NumAll - NumCanBuy)
		%,write('BuyAbility\n')
	%,nodebug
	.

cardShares(_, [], []).
cardShares(MyGemsAndBonuses, [H|T], [Share|Shares]) :-
		%write('start cardShares\n'),
	%show(0, 'MyGemsAndBonuses ~w ~n', [MyGemsAndBonuses]),
	%show(0, 'H ~w ~n', H),
	%show(0, 'T ~w ~n', [T]),
	%show(0, 'Share ~w ~n', [Share]),
	%show(0, 'Shares ~w ~n', [Shares]),
	%trace,
	canBuyThis(H, MyGemsAndBonuses, Share),
	%show(0, 'Share ~w ~n', Share),
	%nodebug,
	cardShares(MyGemsAndBonuses, T, Shares).


% Share = 1 means card can be bought, otherwise its less
% Card, MyGemsAndBonuses, Result
canBuyThis(ID, [MW, MB, MG, MR, MS, MGold], Share):-
	card(ID, [W, B, G, R, S, _]-_-_),
	checkOneColor(W, MW, GW),
	checkOneColor(B, MB, GB),
	checkOneColor(G, MG, GG),
	checkOneColor(R, MR, GR),
	checkOneColor(S, MS, GS),
	GoldCost is GW + GB + GG + GR + GS,
	checkOneColor(GoldCost, MGold, Missing),
	foldl(plus, [W, B, G, R, S], 0, Needed),
	(
		%trace,
		Half is Needed/2,
		%nodebug,
		Missing > Half,
		Share = 0
		;
		%trace,
		Share is 1/(Needed-Missing)
		%,nodebug
	).

checkOneColor(Cost, Color, Missing):-
	(
	Cost =< Color,
	Missing = 0
	;
	Missing is Cost - Color
	).
