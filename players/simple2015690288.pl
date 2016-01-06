:- module(simple2015690288, []).

% Nobles: 501-[3,3,0,0,3,0]-3
% 

initialize(PlayerName, PlayerCount) :-
	show(1, 'I am ~w of a ~w player game.~n', [PlayerName, PlayerCount]).

getGems(_,_).
buyCard(_).
reserveCard(_).
reserveCardFromDeck(_).

decideAction(Player, Oponents, StateProxy, Action) :-
	call(StateProxy, Player, gems, Gems),
	%show(1, '~n MyGems: ~w ~n',[Gems]),
	call(StateProxy, Player, bonuses, Bonuses),
	%show(0, '~n MyBonus: ~w ~n',[Bonuses]),
	call(StateProxy, Player, reserves, Reserves),
	%show(1, '~n MyReserved: ~w ~n',[Reserves]),
	call(StateProxy, game, cards, [C|Cards]),
	%card(C, Info),
	%show(0,  '~n Card: ~w ~n', Info),
	%show(1, '~n OpenCards: ~w ~n',[Cards]),
	call(StateProxy, game, nobles, AvNobles),
	%show(1, '~n Nobles: ~w ~n',[AvNobles]),
	call(StateProxy, game, tokens, Tokens),
	%show(0, '~n Tokens: ~w ~n',[Tokens]),
	%show(1, '~n Oponents: ~w ~n', [Oponents]),
	(
		% if buying possible, do it!
		append(Reserves, [C|Cards], AllAvCards),
		canBuyCards(Gems, Bonuses, AllAvCards, CanBuyCards),
		cardMostPrestige(CanBuyCards, OrdDecrByPrestige),
		selectCardToBuy(OrdDecrByPrestige, AvNobles, Bonuses, UsefulForNobleCards),
		(
			%show(1, 'try UsefulCards',[]),
			nth1(1, UsefulForNobleCards, CardId)
			;
			%show(1, 'try plain',[]),
			nth1(1, OrdDecrByPrestige, CardId)
		),
		Action = buyCard(CardId)
		;
		% if reserving possible, reserve with probability 1/10
		length(Reserves, ReservesLength),
		ReservesLength<3,
		random(1,11,1),
		length([C|Cards], CardsLength),
		CardsLength1 is CardsLength+1,
		random(1, CardsLength1, ReserveId),
		nth1(ReserveId, [C|Cards], ReservedCard),
		Action = reserveCard(ReservedCard,[0,0,0,0,0,0])
		;
		% get gems
		call(StateProxy, game, tokens, Tokens),
		randomGetGems(Gems, Tokens, RandGems, BackGems),
		Action = getGems(RandGems, BackGems)
	)
	,member(Oponent, Oponents)
	,call(StateProxy, Oponent, score, _)
	.


% select card with bonus one has the less
selectCardToBuy([],_,_,_):- !, fail.
selectCardToBuy(CanBuyCards, AvNobles, Bonuses, UsefulForNobleCards):-
	colorsMissingForAnyNoble(AvNobles, Bonuses, Colorlist),
	%show(1, '~n Colorsmissing: ~w ~n',[Colorlist]),
	%show(1, '~n CanBuyCards: ~w ~n',[CanBuyCards]),
	%trace,
	include(cardUsefulForAnyNoble(Colorlist),CanBuyCards, UsefulForNobleCards)
	%show(1, '~n UsefulForNobleCards: ~w ~n',[UsefulForNobleCards])
	%favoriteBonus(AvNobles, Bonuses, Favorites),
	%,nodebug
	%show(1, '~n Favorites: ~w ~w ~w ~w ~w ~n',[Favorites])
	.


cardMostPrestige(Cards, OrderedByPrestige):- predsort(comparePrestige, Cards, OrderedByPrestige).

comparePrestige(>, Card1, Card2):-
	card(Card1, [_,_,_,_,_,_]-_-Prest1),
	card(Card2, [_,_,_,_,_,_]-_-Prest2),
	Prest1 < Prest2
	.
comparePrestige(<, _, _).

%checkForSameInARow(POrderedCards, Bonuses, SamePOrdered):-



% Bonuses missing to get a noble
cardUsefulForAnyNoble([W,B,G,R,S,_], CardID):-
	card(CardID, [_,_,_,_,_,_]-Cardcolor-_),
	%show(1, 'Cardcolor ~w ~n', [Cardcolor]),

	(
		Cardcolor =1,!,
		W > 0
		;
		Cardcolor =2,!,
		B > 0
		;
		Cardcolor =3,!,
		G > 0
		;
		Cardcolor =4,!,
		R > 0
		;
		Cardcolor =5,!,
		S > 0
	).


% not used for now, does not work yet and might not even help much
cardUsefulForMoreThanOneNoble([W,B,G,R,S], CardID):-
	card(CardID, [_,_,_,_,_,_]-Cardcolor-_),
	%show(1, 'Cardcolor ~w ~n', [Cardcolor]),
	(
		Cardcolor =1,!,
		W > 1
		;
		Cardcolor =2,!,
		B > 1
		;
		Cardcolor =3,!,
		G > 1
		;
		Cardcolor =4,!,
		R > 1
		;
		Cardcolor =5,!,
		S > 1
	).


colorsMissingForAnyNoble([], _, [0,0,0,0,0,0]).
colorsMissingForAnyNoble([Noble|Nobles], MyBonuses, Colorlist):-
	colorsMissingForOneNoble(Noble, MyBonuses, ThisColors),
	colorsMissingForAnyNoble(Nobles, MyBonuses, RemainColors),
	maplist(myMax, ThisColors, RemainColors, Colorlist).

myMax(A,B, C):-
	(
		A > B,
		C is A
		;
		C is B
	).

% colors needed, colors available,  colors to add
colorsMissingForOneNoble(_-ColorsNeededForNoble-_, MyBonuses, MissingBonuses):-
	maplist(diffGreaterZero, ColorsNeededForNoble, MyBonuses, MissingBonuses).


diffGreaterZero(A, B, C):- D is A - B, C is max(D, 0).

% sum up bonuses of each color needed for available nobles, subtract owned bonuses (once for every noble!)
favoriteBonus(AvNobles, [MW,MB,MG,MR,MS,_], [W,B,G,R,S]):-
	foldl(sumNobleBonus, AvNobles, [0,0,0,0,0], [TW,TB,TG,TR,TS]),
	%trace,
	%show(1, '~n Noblecolors needed: ~w ~w ~w ~w ~w ~n', [TW,TB,TG,TR,TS]),
	subtractOwnedBonusses(AvNobles, [MW,MB,MG,MR,MS], [TW,TB,TG,TR,TS], [W,B,G,R,S])
	%,nodebug
	.

subtractOwnedBonusses([],_, Same, Same).
subtractOwnedBonusses([Av|Nobles], MyBonuses, OldBonuses, NewBonuses):-
	subtractOwnedBonusses(Nobles, MyBonuses, OldBonuses, TempBonuses),
	subtractOwnedBonus(Av, MyBonuses, TempBonuses, NewBonuses).



% nobleinfo, mybonus, old bonus needed, new bonus needed
subtractOwnedBonus(_-[W,B,G,R,S,_]-_,[MW,MB,MG,MR,MS], [OW,OB,OG,OR,OS], [NW,NB,NG,NR,NS]):-
	subtractConditionally(W, OW, MW, NW),
	subtractConditionally(B, OB, MB, NB),
	subtractConditionally(G, OG, MG, NG),
	subtractConditionally(R, OR, MR, NR),
	subtractConditionally(S, OS, MS, NS).

subtractConditionally(C, O, M, N):-
(
	C>0,
	N is O - M
	;
	N is O
).

sumNobleBonus(_-[W,B,G,R,S,_]-_,[OW,OB,OG,OR,OS], [NW,NB,NG,NR,NS]):- sumBonus([W,B,G,R,S], [OW,OB,OG,OR,OS], [NW,NB,NG,NR,NS]).


%white#,blue#,green#,red#,black#,
sumBonus([W,B,G,R,S], [OW,OB,OG,OR,OS], [NW,NB,NG,NR,NS]):-
	NW is W + OW,
	NB is B + OB,
	NG is G + OG,
	NR is R + OR,
	NS is S + OS.


%countBonusColors(CardIds, [W,B,G,R,S]):-

%filterForNeededBonus([W,B,G,R,S], CanBuyCards, UsefulCards):-
%	colorsMissingForNobles().



%510-[4,4,0,0,0,0]-3
%A, [N1,N2,N3,N4,N5,0]-X-P, L

%testfold:-findall([W,B,G,R,S],cardDataRaw(_, _,   _,_,_,_,_, _, W,B,G,R,S), AllCards),
%	foldl(sumBonus, AllCards, [0,0,0,0,0], [W,B,G,R,S]),
%	show(1, '~n Cardcolors: ~w ~w ~w ~w ~w ~n', [W,B,G,R,S]).



%%%% Determine Bonus values
% UsefulForNobleCards - more than one? do I get a noble with this bonus in this turn?
% Untergeordnet: 






selectNoble([H|_],H).

canBuyCards(_, _, [], []).

canBuyCards(Gems, Bonuses, [H|T], A) :-
	(
		canBuyCard(Gems, Bonuses, H),
		A = [H|X2]
		;
		A=X2
	),
	canBuyCards(Gems, Bonuses, T, X2)
	.
