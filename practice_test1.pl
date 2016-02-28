% Given Facts
sellsFor('Peter Jones',swan123,28).
sellsFor('Peter Jones',electrolux214,27).

sellsFor('Peter Jones',hoover02,60).
sellsFor('Peter Jones',electrolux09,70).

sellsFor('Harrods', electrolux214, 31).
sellsFor('Harrods', swan123, 30).

sellsFor('Harrods', hoover02, 65).
sellsFor('Harrods', electrolux09, 80).

inStock('Peter Jones',electrolux214).
inStock('Peter Jones',hoover02).
inStock('Peter Jones',electrolux09).

inStock('Harrods',swan123).
inStock('Harrods',electrolux09).

locatedIn('Peter Jones',london).
locatedIn('Harrods',london).
	

typeOfItem(swan123,electricKettle).
typeOfItem(electrolux214,electricKettle).
typeOfItem(hoover02, vacuum_cleaner).     
typeOfItem(electrolux09, vacuum_cleaner).        

equivalentItems(electrolux214,swan123).
equivalentItems(electrolux09,hoover02).



forall(C1,C2) :- \+ ((C1,\+C2)).


% ----------------------------------------------------------------------------
% Supplier S sells item I of type T at price I which is less than MP
sellsOneForLessThan(T,MP,S,I,P):-
	sellsFor(S, I, P),
	typeOfItem(I,T),
	P < MP.

% I1 and I2 have equivalent funcitonality no matter what the alphabetic
% identifiers of I1, I2
equivalent(I1,I2):-
	(equivalentItems(I1,I2);
	equivalentItems(I2,I1)).

% S is a supplier located in city C, that has item I in stock
itemInStock(I,C,S):-
	locatedIn(S,C),
	inStock(S,I).

% S is a supplier located in city C, and S has an item equivalent to I
% in stock, the equivalent is EI and its price is no more than that of
% item I.
equivalentItemInStock(I,C,EI,S):-
	locatedIn(S,C),
	equivalent(I,EI),
	sellsFor(S,I,P1),
	sellsFor(S,EI,P2),
	inStock(S, EI),
	P2<P1.

% S is a supplier located in city C, and S either has item I in
% stock and EI=I, or S has an equivalent item EI in stock for 
% no more than its price for item I.
sellsEquivalentItemIn(I, C, EI, S):-
	itemInStock(I,C,S);
	equivalentItemInStock(I,C,EI,S).

% There is no other supplier that in city C sells any item
% that S sells for a price less than S.	
neverUnderSold(S,C):-
	locatedIn(S,C),
	sellsFor(S,I,P),
	sellsFor(Competetor, I, P1),
	(locatedIn(Competetor, C)-> P1 > P),
	competetor\=S.

% L is a list of pairs (P,S) where S is a supplier located
% in city C that supplies item I for the price P and has
% I in stock. L is ordered by increasing price. 
listOfSuppliersFor(I,C,L):-	
	setof((P,S), pair(P,S,I,C) , L).	

pair(P,S,I,C):- 
	locatedIn(S,C),
	sellsFor(S,I,P),
	inStock(S,I).
% ----------------------------------------------------------------------------

% remove_item(E,L,NewL), NewL is the list that results from 
% removing all occurences of element E from list L

remove_item(_,[],[]).
remove_item(E,[H|T], [H|NewL]):- 
	H\=E,
	remove_item(E,T,NewL).
remove_item(E,[H|T],NewL):-
	H=E,
	remove_item(E,T,NewL).

% drop_items(L, N, NewL), NewL is list L with its first N
% elements deleted.

drop_items(L,N,[]):-
	length(L,X),
	X < N.

drop_items(L,N,NewL):-
	append(X, NewL,L),
	length(X,N).
	 



% remove_from_list(L1, L2, L3), remove all elements in the list
% L2 from the list L1, the list without the elements is L3

remove_from_list(L1, [], []).
remove_from_list(L1,[H|T],L3):-
	member(H, L1),
	remove_from_list(L1, T, L3).

remove_from_list(L1,[H|T],[H|L3]):-
	nonmember(H, L1),
	remove_from_list(L1, T, L3).

% drop_more_items(L, N, NewL), NewL is the result of first
% removing th efirst N elements from list L and then removing
% all occurences of these first N elements from the rest of
% the list.
drop_more_items(L,N,[]):-
	length(L,X),
	X < N.

drop_more_items(L,N,NewL):-drop_more_items(L,N,NewL, Discard).

drop_more_items(L, N, NewL, Discard):-
	append(Discard, FrontOff,L),
	length(Discard,N),
	remove_from_list(Discard, FrontOff, NewL).

% L is a list of elements and Result is a list of all items
% of the form (I,Num), where I is an element in L and Num
% is the number of times I occurs in L.
	
count(L, Result):-
	setof((I,Num), letter_count(I,Num,L), Result).


% letter_count(X, Num, L), the number of times Num that 
% X occurs in list L. 
letter_count(_,0,[]).
letter_count(H, Num, [H|T]):-
	letter_count(H, PrevNum, T),
	Num is PrevNum + 1.

letter_count(X,Num, [H|T]):-
	letter_count(X,Num, T),	
	X\=H.

