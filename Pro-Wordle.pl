:- dynamic word/2.

is_category(C):-
	word(_,C).


categories(L) :- 
    setof(Y,is_category(Y),L).


available_length(L):-
	word(X,_),
	atom_length(X,L).
	


pick_word(W,L,C):-
	word(W,C),
	atom_length(W,L).

	
remove_duplicates([], []).

remove_duplicates([H|T], R) :-
    member(H, T), !,
    remove_duplicates(T, R).

remove_duplicates([H | T], [H | R]) :-
    remove_duplicates(T, R).

correct_letters(L1,L2,L4):-
	correct_letters_h(L1,L2,L3),
	remove_duplicates(L3,L4).

correct_letters_h([],_,[]).
correct_letters_h([H1|T1],L2,[H1|T2]):-
	member(H1,L2),
	correct_letters_h(T1,L2,T2).
correct_letters_h([H1|T1],L2,CL):-
	\+member(H1,L2),
	correct_letters_h(T1,L2,CL).

	
correct_positions([],[],[]).
correct_positions([H1|T1],[H1|T2],[H1|T3]):-
	correct_positions(T1,T2,T3).
correct_positions([H1|T1],[H2|T2],PL):-
	H1\==H2,
	correct_positions(T1,T2,PL).


build_kb:-
	write('Please enter a word and its category on separate lines:'),
	nl,
	read(W),
	( (W = done ,
		nl,
		write('Done building the words database...'),nl);
	  (read(C),
	  assert(word(W,C)),
	  build_kb)
	).

getCategory(Cat):-
	write('Choose a category:'),
	nl,
	read(R),
	((is_category(R), Cat=R);
	(\+is_category(R),
	write('This category does not exist.'),
	nl,
	getCategory(Cat))).

	
getLength(Len,Cat):-
	write('Choose a length:'),
	nl,
	read(L1),
	((\+number(L1), write('Input should be integer'), nl, getLength(Len,Cat));
	(available_length(L1),pick_word(_,L1,Cat), Len=L1);
	((\+available_length(L1) ; \+pick_word(_,L1,Cat)),
	  write('There are no words of this length.'),
	  nl,
	  getLength(Len,Cat))).

checkInput(Trials,Len,Word,X):-
	(atom_chars(X,ListX),
	 atom_chars(Word,ListW),
	 atom_length(X,TL),
	 TL\==Len,
	 write('Word is not composed of '),
	 write(Len),
	 write(' letters. Try again.'),
	 nl,
	 write('Remaining Guesses are '),
	 write(Trials),
	 nl,
	 nl,
	 guessing(Trials,Len,Word));
	(\+word(X,_),
     write('Not a valid word'), 
	 nl, 
	 write('Remaining Guesses are '),
	 write(Trials),
	 nl,
	 nl,
	 guessing(Trials,Len,Word));
	(atom_chars(X,ListX),
	 atom_chars(Word,ListW),
	 ListW = ListX,
	 write('You Won!'));
	(NewTrials is Trials-1,
	 NewTrials = 0,
	 guessing(NewTrials,Len,Word));
	(atom_chars(X,ListX),
	 atom_chars(Word,ListW),
	 correct_letters(ListX,ListW,Letters),
	 correct_positions(ListX,ListW,Positions),
	 write('Correct letters are: '),
	 write(Letters),
	 nl,
	 write('Correct letters in correct positions are: '),
	 write(Positions),
	 nl,
	 write('Remaining Guesses are '),
	 NewTrials is Trials-1,
	 write(NewTrials),
	 nl,
	 nl,
	 guessing(NewTrials,Len,Word)).

guessing(0,Len,Word):-
	write('You lost!').
guessing(Trials,Len,Word):-
	write('Enter a word composed of '),
	write(Len),
	write(' letters:'),
	nl,
	read(X),
	checkInput(Trials,Len,Word,X).
	
	
	
play:-
	write('The available categories are: '),
	categories(L),
	write(L),
	nl,
	getCategory(Cat),
	getLength(Len,Cat),
	Trials is Len+1,
	write('Game started. You have '),
	write(Trials),
	write(' guesses.'),
	nl,
	nl,
	pick_word(Word,Len,Cat),
	guessing(Trials,Len,Word).
	
main:-
	write('Welcome to Pro-Wordle!'),
	nl,
	write('---------------------------------'),
	nl,
	nl,
	build_kb,
	play.