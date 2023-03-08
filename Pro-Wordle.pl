main:-
	write("Welcome to Pro-Wordle!"),nl,
	write("----------------------"),nl,
	build_kb;
	write("Done building the words database..."),nl,
	play.
	
build_kb :-
	write("Please enter a word and its category on seperate lines: "),nl,
	(
		(read(N), N \= done, read(C)),
		assert(word(N,C)),
		build_kb
	).

play :-
	write("The available categories are: "),
	categories(C),nl,
	choose_category(R),nl,
	choose_length(R,L,Z),nl,
	write("Game started. you have 6 guesses."),nl,
	play_helper(Z,6,L).
	
play_helper(Word,Tries,L):-
	write("Enter a word composed of " ),
	write(L),
	write(" letters: "),nl,
	read(X),
	(
		string_length(X,Z),
		(
			Z \= L,
			write("Word is not composed of "),
			write(L),
			write(" letters .Try again."),
			nl,
			write("Remaining guesses are "),write(Tries),nl,
			play_helper(Word,Tries,L)
		);
		(
			Word == X,
			write("You Won! ")
		);
		(
			Tries1 is Tries - 1,Tries1 == 0,
			write("You Lost! ")
		);
		(
			atom_chars(Word,WL),
			atom_chars(X,XL),
			correct_positions(XL,WL,J),
			correct_letters(XL,WL,G),
			T1 is Tries - 1,
			write("correct letters are: "),write(G),nl,
			write("correct letters in the correct positions are: "),write(J),nl,
			write("Remaining guesses are "),write(T1),nl,
			play_helper(Word,T1,L)
		)
	).

choose_category(R):-
	write("Choose a category: "),nl,
	read(X),
	(
		(
			is_category(X),
			R=X
		);
		(
			write("This category does not exist."),nl,
			choose_category(R)
		)
	).
		
choose_length(R,L,Z):-
	write("Choose a length:"),nl,
	read(X),
	(
		(
			pick_word(W,X,R),
			L = X,
			Z = W
		);
		(
			write("There are no words of this length"),nl,
			choose_length(R,L,Z)
		)
	).
	
is_category(C):-
	word(_,C).

categories(C):-
	setof(C,is_category(C),L),
	write(L).
	
available_length(L):-
	word(N,_),
	string_length(N,L).
	
pick_word(W,L,C):-
	word(W,C),
	string_length(W,L).

correct_letters([X|Y],M,[X|Z]):- 
	member(X,M),
	correct_letters(Y,M,Z).
correct_letters([X|Y],M,Z):- 
	\+ member(X,M),
	correct_letters(Y,M,Z).
correct_letters([],M,[]).

correct_positions([X|Y],[X|M],[X|Z]):-
	correct_positions(Y,M,Z).
correct_positions([X|Y],[A|M],Z):-
	correct_positions(Y,M,Z).
correct_positions([],_,[]).
