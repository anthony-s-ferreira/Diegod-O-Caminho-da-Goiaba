:- include('Play.pl').


rendler(P, X):- read(J),
play(J, P, X).
