:- include('Dado.pl').
:- include('Position.pl').
:- include('Pontuacao.pl').
:- include('Assets.pl').

play("J", X, RD):-
random(1,6,R), nl,
write("Você tirou "), write(R), write("!"), nl, nl, dado(R), 
addPontos(X,R,P), 
position(P, RD, L), 
event(L).


% play("A"):-
% (hasBox -> gift() ).