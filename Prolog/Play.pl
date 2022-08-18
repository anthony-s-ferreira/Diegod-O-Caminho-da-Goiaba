:- include('Dado.pl').
:- include('Position.pl').
:- include('Pontuacao.pl').
:- include('Assets.pl').
:- include('PowerUps.pl').

play("J", X, RD, BX, BN, M):-
random(1,6,R), nl,
write("Você tirou "), write(R), write("!"), nl, nl, dado(R),
%Se tiver pontos a serem multiplicados
(BN > 0 -> Pts is M * R, BN1 is BN -1, MT = M;
BN == 0, M > 1 -> Pts = R, BN1 = 0, MT = 0;
BN == 0 -> Pts = R, BN1 = 0, MT = M),
addPontos(X,Pts,P), 
position(P, L), 
event(L,F,B,MT,D), BX = B,
%D é verdade se ele quase morrer ou perder o bônus
(D -> MT = 0, BN1 = 0;
not(D) -> MT = M, BN1 = BN),
RD1 is RD + 1,
%F for false é pq ele morreu
(F -> read(I), play(I, P, RD1, BX, BN1, MT);
not(F) -> write("Você morreu em "), write(RD), write(" rodadas!")).

play("A", X, RD, true, _, _):-
power_up(X,R,V,P), event(P),
read(I), play(I, X, RD, false, R, V).
