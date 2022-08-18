show(P):- write("Pontuação ("), write(P), write(")"), nl, nl.
addPontos(X,R,P):- P is R + X, show(P).
removePontos(X,R,P):- P is X - R, show(P).
