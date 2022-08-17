
situationSemBaculejo(P, R):- S is P mod 5, 
(S == 2 -> R = 3;
not(S== 2) -> R = S).

situation(P, R):- R is P mod 5.

position(P, I, L):-
(I < 4 -> situationSemBaculejo(P, R), L = R;
I > 3-> situation(P, R), L = R).










