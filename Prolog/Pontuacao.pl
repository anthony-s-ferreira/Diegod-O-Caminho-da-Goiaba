:- discontiguous show/1.
:- discontiguous addPontos/3.
:- discontiguous removePontos/3.

show(Total):- write("Pontuação "), write(Total), 
    nl, 
    nl.


addPontos(Pontos,Variavel,Total):- 
    Total is Variavel + Pontos, 
    show(Total).

removePontos(Pontos,Variavel,Total):- 
    Total is Pontos - Variavel, 
    show(Total).