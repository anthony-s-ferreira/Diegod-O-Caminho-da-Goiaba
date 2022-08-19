show(Total):- 
    write("Pontuação ("), 
    write(Total), 
    write(")"), 
    nl, 
    nl.


addPontos(Pontos,Variavel,Total):- 
    Total is Variavel + Pontos, 
    show(Total).

removePontos(Pontos,Variavel,Total):- 
    Total is Pontos - Variavel, 
    show(Total).

:- discontiguous removePontos/3.
:- discontiguous addPontos/3.
:- discontiguous show/1.