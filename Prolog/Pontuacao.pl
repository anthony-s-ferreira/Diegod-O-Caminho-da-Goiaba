show(Total):- 
    write("Pontuação "), 
    write(Total), 
        nl, 
        nl.

addPontos(Pontos,Variavel,Total):- 
    Total is Variavel + Pontos, 
    write("Pontuação "),
    write(Total),nl, 
        nl.


removePontos(Pontos,Variavel,Total):- 
    Total is Pontos - Variavel, 
    write("Pontuação "),
    write(Total),
    nl, 
        nl.

:- discontiguous removePontos/3.
:- discontiguous addPontos/3.
:- discontiguous show/1.