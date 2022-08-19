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