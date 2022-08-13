
noEvent() :- write("Nada aconteceu!"), nl, nl, 
write("Jogar dado (ganha pontos)"), nl, nl.

box():- write("Diegod achou uma caixa! Deseja abrir?"), nl, nl, 
write("Abrir caixa (-10 pontos + Power up)  Jogar dado (ganha pontos)"), 
nl, nl.

moto():- write("Diegod ganhou uma 50tinha, pontuação x2 por 5 rodadas."),nl, 
write("Cuidado com os policiais!"), nl, nl, write("Jogar dado (ganha pontos)"), nl, nl.

calopisita():- write("Diegod achou a calopsita gigante, pontuação x3 por 5 rodadas."),
nl, nl, write("Jogar dado (ganha pontos)"), nl, nl.

goiaba():- write("Diegod achou a goiaba dourada, não perderá pontos ou morrerá!"),nl, 
write("A goiaba dourada só será usada uma vez."),nl, nl, write("Jogar dado (ganha pontos)"), nl, nl.

baculejoMotoquinha():- write("Diegod foi pego pela polícia e perdeu a 50tinha!"),nl, 
write("Você perdeu a pontuação x2!"), nl, nl, write("Jogar dado (ganha pontos)"), nl, nl.

perdeCalopsita():- write("A calopsita gigante defendeu você das abelhas e foi embora."),nl, 
write("Você perdeu a pontuação x3!"), nl, nl, write("Jogar dado (ganha pontos)"), nl, nl.