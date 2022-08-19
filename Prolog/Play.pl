:- include('Dado.pl').
:- include('Position.pl').
:- include('Pontuacao.pl').
:- include('Assets.pl').
:- include('PowerUps.pl').

play(1, Pontuacao, Rodadas, false, Bonus, Multiplicador):-
    random(1,6,Random),
    write("Você tirou "), 
    write(Random), 
    write("!"), nl, 
    dado(Random),

    %Se tiver pontos a serem multiplicados
    (Bonus > 0 -> Pontos is Multiplicador * Random, 
                  BonusPlus is Bonus -1, 
                  MultiplicadorPlus = Multiplicador,
                  write("------------------------------BOnus >1!!!!!!1------------------------------");

    Bonus == 0, Multiplicador >= 1 -> Pontos = Random, 
                         BonusPlus = 0, 
                         MultiplicadorPlus = 0, write("------------------------------BOnus == 0 multi > 1------------------------------");  

    Bonus == 0, Multiplicador == 0 -> Pontos = Random, 
    BonusPlus = 0, 
    MultiplicadorPlus = Multiplicador, write("------------------------------BOnus == 0------------------------------")),

    addPontos(Pontuacao,Pontos,PontuacaoAtual), 

    position(EventoAleatorio), 

    event(EventoAleatorio, CondicaoVida, CheckBonus, MultiplicadorPlus, QuaseMorreu, PontuacaoAtual, NovaPontuacao),
    write("Saiu do evento"),
    RodadasPlus is (Rodadas + 1),
    write("Somou rodada"),
    (QuaseMorreu -> MultiplicadorPlus = 0; 
    not(QuaseMorreu) -> MultiplicadorPlus is Multiplicador, write("FALSETED")),
    write(" checou quase morte"),
    %CondicaoVida for false é pq ele morreu
    (CondicaoVida -> read(Input), write("------------------------------Leu input------------------------------"),
                     nl,
                     
                     play(Input, NovaPontuacao, RodadasPlus, CheckBonus, BonusPlus, MultiplicadorPlus);
    
    not(CondicaoVida), RodadasPlus == 1 -> write("Você perdeu na primeira rodada, que azar!");

    not(CondicaoVida) -> write("Você perdeu em "), 
                         write(RodadasPlus), 
                         write(" rodadas!")).

play(2, Pontuacao, Rodadas, true, Bonus, Multiplicador):-
    (Pontuacao >= 10 ->
                      write("------------------------------Maior que 10------------------------------"),
                      power_up(BonusPlus,MultiplicadorPowerUp,PowerUpName), 
                      removePontos(Pontuacao, PontosRemovidos),
                      eventPower(PowerUpName), 
                      read(Input), 
                      nl, 
                      play(Input, PontosRemovidos, Rodadas, false, BonusPlus, MultiplicadorPowerUp),  
    (Pontuacao < 10) -> nl, 
                          write("Você não tem pontos suficientes"), 
                          nl, 
                          read(Input), 
                          play(Input, Pontuacao, Rodadas, false, Bonus, Multiplicador)).

addPontos(Pontos,Variavel,Total):- 
    Total is Variavel + Pontos, 
    write("Pontuação "),
    write(Total),nl, 
        nl.


removePontos(Pontos,Total):- 
    Total is Pontos - 10, 
    write("Pontuação "),
    write(Total),
    nl, 
    nl.
