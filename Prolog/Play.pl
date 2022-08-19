:- include('Dado.pl').
:- include('Position.pl').
:- include('Pontuacao.pl').
:- include('Assets.pl').
:- include('PowerUps.pl').

play(1, Pontuacao, Rodadas, _, Bonus, Multiplicador):-
    random(1,6,Random),
    write("Você tirou "), 
    write(Random), 
    write("!"), nl, 
    dado(Random),

    %Se tiver pontos a serem multiplicados
    (Bonus > 0 -> Pontos is Multiplicador * Random, 
                  BonusPlus is Bonus -1, 
                  MultiplicadorPlus = Multiplicador;

    Bonus == 0, Multiplicador > 1 -> Pontos = Random, 
                         BonusPlus = 0, 
                         MultiplicadorPlus = 0;

    Bonus == 0, not(Multiplicador > 1) -> Pontos = Random, 
    BonusPlus = 0, 
    MultiplicadorPlus = Multiplicador),

    addPontos(Pontuacao,Pontos,PontuacaoAtual), 

    position(Localizacao), 

    event(Localizacao,CondicaoVida,CheckBonus,MultiplicadorPlus,QuaseMorreu, PontuacaoAtual),

    %QuaseMorreu é verdade se ele quase morrer ou perder o bônus
    (QuaseMorreu -> MultiplicadorPlus = 0, BonusPlus = 0;

    not(QuaseMorreu) -> MultiplicadorPlus = Multiplicador, BonusPlus = Bonus),
                        RodadasPlus is Rodadas + 1,

    %CondicaoVida for false é pq ele morreu
    (CondicaoVida -> read(Input),
                     nl,
                     play(Input, PontuacaoAtual, RodadasPlus, CheckBonus, BonusPlus, MultiplicadorPlus);
    
    not(CondicaoVida), RodadasPlus == 1 -> write("Você morreu na primeira rodada, que azar!");

    not(CondicaoVida) -> write("Você morreu em "), 
                         write(RodadasPlus), 
                         write(" rodadas!")).

play(2, Pontuacao, Rodadas, true, Bonus, Multiplicador):-
    (Pontuacao >= 10 -> power_up(BonusPlus,MultiplicadorPowerUp,PowerUpName), 
                      removePontos(Pontuacao,10,PontosRemovidos),
                      event(PowerUpName), 
                      read(Input), 
                      nl, 
                      play(Input, PontosRemovidos, Rodadas, false, BonusPlus, MultiplicadorPowerUp);
    (Pontuacao < 10) -> nl, 
                          write("Você não tem pontos suficientes"), 
                          nl, 
                          read(Input), 
                          play(Input, Pontuacao, Rodadas, false, Bonus, Multiplicador)).

