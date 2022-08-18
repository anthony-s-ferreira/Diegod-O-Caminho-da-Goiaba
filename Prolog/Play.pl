:- include('Dado.pl').
:- include('Position.pl').
:- include('Pontuacao.pl').
:- include('Assets.pl').
:- include('PowerUps.pl').

play("J", Pontuacao, Rodadas, _, Bonus, Multiplicador):-
    random(1,6,Random), nl,
    write("Você tirou "), 
    write(Random), 
    write("!"), nl, 
    nl, 
    dado(Random),

    %Se tiver pontos a serem multiplicados

    (Bonus > 0 -> Pontos is Multiplicador * Random, 
                  BonusPlus is Bonus -1, 
                  MultiplicadorPlus = Multiplicador;

    Bonus == 0, 

    Multiplicador > 1 -> Pontos = Random, 
                         BonusPlus = 0, 
                         MultiplicadorPlus = 0;

    Bonus == 0 -> Pontos = Random, 
    BonusPlus = 0, 
    MultiplicadorPlus = Multiplicador),
    addPontos(Pontuacao,Pontos,PontuacaoAtual), 
    position(PontuacaoAtual, Localizacao), 
    event(Localizacao,CondicaoVida,CheckBonus,MultiplicadorPlus,QuaseMorreu),
    %QuaseMorreu é verdade se ele quase morrer ou perder o bônus
    (QuaseMorreu -> MultiplicadorPlus = 0, BonusPlus = 0;
    not(QuaseMorreu) -> MultiplicadorPlus = Multiplicador, BonusPlus = Bonus),
    RodadasPlus is Rodadas + 1,
    %CondicaoVida for false é pq ele morreu
    (CondicaoVida -> read(Input),nl,play(Input, PontuacaoAtual, RodadasPlus, CheckBonus, BonusPlus, MultiplicadorPlus);
    not(CondicaoVida) -> write("Você morreu em "), write(RodadasPlus), write(" rodadas!")).

play("ABRIR", Pontuacao, Rodadas, true, Bonus, Multiplicador):-
(Pontuacao > 10 -> power_up(Pontuacao,Random,MultiplicadorPowerUp,PontuacaoAtual), 
removePontos(Pontuacao,10,PontosRemovidos),event(PontuacaoAtual), 
read(Input), 
nl, 
play(Input, PontosRemovidos, Rodadas, false, Random, MultiplicadorPowerUp);
not(Pontuacao > 10) -> nl, write("Você não tem pontos suficientes"), 
                       nl, 
                       read(Input), 
                       play(Input, Pontuacao, Rodadas, false, Bonus, Multiplicador)).

