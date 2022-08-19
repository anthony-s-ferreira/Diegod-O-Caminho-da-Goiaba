:- include('Events.pl').
:- include('Bodies.pl').
:- include('Pontuacao.pl').

% 0 caixa
% 1 poça -> pode ou não ser mortal
% 2 policia
% 3 nada
% 4 abelhas

%Para a poça matar ele afogado será gerado um número aleatório de 1 até 100
%Se esse número for divisível por 3 ele pode morrer afogado
pocaMortal(X):- 
    random(1,100,Random), 
    Mod is Random mod 3, 
    X = (Mod == 0).


event(0,CondicaoVida,CheckBonus,Multiplicador,QuaseMorreu, Pontuacao):- 
    HaPontos = (Pontuacao >= 10),
    (
    not(HaPontos) -> boxNoPoints,
                     foundBoxNoPoints,
                     CondicaoVida = true, 
                     CheckBonus = true, 
                     QuaseMorreu = true;

    Multiplicador == 1 -> box, 
                           write("Ao abrir esta caixa você perderá a goiaba dourada"), 
                           nl, 
                           nl,
                           foundBox, 
                           CondicaoVida = true, 
                           CheckBonus = true, 
                           QuaseMorreu = false;

    Multiplicador == 2 -> box, 
                          write("Ao abrir esta caixa você perderá a cinquentinha"), 
                          nl, 
                          nl,
                          foundBox, 
                          CondicaoVida = true, 
                          CheckBonus = true, 
                          QuaseMorreu = false;

    Multiplicador == 3 -> box, 
                          write("Ao abrir esta caixa você perderá a calopsita gigante"), 
                          nl, 
                          nl,
                          foundBox, 
                          CondicaoVida = true, 
                          CheckBonus = true, 
                          QuaseMorreu = false;

    Multiplicador == 0 -> box, 
                          foundBox, 
                          CondicaoVida = true, 
                          CheckBonus = true, 
                          QuaseMorreu = false).


event(1,CondicaoVida,CheckBonus,Multiplicador,QuaseMorreu, _):- 
    pocaMortal(X),
    (Multiplicador == 1 -> pocaFunda, 
                           pocaMtFunda, 
                           CondicaoVida = true, 
                           CheckBonus = false, 
                           QuaseMorreu = true;
    
    Multiplicador == 2 -> noEvent, 
                          cinquentinha, 
                          CondicaoVida = true, 
                          CheckBonus = false,
                          QuaseMorreu = false;

    Multiplicador == 3 -> noEvent, 
                          calopisitaGigante, 
                          CondicaoVida = true, 
                          CheckBonus = false,
                          QuaseMorreu = false;

    X -> derrotaPoca, 
         pocaDefeat, 
         CondicaoVida = false, 
         CheckBonus = false,
         QuaseMorreu = false;

    not(X) -> pisaPoca, 
              pocaDoCao, 
              CondicaoVida = true, 
              CheckBonus = false,
              QuaseMorreu = false).

event(2,CondicaoVida,CheckBonus,Multiplicador,QuaseMorreu, Pontuacao):- 
    TemPontos = (Pontuacao >= 10),

    (Multiplicador == 1 -> baculejoGoiabaDourada,
                           baculejoGoiaba,
                           Multiplicador = 0;

     Multiplicador == 2 -> baculejoMotoquinha,
                           baculejoMoto,
                           Multiplicador = 0;

     not(TemPontos), not(Multiplicador == 2), not(Multiplicador == 1) -> derrotaBaculejo,
                                                                         baculejoDefeat,
                                                                         CondicaoVida = false,
                                                                         CheckBonus = false,
                                                                         QuaseMorreu = true,
                                                                         Pontuacao = Pontuacao;

     TemPontos, not(Multiplicador == 2), not(Multiplicador == 1) -> baculejo, 
                                                                    baculejoSimples,
                                                                    write("PAssando aquiiiiiiiiiiiiiiiiiiiiiiiiiiiiii"),
                                                                    removePontos(Pontuacao, 10, PontuacaoPlus),
                                                                    Pontuacao = PontuacaoPlus
    ).
    

event(3, CondicaoVida, CheckBonus, Multiplicador,QuaseMorreu, _):-
    noEvent,
    (Multiplicador == 0 -> none,
                           CondicaoVida = true, 
                           CheckBonus = false,
                           QuaseMorreu = true;

     Multiplicador == 1 -> noneGoiaba,
                           CondicaoVida = true, 
                           CheckBonus = false,
                           QuaseMorreu = true;

     Multiplicador == 2 -> noneCinquentinha,
                           CondicaoVida = true, 
                           CheckBonus = false,
                           QuaseMorreu = true;

     Multiplicador == 3 -> noneCalopsita,
                           CondicaoVida = true, 
                           CheckBonus = false,
                           QuaseMorreu = true
    ).

event(4,CondicaoVida,CheckBonus,Multiplicador,QuaseMorreu, _):- 
    (Multiplicador == 1 -> ataqueAbeia,
                           ataqueDasAbeia,
                           CondicaoVida = true,
                           CheckBonus = false,
                           QuaseMorreu = true;

     not(Multiplicador == 1) -> derrotaAbeia, 
                                abeiaDefeat, 
                                CondicaoVida = false, 
                                CheckBonus = false, 
                                QuaseMorreu = true
    ).
    

event("Goiaba"):- 
    goiaba, 
    goibaDourada.

event("Calopsita"):- 
    calopsita, 
    calopisitaGigante.

event("Motoquinha"):- 
    moto, 
    cinquentinha.

