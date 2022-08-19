power_up(Pontuacao,Rounds,PowerUp,PowerUpName):- 
  random(0, 3, Random),
  Rounds = 5,
  ( Random == 1 -> PowerUpName = "Goiaba", 
                   PowerUp = 1, !;

    Random == 0 -> PowerUpName ="Calopsita", 
                    PowerUp = 3, !;

    not(Random==0), not(Random==1) -> PowerUpName = "Motoquinha", 
                                       PowerUp = 2, !
  ),
  Pontuacao = Pontuacao, !.

% Calopsita: se o dobro da pontuação é divisível por 3
% Motoquinha: se nenhuma outra condição for atendida
% Goiaba Dourada: se a potencia da pontuação é um número ímpar