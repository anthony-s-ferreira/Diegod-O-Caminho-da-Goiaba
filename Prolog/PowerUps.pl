
power_up(Pontuacao,R,PowerUp,PowerUpName):- 
  Calculo is (Pontuacao * 2) mod 3, 
  Goiaba is (Pontuacao ^ 2) mod 2, 
  R = 5,
  ( Goiaba == 1 -> PowerUpName = "Goiaba", 
                   PowerUp = 1;

    Calculo == 0 -> PowerUpName ="Calopsita", 
                    PowerUp = 3;

    not(Calculo==0), not(Goiaba==1) -> PowerUpName = "Motoquinha", 
                                       PowerUp = 2
  ).

% Calopsita: se o dobro da pontuação é divisível por 3
% Motoquinha: se nenhuma outra condição for atendida
% Goiaba Dourada: se a potencia da pontuação é um número ímpar