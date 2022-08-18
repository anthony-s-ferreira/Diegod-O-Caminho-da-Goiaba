
power_up(X,R,V,P):- C is (X*2) mod 3, G is (X ^ 2) mod 2, R=5,
( G == 1 -> P = "G", V=1;
  C == 0 -> P ="C", V=3;
  not(C==0), not(G==1) -> P = "M", V=2
).

% Calopsita: se o dobro da pontuação é divisível por 3
% Motoquinha: se nenhuma outra condição for atendida
% Goiaba Dourada: se a potencia da pontuação é um número ímpar