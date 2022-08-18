:- include('Events.pl').
:- include('Bodies.pl').

% 0 caixa
% 1 poça -> pode ou não ser mortal
% 2 policia
% 3 nada
% 4 abelhas

%Para a poça matar ele afogado será gerado um número aleatório de 1 até 100
%Se esse número for divisível por 3 ele pode morrer afogado
pocaMortal(X):- random(1,100,N), Y is N mod 3, X = (Y == 0).


event(0,R,B,M,D):- 
(M == 1 -> box, write("Ao abrir esta caixa você perderá a goiaba dourada"), nl, nl,
 foundBox, R = true, B = true, D = false;
 M == 2 -> box, write("Ao abrir esta caixa você perderá a cinquentinha"), nl, nl,
 foundBox, R = true, B = true, D = false;
 M == 3 -> box, write("Ao abrir esta caixa você perderá a calopsita gigante"), nl, nl,
 foundBox, R = true, B = true, D = false;
 M == 0 -> box, foundBox, R = true, B = true, D = false).


event(1,R,B,M,D):- pocaMortal(X),
(M == 1 -> pocaFunda, pocaMtFunda, R = true, B = false, D = true;
 M == 2 -> noEvent, cinquentinha, R = true, B = false,D = false;
 M == 3 -> noEvent, calopisitaGigante, R = true, B = false,D = false;
 X -> derrotaPoca, pocaDefeat, R = false, B = false,D = false;
 not(X) -> pisaPoca, pocaDoCao, R = true, B = false,D = false).

event(2,R,B,_,D):- baculejo, baculejoSimples, R = true, B = false,D = false.
event(3,R,B,_,D):- noEvent, none, R = true, B = false,D = false.
event(4,R,B,_,D):- derrotaAbeia, abeiaDefeat, R = false, B = false, D = true.