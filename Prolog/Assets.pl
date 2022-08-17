:- include('Events.pl').
:- include('Bodies.pl').

% 0 caixa
% 1 poça -> pode ou não ser mortal
% 2 policia
% 3 nada
% 4 abelhas

event(0):- box, foundBox.
event(1):- pisaPoca, pocaDoCao.
event(2):- baculejo, baculejoSimples.
event(3):- noEvent, none.
event(4):- derrotaAbeia, ataqueDasAbeia.