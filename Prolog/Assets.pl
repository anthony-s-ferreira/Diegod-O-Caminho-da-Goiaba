:- include('Events.pl').
:- include('Bodies.pl').

% 0 caixa
% 1 poça -> pode ou não ser mortal
% 2 policia
% 3 nada
% 4 abelhas

event(0,R):- box, foundBox, R = true.
event(1,R):- pisaPoca, pocaDoCao, R = true.
event(2,R):- baculejo, baculejoSimples, R = true.
event(3,R):- noEvent, none, R = true.
event(4,R):- derrotaAbeia, ataqueDasAbeia, R = false.