:- include('Menu.pl').
:- include('Play.pl').

main:- 
menu,
read(J),
( J == 0 -> creditos;
  not(J == 0) -> play(J, 0, 0, false, 0, 0)
).

