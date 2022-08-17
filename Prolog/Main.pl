:- include('Menu.pl').
:- include('Play.pl').

main:- 
menu,
read(J),
play(J, 0, 0).
