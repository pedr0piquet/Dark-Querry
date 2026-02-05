:- discontiguous male/1.
:- discontiguous female/1.
:- discontiguous kahnwald/1.
:- discontiguous nielsen/1.
:- discontiguous doppler/1.
:- discontiguous tiedemann/1.
:- discontiguous parent/2.
:- discontiguous brother/2.
:- discontiguous sister/2.
:- discontiguous half_brother/2.
:- discontiguous half_sister/2.
:- discontiguous grandparent/2.
:- discontiguous grandmother/2.
:- discontiguous grandfather/2.
:- discontiguous mother/2.
:- discontiguous father/2.
:- discontiguous ancestor/2.
:- discontiguous paradox/1.
:- discontiguous aunt/2.
:- discontiguous uncle/2.
:- discontiguous half_uncle/2.
:- discontiguous half_aunt/2.

%kahnwalds

same_person(mikkel, michael).

male(jonas).
kahnwald(jonas).
parent(michael, jonas).
parent(hannah, jonas).

male(michael).
kahnwald(michael).
parent(ulrich, michael).
parent(katharina, michael).

female(hannah).
kahnwald(hannah).
parent(sebastian, hannah).
parent(unknown_mother, hannah).

female(ines).
kahnwald(ines).
parent(daniel, ines).
parent(unknown_mother, ines).

male(sebastian).
kahnwald(sebastian).
parent(unknown_father, sebastian).
parent(unknown_mother, sebastian).



%nielsens

male(ulrich).
nielsen(ulrich).
parent(tronte, ulrich).
parent(jana, ulrich).

female(katharina).
nielsen(katharina).
parent(helene, katharina).
parent(hermann, katharina).

male(mikkel).
nielsen(mikkel).
parent(ulrich, mikkel).
parent(katharina, mikkel).

female(martha).
nielsen(martha).
parent(ulrich, martha).
parent(katharina, martha).

male(magnus).
nielsen(magnus).
parent(ulrich, magnus).
parent(katharina, magnus).

male(tronte).
nielsen(tronte).
parent(unknown_father, tronte).
parent(agnes, tronte).

female(jana).
nielsen(jana).
parent(unknown_father, jana).
parent(unknown_mother, jana).

female(agnes).
nielsen(agnes).
parent(bartosz, agnes).
parent(silja, agnes).

%dopplers

male(peter).
doppler(peter).
parent(helge, peter).
parent(unknown_mother, peter).

female(charlotte).
doppler(charlotte).
parent(noah, charlotte).
parent(elisabeth, charlotte).

female(elisabeth).
doppler(elisabeth).
parent(peter, elisabeth).
parent(charlotte, elisabeth).

female(franziska).
doppler(franziska).
parent(peter, franziska).
parent(charlotte, franziska).

male(helge).
doppler(helge).
parent(bernd, helge).
parent(greta, helge).

male(bernd).
doppler(bernd).
parent(unknown_father, bernd).
parent(unknown_mother, bernd).

female(greta).
doppler(greta).
parent(unknown_father, greta).
parent(unknown_mother, greta).

%tiedemanns

female(claudia).
tiedemann(claudia).
parent(egon, claudia).
parent(doris, claudia).

male(egon).
tiedemann(egon).
parent(unknown_father, egon).
parent(unknown_mother, egon).

female(doris).
tiedemann(doris).
parent(unknown_father, doris).
parent(unknown_mother, doris).

male(aleksander).
tiedemann(aleksander).
parent(unknown_father, aleksander).
parent(unknown_mother, aleksander).

male(bartosz).
tiedemann(bartosz).
parent(aleksander, bartosz).
parent(regina, bartosz).

female(regina).
tiedemann(regina).
parent(tronte, regina).
parent(claudia, regina).

%other

male(noah).
tauber(noah).
parent(bartosz, noah).
parent(silja, noah).

female(silja).
tiedemann(silja).
parent(egon, silja).
parent(hannah, silja).

father(X, Y) :- 
    parent(X, Y), 
    male(X).

mother(X, Y) :- 
    parent(X, Y), 
    female(X).

son(X, Y) :- 
    parent(Y, X), 
    male(X).

daughter(X, Y) :- 
    parent(Y, X), 
    female(X).

brother(X, Y) :-
    male(X),
    parent(M, X), parent(M, Y),
    parent(F, X), parent(F, Y),
    M \= F, X \= Y.

sister(X, Y) :-
    female(X),
    parent(M, X), parent(M, Y),
    parent(F, X), parent(F, Y),
    M \= F, X \= Y.

half_brother(X, Y) :-
    male(X),
    parent(P, X), parent(P, Y),        % Share one parent
    parent(P2, X), parent(P3, Y),      % Identify second parents
    P \= P2, P \= P3, P2 \= P3,        % Ensure they are different people
    X \= Y.

half_sister(X, Y) :-
    female(X),
    parent(P, X), parent(P, Y),
    parent(P2, X), parent(P3, Y),
    P \= P2, P \= P3, P2 \= P3,
    X \= Y.

grandparent(GP, GC) :-
    parent(GP, Z),
    parent(Z, GC).

grandfather(GF, GC) :-
    male(GF),
    grandparent(GF, GC).

grandmother(GM, GC) :-
    female(GM),
    grandparent(GM, GC).

ancestor(A, X) :-
    parent(A, X).
% Recursive case: An ancestor of your parent is also your ancestor
ancestor(A, X) :-
    parent(P, X),
    ancestor(A, P).

paradox(X) :-
    ancestor(X, X).

uncle(U, X) :-
    parent(P, X),
    brother(U, P).

half_uncle(U, X) :-
    parent(P, X),
    half_brother(U, P).

half_aunt(A, X) :-
    parent(P, X),
    half_sister(A, P).

aunt(A, X) :-
    parent(P, X),
    sister(A, P).

