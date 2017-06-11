% Author:  Matus Namesny
% Date: 6.6.2016

/************************* 1. Inicializacia ***********************************/

/**
* postavi siet zo zadaneho grafu
* postavSiet(+[PrvaPartita, DruhaPartita], +Hrany, -Vrcholy, -HranyVystup)
* @arg PrvaPartita - vrcholy prvej partity vstupneho grafu
* @arg DruhaPartita - vrcholy druhej partity vstupneho grafu
* @arg Hrany - zoznam hran vstupneho grafu
* @arg Vrcholy - vystupny zoznam vrcholov siete
* @arg HranyVystup - vystupny zoznam hran siete
*/
postavSiet([PrvaPartita, DruhaPartita], Hrany, Vrcholy, HranyVystup) :-
                    orientuj(PrvaPartita, DruhaPartita, Hrany, [], Vystup), % zmeni vsetky hrany na orientovane
                    bipartitny([PrvaPartita, DruhaPartita], Vystup), % skontroluje ci zadany graf je bipartitny
                    findall(1-[s,X], member(X, PrvaPartita), Hranys),
                    findall(1-[Y,t], member(Y, DruhaPartita), Hranyt),
                    append(Hranys, Hranyt, NoveHrany),
                    append(Vystup, NoveHrany, HranyVystup1),
                    zpatneHrany(HranyVystup1, ZpatneHrany),
                    append(HranyVystup1, ZpatneHrany, HranyVystup),
                    append([s|PrvaPartita], [t|DruhaPartita], Vrcholy). % Vrcholy je list zo vsetkymi vrcholmi v sieti

/**
* zmeni neorientovane hrany na orientovane
* kazda hrana vychadza z prvej partity a vchadza do druhej partity
* orientuj(+PrvaPartita, +DruhaPartita, +Hrany, +InitVystup, -Vystup)
* @arg PrvaPartita - vrcholy prvej partity vstupneho grafu
* @arg DruhaPartita - vrcholy druhej partity vstupneho grafu
* @arg Hrany - zoznam hran vstupneho grafu
* @arg InitVystup - pociatocny zoznam orientovanych hran
* @arg Vystup - vysledny zoznam orientovanych hran
*/
orientuj(_, _, [], Vystup, Vystup).
orientuj(PrvaPartita, DruhaPartita, [[X,Y]|Hrany], InitVystup, Vystup) :- ((member(Y,PrvaPartita), member(X,DruhaPartita)) ->
                                                                      orientuj(PrvaPartita, DruhaPartita, Hrany, [1-[Y,X]|InitVystup], Vystup);
                                                                    orientuj(PrvaPartita, DruhaPartita, Hrany, [1-[X,Y]|InitVystup], Vystup)).



/**
* overi, ze graf je bipartitny
* false ak existuju hrany medzi vrcholmi jednej partity
* bipartitny(+[PrvaPartita, DruhaPartita], +Hrany)
*/
bipartitny([PrvaPartita, _],Hrany) :- member(X, PrvaPartita), member(Y, PrvaPartita), not(member(_-[X,Y], Hrany)).
bipartitny([_, DruhaPartita],Hrany) :- member(X, DruhaPartita), member(Y, DruhaPartita), not(member(_-[X,Y], Hrany)).

/**
* zpatneHrany(+Hrany, -ZpatneHrany)
* Ku kazdej hrane z Hrany prida zpatnu hranu s kapacitou 0
*/
zpatneHrany([], []).
zpatneHrany([1-[X,Y]|Hrany], [0-[Y,X]|HranyVystup]) :- zpatneHrany(Hrany,HranyVystup).

/**
* vytvori pociatocnu maticu tokov
* matica ma velkost (pocet vrcholov)*(pocet vrcholov)
* inicializujTok(+Vrcholy, -MaticaTokov)
* @arg Vrcholy - zoznam vrcholov, pomocou ktoreho indexujem maticu
* @arg MaticaTokov - vystupna matica so pociatocnym tokom
*/
inicializujTok(Vrcholy, MaticaTokov) :- length(Vrcholy, N),
                                        postavMaticu(N,N,MaticaTokov, []).

/**
* rekurzivne postavi maticu NxM, s pociatocnou hodnotou 0
* pomocny predikat pre inicializujTok
* postavMaticu(+N, +M, -MaticaTokov, ?Matica)
* @arg N - vyska matice
* @arg M - sirka matice
* @arg MaticaTokov - vysledna matica
* @arg Matica - do premennej Matica sa postupne buduje vysledna MaticaTokov
*/
postavMaticu(0, _, MaticaTokov, MaticaTokov).
postavMaticu(X, Y, MaticaTokov, Matica) :- postavRiadok(0, Y, Riadok),
                                           X1 is X - 1,
                                           postavMaticu(X1, Y, MaticaTokov, [Riadok|Matica]).

/**
* vytvori jeden riadok matice, s dlzkou N a pociatocnou hodnotou X
* postavRiadok(+X, +N, -List)
* @arg X - pociatocna hodnota, zvycajne 0
* @arg N - dlkzka riadka matice
* @arg List - vysledny riadok matice
*/
postavRiadok(X, N, List)  :- length(List, N),
                             maplist(=(X), List).

/******************** 2. Hladanie maximalneho toku ****************************/


/**
* najde v sieti zlepsujuci tok a zlepsi ho
* maxtok(-Toky, +,Hrany, +Vrcholy, +InitTok)
* @arg InitTok - pociatocny tok
* @arg Toky - maximalny tok
* @arg Hrany - hrany tvoriace celu siet
* @arg Vrcholy - zoznam vrcholov siete
*/
maxtok(Toky, Hrany, Vrcholy, InitTok) :- once(postavUrovnovuSiet(UrovnoveHrany, InitTok, Hrany, Vrcholy)), % postavi urovnovu siet
                                         UrovnoveHrany \= [] -> % ak su nejake hrany v urovnovej sieti
                                         (once(najdiaZlepsi(UrovnoveHrany, Vrcholy, InitTok, NoveToky)), % najde na urovnovej sieti vsetky zlepsujuce cesty a zvysi po nich tok
                                          maxtok(Toky, Hrany, Vrcholy, NoveToky)); % rekurzivne volanie na dalsi zlepsenie
                                         Toky = InitTok. % ak uz v urovnovej sieti nie su ziadne hrany, nasli sme maximalny tok

/******************* 2.a Vytvorenie urovnovej siete ***************************/

/**
* postavi urovnovu siet
* postavUrovnovuSiet(-Sorted, +Toky, +Hrany, +Vrcholy)
* @arg Toky - vstupna matica tokov
* @arg Sorted - zotriedene hrany, ktore sa nachadzaju v urovnovej sieti
* @arg Hrany - hrany tvoriace celu siet
* @arg Vrcholy - zoznam vrcholov siete
*/
postavUrovnovuSiet(Sorted, Toky, Hrany, Vrcholy) :- findall(Cesta, bfs(Cesta, Toky, Hrany, Vrcholy), Cesty), % najde vsetky pripustne cesty
                                                    najdiMin(Cesty, Min, 100), % zo vsetkych ciest najde tu najkratsiu
                                                    lenMin(Cesty, Min, MinCesty, []), % zo vsetkych ciest odstrani vsetky dlhsie ako ta najkratsia
                                                    nahradHranami(MinCesty, NoveMinCesty, Hrany), % nahradi dvojice vrcholov hranami, ktore ich spajaju
                                                    sort(NoveMinCesty, Sorted). % sort, popri zotriedeni hran odstrani duplicity


/**
* bfs najde vsetky pripustne cesty pomocou prehladavania do sirky
* bfs(-Cesta, +Toky, +Hrany, +Vrcholy)
* @arg Cesta - vysledna cesta zo zdroja do stoku
* @arg Toky - matica tokov
* @arg Hrany - hrany tvoriace celu siet
* @arg Vrcholy - zoznam vrcholov siete
*/
bfs(Cesta, Toky, Hrany, Vrcholy):- bfs1([[s]|Z]-Z,CestaR, Toky, Hrany, Vrcholy),
                       reverse(CestaR, Cesta).

/**
* pomocny predikat pre bfs
* bfs1(+Fronta, -Cesta, +Toky, +Hrany, +Vrcholy)
* @arg Fronta - fronta vrcholov pre prehladavanie do sirky
* @arg Toky - matica tokov
* @arg Hrany - hrany tvoriace celu siet
* @arg Vrcholy - zoznam vrcholov siete
*/
bfs1([Xs|_]-_, Xs, _, _, _):- Xs=[t|_]. %koniec rekurzie, vo fronte sa nachadza koncovy vrchol
bfs1([[X|Xs]|Xss]-Z, Cesta, Toky, Hrany, Vrcholy) :- findall([Y,X|Xs],
                                                             (member(K-[X,Y], Hrany),
                                                             nth0(C1, Vrcholy, X),
                                                             nth0(C2, Vrcholy, Y),
                                                             nth0(C1, Toky, List),
                                                             nth0(C2, List, Tok),
                                                             Tok < K,
                                                             \+ member(Y,[X|Xs])), % pripustne hrany maju tok v matici tokov ostro mensi ako kapacita hrany
                                                            NoveCesty),
                                                     append(NoveCesty, ZZ, Z), !,
                                                     Xss \== ZZ,
                                                     bfs1(Xss-ZZ, Cesta, Toky, Hrany, Vrcholy).

/**
* najde minimalnu dlzku zoznamu v zozname zoznamov
* najdiMin(+Cesty, -Min, ?PociatocneMin)
* @arg Cesty - zoznam ciest, z ktorych hladame ten s najkratsou dlzkou
* @arg Min - dlzka najkratsieho zoznamu
* @arg PociatocneMin - pociatocna hodnota Min
*/
najdiMin([], Min, Min).
najdiMin([X|Xs], Min, Z) :- length(X, Lx),
                            NZ is min(Z,Lx),
                            najdiMin(Xs, Min, NZ).

/**
* vo vystupnom zoznamu budu len listy zo zadanou dlzkou
* lenMin(+Cesty, +Min, -MinCesty, ?PocMinCesty)
* @arg Cesty - zoznam ciest, z ktorych chceme odstranit tie dlhsie ako Min
* @arg Min - dlzka najkratsej pripustnej cesty
* @arg MinCesty - vysledny zoznam obsahujuci len najkratsie cesty
* @arg PocMinCesty - pociatocny zoznam minimalnych ciest
*/
lenMin([], _, Y, Y).
lenMin([X|Xs], Min, Y, Z) :- (length(X, Min), append([X], Z, NZ), lenMin(Xs, Min, Y, NZ)); % ak je zoznam X dlzky Min pridam ho do zoznamu najkratsich ciest
                             (length(X, NMin), NMin \= Min, lenMin(Xs, Min, Y, Z) ).

/**
* nahradi dvojice vrcholov ich hranami v zozname ciest
* nahradHranami(+Cesty, -CestysHranami, +Hrany)
* @arg Cesty - cesty, v ktorych treba nahradit dvojice vrcholov hranami
* @arg CestysHranami - vysledne nahradene cesty
* @arg Hrany - zoznam hran, ktore tvoria siet
*/
nahradHranami([], [], _).
nahradHranami([X|Xs], Vystup, Hrany) :- nahradCestu(X, CestaH, [], Hrany),
                                        nahradHranami(Xs, CastVystupu, Hrany),
                                        append(CestaH, CastVystupu, Vystup).

/**
* nahradi dvojice vrcholov ich hranami v jednej ceste
* nahradCestu(+Cesta, -CestasHranami, ?PocCestasHranami, +Hrany)
* @arg Cesta - cesta, ktorej dvojice vrcholov nahradime hranami
* @arg CestasHranami - vysledna nahradena cesta
* @arg PocCestasHranami - pociatocna cesta s nahradenimi hranami
* @arg Hrany - zoznam hran, ktore tvoria siet
*/
nahradCestu(X, CestaH, InitCesta, Hrany) :- length(X, 2), head(X, Y), last(X, Z), member(K-[Y,Z],Hrany), % cesta je tvorena uz len poslednymi vrcholmi
                                            append([K-[Y,Z]], InitCesta, CestaH).
nahradCestu([X|Xs], CestaH, InitCesta, Hrany) :- head(Xs, Y), member(K-[X,Y], Hrany),
                                                 append([K-[X,Y]],InitCesta, NovaCesta),
                                                 nahradCestu(Xs, CestaH, NovaCesta, Hrany).
                                                 % nahradim, prve dva vrcholy ich hranou a rekurzivne nahradim cestu bez prveho vrcholu

/**
* head(+List, -Hlava)
* Hlava je hlava listu List
*/
head([Y],Y):-!.
head([X|_],X).

/********************* 2.b Zlepsenie vsetkych zlepsujucich ciest **************/

/**
* najde vsetky zlepsujuce cesty pomocou dfs a zlepsi po nich tok
* najdiaZlepsi(+Hrany, +Vrcholy, +Toky, -NoveToky)
* @arg Hrany - zoznam hran, ktore tvoria urovnovu siet
* @arg Vrcholy - zoznam vrcholov siete
* @arg Toky - matica pociatocnych tokov
* @arg NoveToky - matica zlepsenych tokov
*/
najdiaZlepsi(Hrany, Vrcholy, Toky, NoveToky) :- (dfs(Cesta,Hrany,Vrcholy,Toky,RTok) ->
                                                    (nahradCestu(Cesta,NovaCesta,[],Hrany), !,
                                                     zlepsiCestu(NovaCesta,Toky,ZlepseneToky,RTok,Vrcholy), !,
                                                     najdiaZlepsi(Hrany,Vrcholy,ZlepseneToky,NoveToky), !);
                                                 Toky = NoveToky).

/**
* dfs najde cestu zo zdroja do stoku a jej rezidualny tok pomocou prehladavania do hlbky
* dfs(-Cesta, +Hrany, +Vrcholy, +Toky, -RTok)
* @arg Cesta - cesta v grafe
* @arg Hrany - hrany, ktore tvoria urovnovu siet
* @arg Vrcholy - zoznam vrcholov siete
* @arg Toky - matica tokov v sieti
* @arg RTok - minimalny rezidualny tok po hladanej ceste
*/
dfs(Cesta,Hrany,Vrcholy,Toky,RTok):-dfs1(s,t,[s],C, Hrany,Vrcholy,Toky,1,RTok),
                                        reverse(C,Cesta).

/**
* pomocny predikat pre dfs
* dfs1(+X, +Y, +Navstivene, -Cesta, +Hrany, +Vrcholy, +Toky, +InitRTok, -RTok)
* @arg X - prave skumany vrchol
* @arg Y - koncovy vrchol
* @arg Navstivene - zoznam uz Navstivenych vrcholov
* @arg Cesta - hladana cesta v sieti
* @arg Hrany - hrany, ktore tvoria urovnovu siet
* @arg Vrcholy - zoznam vrcholov siete
* @arg Toky - matica tokov v sieti
* @arg InitRTok - pociatocny rezidualny tok
* @arg RTok - minimalny rezidualny tok po hladanej ceste
*/
dfs1(X,X,C,C, _,_,_,RTok,RTok).
dfs1(X,Z,Nav,C,Hrany,Vrcholy,Toky,InitRTok,RTok):- member(K-[X,Y], Hrany),
                                                   nth0(C1, Vrcholy, X),
                                                   nth0(C2, Vrcholy, Y),
                                                   nth0(C1, Toky, List),
                                                   nth0(C2, List, Tok),
                                                   CiastRTok is K - Tok,
                                                   CiastRTok \= 0,
                                                   NInitRTok is min(InitRTok, CiastRTok),
                                                   \+member(Y,Nav),
                                                   dfs1(Y,Z,[Y|Nav],C, Hrany,Vrcholy,Toky,NInitRTok,RTok).

/**
* zlepsi tok po vsetkych hranach na ceste
* zlepsiCestu(+Cesta, +Toky, -ZlepseneToky, +RezidualnyTok, +Vrcholy)
* @arg Cesta - cesta, ktoru chceme zlepsit
* @arg Toky - matica tokov, ktore chceme zlepsit
* @arg ZlepseneToky - matica tokov po zlepseni
* @arg RezidualnyTok - hodnota o ktoru chceme tok zlepsit
* @arg Vrcholy - zoznam vrcholov siete
*/
zlepsiCestu([], NoveToky, NoveToky, _, _).
zlepsiCestu([_-[X,Y]|Xs], Toky, NoveToky, RTok, Vrcholy) :- nth0(C1, Vrcholy, X),
                                                          nth0(C2, Vrcholy, Y),
                                                          Min is min(C1,C2),
                                                          Max is max(C1,C2),
                                                          Dif is Max - Min - 1,
                                                          prvychN(Min, Toky, PrvychN, [], [A|As]),
                                                          prvychN(Max, A, Hrana, [], [B|Bs]),
                                                          prvychN(Dif, As, DalsichN, [], [C|Cs]),
                                                          prvychN(Min, C, OpHrana, [], [D|Ds]),
                                                          % rozlozime maticu aby sme ziskali, riadky a hrany, ktore potrebujeme

                                                          ( Min = C1 ->
                                                            (NovyTok is B + RTok,
                                                             NovyOpTok is D - RTok,
                                                             append(Hrana, [NovyTok|Bs], NoveHrany),
                                                             append(PrvychN, [NoveHrany|DalsichN], NovaCast),
                                                             append(OpHrana, [NovyOpTok| Ds], NoveOpHrany),
                                                             append(NovaCast, [NoveOpHrany|Cs], Toky2));
                                                             % zlozime maticu naspat dohromady, s novou hodnotou toku na danej hrane

                                                           (NovyTok is D + RTok,
                                                           NovyOpTok is B - RTok,
                                                           append(Hrana, [NovyOpTok|Bs], NoveHrany),
                                                           append(PrvychN, [NoveHrany|DalsichN], NovaCast),
                                                           append(OpHrana, [NovyTok| Ds], NoveOpHrany),
                                                           append(NovaCast, [NoveOpHrany|Cs], Toky2))),
                                                           % zlozime maticu naspat dohromady, s novou hodnotou toku na danej hrane
                                                          zlepsiCestu(Xs, Toky2, NoveToky, RTok, Vrcholy). % rekurzivne zlepsime dalsie hrany na ceste

/**
* rozdeli zoznam na prvych n prvkov a zvysok
* prvychN(+N, +Zoznam, -PrvychN, +Zaciatok, -Zvysok)
* @arg N - pocet prvkov ktore potrebujeme
* @arg Zoznam - zoznam, ktory chceme rozdelit
* @arg PrvychN - prvych n prvkov
* @arg Zaciatok - pociatocna hodnota prvych n prvkov
* @arg Zvysok - zvysok zoznamu po odobrati prvych n prvkov
*/
prvychN(_, [], PrvychN, PrvychN, []).
prvychN(0, Zvysok, PrvychN, PrvychN, Zvysok).
prvychN(N, [X|Xs], PrvychN, Cast, Zvysok) :- N \= 0,
                                     append(Cast, [X], NovaCast),
                                     N1 is N - 1,
                                     prvychN(N1, Xs, PrvychN, NovaCast, Zvysok).



/********************* 3. Hladanie parovania ********************************************************/


/**
* pomocou matice obsahujucej maximalne toky po hranach, najde maximalne parovanie
* parovanie(+MaxToky, +Vrcholy, -Parovanie)
* @arg MaxToky - matica maximalnych tokov
* @arg Vrcholy - zoznam vrcholov siete
* @arg Parovanie - zoznam parov vrcholov
*/
parovanie1(MaxToky, Vrcholy, Parovanie) :- najdiParyMatica(MaxToky, 0, Parovanie, [], Vrcholy).

/**
* pomocny predikat pre parovanie
* najdiParyMatica(+MaxToky, +N, -Parovanie, +PociatocneParovanie, +Vrcholy)
* @arg MaxToky - matica maximalnych tokov
* @arg N - index vrcholu, ktoreho hrany prehliadam
* @arg Parovanie - zoznam parov vrcholov
* @arg PociatocneParovanie - pociatocny zoznam parov
* @arg Vrcholy - zoznam vrcholov siete
*/
najdiParyMatica([], _, Parovanie, Parovanie, _).
najdiParyMatica([X|Xs], N, Parovanie, InitParovanie, Vrcholy) :- N1 is N + 1,
                                                                 nth0(N, Vrcholy, V),
                                                                 (V \= s -> % pary obsahujuce zdroj nas nezaujimaju
                                                                   (najdiParyRiadok(X, 0, V, RParovanie, [], Vrcholy),
                                                                    append(RParovanie, InitParovanie, NoveParovanie),
                                                                    % najdeme pary v danom riadku a pridame ich do celkoveho zoznamu parov
                                                                    najdiParyMatica(Xs, N1, Parovanie, NoveParovanie, Vrcholy)); % rekurzivne prejdem dalsie riadky matice
                                                                  najdiParyMatica(Xs, N1, Parovanie, InitParovanie, Vrcholy)).

/**
* pomocny predikat pre parovanie
* najdiParyRiadok(+Riadok, +Cislo, +Vrchol, -Parovanie, +PociatocneParovanie, +Vrcholy)
* @arg Riadok - riadok matice tokov, v ktorom hladam pary
* @arg Cislo - cislo vrcholu, ktory prave prehliadam
* @arg Vrchol - vrchol, ktoreho hrany prehliadam
* @arg Parovanie - zoznam parov
* @arg PociatocneParovanie - pociatocny zoznam parov
* @arg Vrcholy - zoznam vrcholov siete
*/
najdiParyRiadok([], _, _, RParovanie, RParovanie, _).
najdiParyRiadok([X|Xs], N, V, RParovanie, InitRParovanie, Vrcholy) :- N1 is N + 1,
                                                                      nth0(N, Vrcholy, V2),
                                                                      (X = 1, V2 \= t ->
                                                                       % ak je tok po hrane 1 a koncovy vrchol hrany nie je stok, pridam vrcholy do parovania
                                                                       (append([V-V2], InitRParovanie, NoveParovanie),
                                                                        najdiParyRiadok(Xs, N1, V, RParovanie, NoveParovanie, Vrcholy));
                                                                       najdiParyRiadok(Xs, N1,V, RParovanie, InitRParovanie, Vrcholy)).


/********************* 4. Hlavna cast programu *********************************************************/


%bigraph(3,[1,2,3],[4,5,6],[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5]]).

/**
* najde parovanie na bipartitnom grafe zo suboru s testovacimi datami
* biMatching(+CisloGrafu, -Parovanie)
*/
parovanie(N, Parovanie) :- consult('testData.pl'),
                           bigraph(N, A, B, H),
                           postavSiet([A, B], H, Vrcholy, Hrany),
                           inicializujTok(Vrcholy, Toky),
                           maxtok(MaxToky, Hrany, Vrcholy, Toky),
                           parovanie1(MaxToky, Vrcholy, Parovanie), !.

/**
* najde parovanie na rucne zadanom bipartitnom grafe
* biMatching(+PrvaPartita, +DruhaPartita, +Hrany, -Parovanie)
*/
parovanie(A,B,H,Parovanie) :- postavSiet([A, B], H, Vrcholy, Hrany),
                              inicializujTok(Vrcholy, Toky),
                              maxtok(MaxToky, Hrany, Vrcholy, Toky),
                              parovanie1(MaxToky, Vrcholy, Parovanie), !.
