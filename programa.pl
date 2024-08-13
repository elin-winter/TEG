%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TEG %%%%%%%%%%%%%%%%%%%%%%%%%

continente(americaDelSur).
continente(americaDelNorte).
continente(asia).
continente(oceania).

estaEn(americaDelSur, argentina).
estaEn(americaDelSur, brasil).
estaEn(americaDelSur, chile).
estaEn(americaDelSur, uruguay).
estaEn(americaDelNorte, alaska).
estaEn(americaDelNorte, yukon).
estaEn(americaDelNorte, canada).
estaEn(americaDelNorte, oregon).
estaEn(asia, kamtchatka).
estaEn(asia, china).
estaEn(asia, siberia).
estaEn(asia, japon).
estaEn(oceania,australia).
estaEn(oceania,sumatra).
estaEn(oceania,java).
estaEn(oceania,borneo).

jugador(amarillo).
jugador(magenta).
jugador(negro).
jugador(blanco).

aliados(X,Y):- alianza(X,Y).
aliados(X,Y):- alianza(Y,X).
alianza(amarillo,magenta).

%el numero son los ejercitos
ocupa(argentina, magenta, 5).
ocupa(chile, negro, 3).
ocupa(brasil, amarillo, 8).
ocupa(uruguay, magenta, 5).
ocupa(alaska, amarillo, 7).
ocupa(yukon, amarillo, 1).
ocupa(canada, amarillo, 10).
ocupa(oregon, amarillo, 5).
ocupa(kamtchatka, negro, 6).
ocupa(china, amarillo, 2).
ocupa(siberia, amarillo, 5).
ocupa(japon, amarillo, 7).
ocupa(australia, negro, 8).
ocupa(sumatra, negro, 3).
ocupa(java, negro, 4).
ocupa(borneo, negro, 1).

% Usar este para saber si son limitrofes ya que es una relacion simetrica
sonLimitrofes(X, Y) :- limitrofes(X, Y).
sonLimitrofes(X, Y) :- limitrofes(Y, X).

limitrofes(argentina,brasil).
limitrofes(argentina,chile).
limitrofes(argentina,uruguay).
limitrofes(uruguay,brasil).
limitrofes(alaska,kamtchatka).
limitrofes(alaska,yukon).
limitrofes(canada,yukon).
limitrofes(alaska,oregon).
limitrofes(canada,oregon).
limitrofes(siberia,kamtchatka).
limitrofes(siberia,china).
limitrofes(china,kamtchatka).
limitrofes(japon,china).
limitrofes(japon,kamtchatka).
limitrofes(australia,sumatra).
limitrofes(australia,java).
limitrofes(australia,borneo).
limitrofes(australia,chile).


%%%%%%%%%%%%%%%%%%%%%%%%%%%% Parte A

% Punto 1

loLiquidaron(Jugador):-
    jugador(Jugador),
    not(ocupa(_, Jugador, _)).

% Punto 2

ocupaContinente(Jugador, Continente):-
    continente(Continente),
    jugador(Jugador),
    forall(estaEn(Continente, Pais), ocupa(Pais, Jugador, _)).

% Punto 3

seAtrinchero(Jugador):-
    jugador(Jugador),
    ocupa(Pais, Jugador, _),
    estaEn(Continente, Pais),
    forall(ocupa(P, Jugador, _), estaEn(Continente, P)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Parte B

% Punto 4

puedeConquistar(Jugador, Continente) :-
    jugador(Jugador),
    continente(Continente),
    not(ocupaContinente(Jugador, Continente)),
    forall(paisQueNoTiene(Jugador, Continente, Pais), puedeOcupar(Jugador, Pais)).

paisQueNoTiene(Jugador, Continente, Pais):-
    estaEn(Continente, Pais),
    not(ocupa(Pais, Jugador, _)).

puedeOcupar(Jugador, Pais):-
    ocupa(OtroPais, Jugador, _),
    sonLimitrofes(OtroPais, Pais),
    not((aliados(Jugador, Aliado), ocupa(Pais, Aliado, _))).


% Punto 5

elQueTieneMasEjercitos(Jugador, Pais):-
    jugador(Jugador),
    ocupa(Pais, Jugador, Max),
    forall(
        (ocupa(OtroPais, _, Cant), OtroPais \= Pais), 
        Cant =< Max).

% Punto 6

objetivo(amarillo, ocuparContinente(asia)).
objetivo(amarillo, ocuparPaises(2, americaDelSur)). 
objetivo(blanco, destruirJugador(negro)). 
objetivo(magenta, destruirJugador(blanco)). 
objetivo(negro, ocuparContinente(oceania)).
objetivo(negro, ocuparContinente(americaDelSur)).

cumpleObjetivo(Jugador):-
    jugador(Jugador),
    forall(objetivo(Jugador, Objetivo), loCumple(Jugador, Objetivo)).

loCumple(Jugador, ocuparContinente(Continente)):-
    ocupaContinente(Jugador, Continente).

loCumple(Jugador, ocuparPaises(Cant, Continente)):-
    findall(Pais, 
        (ocupa(Pais, Jugador, _), estaEn(Continente, Pais)), 
        Paises),
    length(Paises, CantPaises),
    CantPaises >= Cant. 

loCumple(_, destruirJugador(Victima)):-
    loLiquidaron(Victima).


% Punto 7

leInteresa(Jugador, Continente):-
    objetivo(Jugador, Objetivo),
    tieneQueVer(Objetivo, Continente).

tieneQueVer(ocuparContinente(Continente), Continente).
tieneQueVer(ocuparPaises(_, Continente), Continente).
tieneQueVer(destruirJugador(Jugador), Continente):-
    ocupa(Pais, Jugador, _),
    estaEn(Continente, Pais).
