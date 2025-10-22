:- encoding(utf8).
% util.pl — Utilidades y estado de sesion

:- use_module(library(pce)).
:- dynamic sesion/2.

% ===== Estado de sesion =====
limpiar_sesion :- retractall(sesion(_, _)).

set_sesion(K, V) :-
    retractall(sesion(K, _)),
    assertz(sesion(K, V)).

get_sesion(K, V) :-
    ( sesion(K, V) -> true ; V = none ).

% ===== Validaciones / conversion =====
validar_numero(X) :- number(X), !.
validar_numero(X) :- atom(X),   atom_number(X, _), !.
validar_numero(X) :- string(X), number_string(_, X).

convertir_real(Str, Num) :-
    ( number(Str) -> Num is float(Str)
    ; atom(Str)   -> atom_number(Str, Num)
    ; string(Str) -> number_string(Num, Str)
    ), !.
convertir_real(_, _) :- fail.

% ===== Mensajes XPCE =====
message_box(_Kind, Title, Parts) :-
    maplist(term_string, Parts, Strs),
    atomic_list_concat(Strs, '', Body),
    format(string(Msg), "~w~n~w", [Title, Body]),
    send(@display, inform, Msg).

% ===== Archivos =====
ensure_file(Path) :-
    file_directory_name(Path, Dir),
    ( Dir \= '' -> make_directory_path(Dir) ; true ),
    ( exists_file(Path) -> true
    ; setup_call_cleanup(open(Path, write, S, [encoding(utf8)]), true, close(S))
    ).

% ===== ID autoincremental (si lo estas usando) =====
next_id(IDStr) :-
    findall(N, paciente(N,_,_,_,_), C1),
    ( current_predicate(paciente_info/8)
    -> findall(N2, paciente_info(N2,_,_,_,_,_,_,_), C2)
    ;  C2 = []
    ),
    append(C1, C2, Todos),
    maplist(id_to_int, Todos, Ints0),
    ( Ints0 = [] -> Max = 0 ; max_list(Ints0, Max) ),
    Siguiente is Max + 1,
    number_string(Siguiente, IDStr).

id_to_int(A, N) :-
    ( number(A) -> N is integer(A)
    ; atom(A)   -> (catch(atom_number(A, N), _, fail) -> true ; N = 0)
    ; string(A) -> (catch(number_string(N, A), _, fail) -> true ; N = 0)
    ; N = 0 ).

% ===== BMI / IMC =====
bmi_cmkg(TallaCm, PesoKg, BMI) :-
    number(TallaCm), number(PesoKg),
    TallaCm > 0, PesoKg > 0,
    M is TallaCm / 100.0,
    BMI is PesoKg / (M*M).

% ===== Debug rapido de sesion (opcional) =====
debug_sesion :- forall(sesion(K,V), (write(K=V), nl)).
