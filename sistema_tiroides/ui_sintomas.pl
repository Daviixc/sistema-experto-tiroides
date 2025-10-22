:- encoding(utf8).
% ui_sintomas.pl — Pestaña 2: Sintomas (estable con menu cycle NO/SI)

:- use_module(library(pce)).
:- ensure_loaded(util).

ui_sintomas_mostrar :-
    new(W, dialog('Sintomas y signos')),

    % Cada sintoma es un menu de tipo cycle con opciones NO/SI
    make_choice(W, 'Fatiga',                C1),
    make_choice(W, 'Intolerancia al frio',  C2),
    make_choice(W, 'Intolerancia al calor', C3),
    make_choice(W, 'Perdida de peso',       C4),
    make_choice(W, 'Ganancia de peso',      C5),
    make_choice(W, 'Palpitaciones',         C6),
    make_choice(W, 'Temblor',               C7),
    make_choice(W, 'Piel seca',             C8),
    make_choice(W, 'Estrenimiento',         C9),
    make_choice(W, 'Diarrea',               C10),
    make_choice(W, 'Bocio',                 C11),
    make_choice(W, 'Taquicardia',           C12),
    make_choice(W, 'Bradicardia',           C13),

    % Boton opcional para verificar (puedes quitarlo al final)
    send(W, append, button('Probar',
        message(@prolog, ui_sintomas_debug,
                C1?selection,C2?selection,C3?selection,C4?selection,C5?selection,
                C6?selection,C7?selection,C8?selection,C9?selection,C10?selection,
                C11?selection,C12?selection,C13?selection))),

    send(W, append, button('Volver',
        message(@prolog, volver_intake, W))),
    send(W, append, button('Siguiente',
        message(@prolog, ui_sintomas_guardar_y_seguir,
                W, C1?selection,C2?selection,C3?selection,C4?selection,C5?selection,
                   C6?selection,C7?selection,C8?selection,C9?selection,C10?selection,
                   C11?selection,C12?selection,C13?selection))),
    send(W, open_centered).

% Un menu cycle con valores NO/SI y NO por defecto.
make_choice(W, Etiqueta, Handle) :-
    send(W, append, new(Handle, menu(Etiqueta, cycle))),
    send(Handle, append, 'NO'),
    send(Handle, append, 'SI'),
    send(Handle, selection, 'NO').

volver_intake(W) :-
    (object(W)->send(W, destroy);true),
    ui_intake_mostrar.

ui_sintomas_guardar_y_seguir(W,S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13) :-
    maplist(choice_to_bool, [S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13], Bs),
    Claves = [fatiga, frio, calor, perdida_peso, ganancia_peso, palpitaciones, temblor, piel_seca,
              estrenimiento, diarrea, bocio, taquicardia, bradicardia],
    guardar_kv_booleans(Claves, Bs),
    send(W, destroy),
    ui_labs_mostrar.

% Verdadero SOLO cuando el valor es exactamente 'SI'
choice_to_bool('SI', true)  :- !.
choice_to_bool(_,    false).

guardar_kv_booleans([], []).
guardar_kv_booleans([K|Ks], [B|Bs]) :-
    set_sesion(K, B),
    guardar_kv_booleans(Ks, Bs).

% Debug opcional: muestra SI/NO antes de seguir
ui_sintomas_debug(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13) :-
    maplist(choice_to_bool, [S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13], Bs),
    Cls = ['Fatiga','Frio','Calor','Perdida peso','Ganancia peso','Palpitaciones',
           'Temblor','Piel seca','Estrenimiento','Diarrea','Bocio','Taquicardia','Bradicardia'],
    pares_a_lineas(Cls, Bs, Txt),
    message_box(info, 'Lectura de sintomas', [Txt]).

pares_a_lineas([], [], '').
pares_a_lineas([C|Cs], [B|Bs], S) :-
    (B==true -> V='SI' ; V='NO'),
    atomic_list_concat([C, ': ', V, '\n'], L),
    pares_a_lineas(Cs, Bs, R),
    atom_concat(L, R, S).






