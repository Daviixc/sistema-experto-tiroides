:- encoding(utf8).
% ui_labs.pl — Pestaña 3: Laboratorio

:- use_module(library(pce)).
:- ensure_loaded(util).

ui_labs_mostrar :-
    new(W, dialog('Laboratorio (opcional)')),
    send(W, append, new(TSH,  text_item('TSH (µIU/mL)'))),
    send(W, append, new(T4L,  text_item('T4 libre (ng/dL)'))),
    send(W, append, new(T3,   text_item('T3 (pg/mL)'))),
    send(W, append, new(SIN, menu('Sin labs', toggle))),
    send(SIN, append, 'SI'),
    send(W, append, button('Volver',
        message(@prolog, volver_sintomas, W))),
    send(W, append, button('Siguiente',
        message(@prolog, ui_labs_guardar_y_seguir,
                W, TSH?selection, T4L?selection, T3?selection, SIN?selection))),
    send(W, open_centered).

volver_sintomas(W) :-
    (object(W)->send(W, destroy);true),
    ui_sintomas_mostrar.

ui_labs_guardar_y_seguir(W, TSHs, T4Ls, T3s, SinLabsSel) :-
    ( SinLabsSel == 'SI' ->
        set_sesion(tsh, none),
        set_sesion(t4l, none),
        set_sesion(t3,  none)
    ;   str_a_num_o_none(TSHs, TSHv), set_sesion(tsh,  TSHv),
        str_a_num_o_none(T4Ls, T4Lv), set_sesion(t4l,  T4Lv),
        str_a_num_o_none(T3s,  T3v),  set_sesion(t3,   T3v)
    ),
    % Abrir Resultado con protección; si falla mostramos aviso
    (   catch(ui_resultado_mostrar, E,
              ( print_message(error, E),
                message_box(warn, 'Aviso',
                  ['Ocurrió un problema al generar el resultado.\n',
                   'Revisa los datos (usa punto decimal, ej. 2.5).']),
                fail))
    ->  true ; true ),
    (object(W) -> send(W, destroy) ; true).

/* Helpers parseo */
str_a_num_o_none(@nil, none) :- !.
str_a_num_o_none('',   none) :- !.
str_a_num_o_none(A0,   Num)  :-
    ( atom(A0)   -> atom_string(A0,S0)
    ; string(A0) -> S0 = A0
    ;              term_string(A0,S0)
    ),
    normalize_space(string(S), S0),
    ( S == "" -> Num = none
    ; sub_string(S, _, _, _, ",") ->
        split_string(S, ",", "", Ps),
        atomic_list_concat(Ps, '.', Sd),
        ( convertir_real(Sd, N) -> Num is N ; Num = none )
    ; ( convertir_real(S, N) -> Num is N ; Num = none )
    ).
    
