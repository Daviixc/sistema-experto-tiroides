:- encoding(utf8).
% ui_main.pl — Menú principal

:- use_module(library(pce)).
:- ensure_loaded(util).

ui_mostrar_menu :-
    new(W, dialog('Sistema experto: Tiroides')),
    send(W, append, new(T, text('Diagnóstico sugerido (hiper/hipo)'))),
    send(T, font, font(helvetica, bold, 14)),
    send(W, append, button('Nueva evaluación',
            message(@prolog, ui_iniciar_evaluacion, W))),
    send(W, append, button('Historial',
            message(@prolog, ui_historial))),
    send(W, append, button('Configuración',
            message(@prolog, ui_configuracion))),
    send(W, append, button('Salir', message(W, destroy))),
    send(W, open_centered).

ui_iniciar_evaluacion(W) :-
    (object(W) -> send(W, destroy) ; true),
    limpiar_sesion,
    ui_intake_mostrar.

ui_historial :-
    findall(ID, paciente(ID, _, _, _, _), L),
    length(L, N),
    message_box(info, 'Historial',
        ['Registros guardados: ', N, '\n',
         '(En próximas versiones: vista de detalle)']).

ui_configuracion :-
    obtener_config(paths(db_pl, DBPL)),
    obtener_config(paths(csv, CSV)),
    obtener_config(paths(json, JSON)),
    message_box(info, 'Configuración',
        ['Rutas:\n- DB PL: ', DBPL,
         '\n- CSV: ', CSV,
         '\n- JSON: ', JSON,
         '\n(Editar en config.pl)']).
