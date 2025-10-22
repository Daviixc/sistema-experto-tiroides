:- encoding(utf8).
% ui_resultado.pl — Ventana de resultado y guardado (sin acentos)

:- use_module(library(pce)).
:- ensure_loaded(util).
:- ensure_loaded(engine).
:- ensure_loaded(storage).

ui_resultado_mostrar :-
    safe_engine_eval(Dx, Conf, Explic),
    term_string(Dx, DxS),
    ( number(Conf) -> format(string(ConfStr), 'Certeza (0-1): ~2f', [Conf])
                    ;  ConfStr = 'Certeza: desconocida' ),
    ( is_list(Explic) -> atomic_list_concat(Explic, '\n', ExpTxt)
                        ; term_string(Explic, ExpTxt) ),
    atomic_list_concat(['Diagnostico sugerido: ', DxS], Header),

    new(W, dialog('Resultado')),
    send(W, append, new(T,  text(Header))),
    send(T, font, font(helvetica, bold, 14)),
    send(W, append, new(T2, text(ConfStr))),
    send(W, append, new(T3, text(ExpTxt))),
    send(W, append, button('Guardar',
        message(@prolog, ui_guardar_evaluacion, W, Dx, Conf))),
    send(W, append, button('Editar datos',
        message(@prolog, volver_a_intake_para_editar, W))),
    send(W, append, button('Menu',
        message(@prolog, volver_menu_desde_resultado, W))),
    send(W, open_centered).

safe_engine_eval(Dx, Conf, Explic) :-
    catch(engine_evaluar(Dx, Conf, Explic), E,
          ( print_message(error, E),
            Dx = indeterminado, Conf = 0.0,
            Explic = ['Error interno durante la evaluacion'] )).

ui_guardar_evaluacion(W, Dx, Conf) :-
    (   catch(storage_guardar_actual(Dx, Conf, []), E,
              ( print_message(error, E), fail ))
    ->  message_box(info, 'Guardado', ['Evaluacion guardada.']),
        (object(W) -> send(W, destroy) ; true),
        ui_mostrar_menu
    ;   message_box(warn, 'Error', ['No se pudo guardar. Revisa permisos.'])
    ).

volver_a_intake_para_editar(W) :-
    (object(W)->send(W, destroy);true),
    ui_intake_mostrar.

volver_menu_desde_resultado(W) :-
    (object(W)->send(W, destroy);true),
    ui_mostrar_menu.




