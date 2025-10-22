:- encoding(utf8).
% ui_resultado.pl — Ventana de resultado (estilo limpio y colores seguros)

:- use_module(library(pce)).
:- ensure_loaded(util).
:- ensure_loaded(engine).
:- ensure_loaded(storage).

res_color(bg,          lightgrey).
res_color(header_bg,   blue).
res_color(header_txt,  white).
res_color(card_bg,     white).

res_font(title,  font(helvetica, bold, 18)).
res_font(btn,    font(helvetica, bold, 12)).

ui_resultado_mostrar :-
    safe_engine_eval(Dx, Conf, Explic),
    term_string(Dx, DxS),
    ( number(Conf) -> format(string(ConfStr), 'Certeza (0-1): ~2f', [Conf])
                    ;  ConfStr = 'Certeza: desconocida' ),
    ( is_list(Explic) -> atomic_list_concat(Explic, '\n', ExpTxt)
                        ; term_string(Explic, ExpTxt) ),

    new(W, dialog('Resultado')),
    send(W, size, size(640, 300)),
    res_color(bg, BG), send(W, background, BG),

    % Header
    new(Header, device),
    new(HBox, box(600, 70)), send(HBox, radius, 10),
    res_color(header_bg, HB), send(HBox, fill_pattern, colour(HB)),
    send(Header, display, HBox, point(20, 20)),
    res_font(title, FT), res_color(header_txt, HT),
    new(T, text('Diagnostico sugerido: ')),
    send(T, font, FT), send(T, colour, HT),
    send(Header, display, T, point(40, 35)),
    new(TDx, text(DxS)),
    send(TDx, font, FT), send(TDx, colour, HT),
    send(Header, display, TDx, point(280, 35)),
    send(W, append, Header),

    % Card con detalle
    new(Card, device),
    new(CardBox, box(600, 140)), send(CardBox, radius, 10),
    res_color(card_bg, CB), send(CardBox, fill_pattern, colour(CB)),
    send(Card, display, CardBox, point(20, 100)),
    new(TConf, text(ConfStr)), send(Card, display, TConf, point(40, 120)),
    new(TExp,  text(ExpTxt)),  send(Card, display, TExp,  point(40, 150)),
    send(W, append, Card),

    % Botonera (usar set/2 con X,Y)
    new(B1, button('Guardar', message(@prolog, ui_guardar_evaluacion, W, Dx, Conf))),
    new(B2, button('Editar datos', message(@prolog, volver_a_intake_para_editar, W))),
    new(B3, button('Menu', message(@prolog, volver_menu_desde_resultado, W))),
    res_font(btn, FB),
    send(B1, font, FB), send(B2, font, FB), send(B3, font, FB),
    send(W, append, B1), send(W, append, B2), send(W, append, B3),
    send(B1, set, 260, 250),
    send(B2, set, 360, 250),
    send(B3, set, 500, 250),

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


