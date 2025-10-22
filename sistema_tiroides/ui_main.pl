:- encoding(utf8).
% ui_main.pl — Menú principal (estilo grande con colores seguros)

:- use_module(library(pce)).
:- ensure_loaded(util).

% Paleta y fuentes (nombres únicos para evitar redefinición)
main_color(bg,          lightgrey).
main_color(header_bg,   blue).
main_color(header_txt,  white).
main_color(card_bg,     white).
main_color(text,        black).
main_color(btn_txt,     black).

main_font(title,  font(helvetica, bold, 22)).
main_font(sub,    font(helvetica, normal, 12)).
main_font(btn,    font(helvetica, bold,   14)).

ui_mostrar_menu :-
    new(W, dialog('Sistema experto: Tiroides')),
    send(W, size, size(720, 460)),
    main_color(bg, BG), send(W, background, BG),

    % --- Encabezado (banner) ---
    new(Header, device),
    send(Header, size, size(680, 90)),
    new(Bar, box(680, 90)), send(Bar, radius, 10),
    main_color(header_bg, HB), send(Bar, fill_pattern, colour(HB)),
    send(Header, display, Bar, point(20, 20)),
    main_font(title, FT), main_color(header_txt, HT),
    new(T, text('Diagnostico de tiroides')),
    send(T, font, FT), send(T, colour, HT),
    send(Header, display, T, point(40, 40)),
    main_font(sub, FS),
    new(S, text('Seleccione una opcion para comenzar')),
    send(S, font, FS), send(S, colour, HT),
    send(Header, display, S, point(40, 70)),
    send(W, append, Header),

    % --- Contenedor "card" para botones ---
    new(Card, device),
    new(CardBox, box(680, 300)), send(CardBox, radius, 12),
    main_color(card_bg, CB), send(CardBox, fill_pattern, colour(CB)),
    send(Card, display, CardBox, point(20, 120)),

    % --- Botones grandes en grilla 2x2 ---
    ui_big_button(Card, 'Nueva evaluacion',  point(60,  140), message(@prolog, ui_nueva_eval, W)),
    ui_big_button(Card, 'Historial',         point(380, 140), message(@prolog, ui_historial, W)),
    ui_big_button(Card, 'Configuracion',     point(60,  230), message(@prolog, ui_config, W)),
    ui_big_button(Card, 'Salir',             point(380, 230), message(@prolog, ui_salir, W)),
    send(W, append, Card),

    % --- Pie / mini status ---
    main_font(sub, FS2),
    new(F, text('v1.0  ·  IA / Prolog + XPCE')),
    send(F, font, FS2),
    send(F, colour, colour(grey40)),
    send(W, append, F),
    send(F, set, 24, 436),

    send(W, open_centered).

% ==== Botón grande estilizado ====
ui_big_button(Parent, Label, point(X,Y), Msg) :-
    % Fondo del botón (box redondeado)
    new(Bx, box(260, 68)), send(Bx, radius, 12),
    send(Bx, fill_pattern, colour(white)),
    send(Parent, display, Bx, point(X, Y)),
    % Botón encima
    main_font(btn, FB), main_color(btn_txt, TXT),
    new(B, button(Label, Msg)),
    send(B, font, FB), send(B, colour, colour(TXT)),
    send(Parent, display, B, point(X+20, Y+18)).

% ==== Acciones ====
ui_nueva_eval(W) :-
    (object(W) -> send(W, destroy) ; true),
    limpiar_sesion,
    ui_intake_mostrar.

ui_historial(_W) :-
    message_box(info, 'Historial', ['En construccion: aqui se listaran consultas guardadas.']).

ui_config(_W) :-
    message_box(info, 'Configuracion', ['No hay opciones configurables por ahora.']).

ui_salir(W) :-
    (object(W) -> send(W, destroy) ; true).
