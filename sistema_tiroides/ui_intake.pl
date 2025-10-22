:- encoding(utf8).
% ui_intake.pl — Pestaña 1: Datos del paciente

:- use_module(library(pce)).
:- ensure_loaded(util).

ui_intake_mostrar :-
    new(W, dialog('Datos del paciente')),

    % ID autogenerado (no editable)
    next_id(NextID),
    send(W, append, new(Id, text_item('ID/Folio'))),
    send(Id, selection, NextID),
    send(Id, editable, @off),

    % Datos básicos
    send(W, append, new(Nombre, text_item('Nombre completo'))),
    send(W, append, new(Edad,   int_item('Edad'))),
    send(W, append, new(Sexo,   menu('Sexo', cycle))),
    send(Sexo, append, 'F'),
    send(Sexo, append, 'M'),
    send(Sexo, append, 'Otro'),

    % Datos de expediente
    send(W, append, new(Talla,  int_item('Talla (cm)'))),
    send(W, append, new(Peso,   text_item('Peso (kg)'))),
    send(W, append, new(Contact,text_item('Contacto (tel/email)'))),
    send(W, append, new(Notas,  text_item('Notas'))),

    send(W, append, button('Siguiente',
        message(@prolog, ui_intake_validar_guardar,
                W, Nombre?selection, Id?selection, Edad?selection, Sexo?selection,
                Talla?selection, Peso?selection, Contact?selection, Notas?selection))),
    send(W, append, button('Cancelar',
        message(@prolog, cancelar_a_menu, W))),
    send(W, open_centered).

ui_intake_validar_guardar(W, Nombre, Id, Edad, Sexo,
                           Talla, Peso, Contacto, Notas) :-
    (   nonvar(Nombre), Nombre \= @nil, atom_string(Nombre, SN), SN \= "",
        nonvar(Id), Id \= @nil, atom_string(Id, SID), SID \= "",
        nonvar(Edad), Edad \= @nil, validar_numero(Edad), Edad >= 0,
        nonvar(Sexo), Sexo \= @nil
    ->  % Sesión para consulta
        set_sesion(nombre, SN),
        set_sesion(id, SID),
        set_sesion(edad, Edad),
        set_sesion(sexo, Sexo),

        % Expediente en sesión
        ( nonvar(Talla), validar_numero(Talla) -> TallaN is Talla ; TallaN = 0 ),
        ( nonvar(Peso), convertir_real(Peso, PesoK) -> true ; PesoK = 0 ),
        set_sesion(talla_cm, TallaN),
        set_sesion(peso_kg,  PesoK),
        set_sesion(contacto, Contacto),
        set_sesion(notas,    Notas),

        send(W, destroy),
        ui_sintomas_mostrar
    ;   message_box(warn, 'Datos incompletos',
            ['Revisa nombre, ID, edad y sexo.'])
    ).

cancelar_a_menu(W) :-
    (object(W) -> send(W, destroy) ; true),
    ui_mostrar_menu.



