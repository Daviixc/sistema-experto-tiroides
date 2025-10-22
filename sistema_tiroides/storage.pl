:- encoding(utf8).
% storage.pl — Guardado / exportaciones

:- use_module(library(filesex)).
:- ensure_loaded(util).
:- ensure_loaded(config).
:- ensure_loaded(expediente_db).

storage_guardar_actual(Dx, Conf, _Explic) :-
    % Datos minimos de la consulta
    get_sesion(id, ID),         nonvar(ID), ID \= none,
    get_sesion(nombre, Nombre), nonvar(Nombre), Nombre \= none,
    get_sesion(edad, Edad),     nonvar(Edad), Edad \= none,

    time_stamp(Fecha),

    % Persiste/actualiza expediente maestro si no existe
    persistir_expediente_si_nuevo(ID, Nombre),

    % Guardar consulta en DB Prolog
    obtener_config(paths(db_pl, DBPath)),
    ensure_file(DBPath),
    setup_call_cleanup(
        open(DBPath, append, S, [encoding(utf8)]),
        ( format(S, 'paciente(~w,~q,~w,~w,fecha(~q)).~n', 
                [ID, Nombre, Dx, Conf, Fecha])
        ),
        close(S)
    ),
    assertz(paciente(ID, Nombre, Dx, Conf, fecha(Fecha))),

    % Exports
    exportar_csv(ID, Nombre, Dx, Conf, Fecha, _),
    exportar_json(ID, Nombre, Dx, Conf, Fecha, _).

% ---------- Persistencia del expediente ----------
persistir_expediente_si_nuevo(ID, Nombre) :-
    ( paciente_info(ID, _, _, _, _, _, _, _) ->
        true
    ;   % Construir expediente desde la sesión y guardar
        get_sesion(sexo,    Sexo),     (Sexo == none -> SexoVal = 'M' ; SexoVal = Sexo),
        get_sesion(edad,    Edad),     (Edad == none -> EdadVal = 0 ; EdadVal = Edad),
        get_sesion(talla_cm,Talla),    (Talla == none -> TallaVal = 0 ; TallaVal = Talla),
        get_sesion(peso_kg, Peso),     (Peso == none -> PesoVal = 0 ; PesoVal = Peso),
        get_sesion(contacto,Contacto), (Contacto == none -> ContactoVal = '' ; ContactoVal = Contacto),
        get_sesion(notas,   Notas),    (Notas == none -> NotasVal = '' ; NotasVal = Notas),

        ensure_file('expediente_db.pl'),
        setup_call_cleanup(
            open('expediente_db.pl', append, S, [encoding(utf8)]),
            ( format(S, 'paciente_info(~w,~q,~q,~w,~w,~w,~q,~q).~n',
                    [ID, Nombre, SexoVal, EdadVal, TallaVal, PesoVal, ContactoVal, NotasVal])
            ),
            close(S)
        ),
        assertz(paciente_info(ID, Nombre, SexoVal, EdadVal, TallaVal, PesoVal, ContactoVal, NotasVal))
    ).

% ---------- Exports ----------
exportar_csv(ID, Nombre, Dx, Conf, Fecha, true) :-
    obtener_config(paths(csv, CSV)),
    ensure_file(CSV),
    setup_call_cleanup(
        open(CSV, append, S, [encoding(utf8)]),
        format(S, '"~w","~w","~w",~2f,"~w"~n', [ID, Nombre, Dx, Conf, Fecha]),
        close(S)
    ), !.
exportar_csv(_,_,_,_,_, false).

exportar_json(ID, Nombre, Dx, Conf, Fecha, true) :-
    obtener_config(paths(json, JSON)),
    ensure_file(JSON),
    setup_call_cleanup(
        open(JSON, append, S, [encoding(utf8)]),
        format(S, '{"id":"~w","nombre":"~w","dx":"~w","conf":~2f,"fecha":"~w"}~n',
               [ID, Nombre, Dx, Conf, Fecha]),
        close(S)
    ), !.
exportar_json(_,_,_,_,_, false).

% ---------- Utilidades ----------
time_stamp(Atom) :-
    get_time(T),
    format_time(atom(Atom), '%Y-%m-%dT%H:%M:%SZ', T).


