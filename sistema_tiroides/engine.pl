:- encoding(utf8).
% engine.pl — Motor de inferencia (sintomas + laboratorios + IMC, sin acentos)

:- ensure_loaded(util).

/* ========= API ========= */
engine_evaluar(Dx, Conf, Explic) :-
    % Leer labs de sesion (pueden ser 'none' o numero)
    get_sesion(tsh, TSH),
    get_sesion(t4l, T4L),

    % Clasificacion de labs
    clasificar_lab(tsh, TSH, CTsh),
    clasificar_lab(t4l, T4L, CT4),

    % 1) Reglas por laboratorios
    (   disparar_por_labs(CTsh, CT4, DxL, CL, EL)
    ->  Dx=DxL, Conf=CL, Explic=EL
    % 2) Reglas por sintomas
    ;   disparar_por_sintomas(DxS, CS, ES)
    ->  Dx=DxS, Conf=CS, Explic=ES
    % 3) Fallback
    ;   Dx=indeterminado, Conf=0.3,
        Explic=['Sintomas insuficientes para concluir']
    ).

/* ========= Clasificacion de laboratorio ========= */
clasificar_lab(_,   none,  none)   :- !.
clasificar_lab(tsh, V,     alta)   :- number(V), V > 4.0, !.
clasificar_lab(tsh, V,     baja)   :- number(V), V < 0.4, !.
clasificar_lab(tsh, _,     normal) :- !.
clasificar_lab(t4l, V,     alta)   :- number(V), V > 1.8, !.
clasificar_lab(t4l, V,     baja)   :- number(V), V < 0.8, !.
clasificar_lab(t4l, _,     normal) :- !.

/* ========= Reglas por laboratorio ========= */
disparar_por_labs(none, none, _, _, _) :- !, fail.
disparar_por_labs(CTSH, CT4L, Dx, Conf, Exp) :-
    (   CTSH = alta,  CT4L = baja   ->
        Dx=hipotiroidismo,            Conf=0.95,
        Exp=['TSH alta y T4L baja -> Hipotiroidismo primario']
    ;   CTSH = baja,  CT4L = alta   ->
        Dx=hipertiroidismo,           Conf=0.95,
        Exp=['TSH baja y T4L alta -> Hipertiroidismo']
    ;   CTSH = alta,  CT4L = normal ->
        Dx=hipotiroidismo_subclinico, Conf=0.75,
        Exp=['TSH alta y T4L normal -> Hipo subclinico']
    ;   CTSH = baja,  CT4L = normal ->
        Dx=hipertiroidismo_subclinico,Conf=0.75,
        Exp=['TSH baja y T4L normal -> Hiper subclinico']
    ), !.

/* ========= Reglas por sintomas =========
   Nucleo:
     HIPO: frio, piel_seca, ganancia_peso
     HIPER: calor, perdida_peso, temblor
   Modificadores (max +1 total):
     HIPO: bradicardia, IMC >= 30
     HIPER: (palpitaciones o taquicardia), IMC < 20 con perdida_peso
   Decision:
     nucleo >= 3                     -> probable (0.55)
     nucleo = 2 y +1 modificador     -> probable (0.55)
     nucleo = 2                      -> posible  (0.50)
     nucleo = 1 y +1 modificador     -> posible  (0.50)
     otro                            -> indeterminado (0.30)
*/
disparar_por_sintomas(Dx, Conf, Exp) :-
    % Valores booleanos seguros desde sesion
    b(frio, Frio), b(piel_seca, PielSeca), b(ganancia_peso, GPeso),
    b(bradicardia, Brad),
    b(calor, Calor), b(perdida_peso, PPeso),
    b(palpitaciones, Palp), b(taquicardia, Taqui),
    b(temblor, Temblor),

    % IMC si existe
    get_sesion(talla_cm, Talla), get_sesion(peso_kg, Peso),
    ( bmi_cmkg(Talla, Peso, BMI) -> true ; BMI = none ),

    % --- HIPO ---
    count_true([Frio, PielSeca, GPeso], CoreHipo),
    (Brad==true -> M1=1 ; M1=0),
    (number(BMI), BMI>=30.0 -> M2=1 ; M2=0),
    ModHipo is min(1, M1+M2),

    (   CoreHipo >= 3 ->
        Dx=hipotiroidismo_probable, Conf=0.55,
        make_exp('hipotiroidismo', CoreHipo, ModHipo, Exp)
    ;   CoreHipo =:= 2, ModHipo >= 1 ->
        Dx=hipotiroidismo_probable, Conf=0.55,
        make_exp('hipotiroidismo', CoreHipo, ModHipo, Exp)
    ;   CoreHipo =:= 2 ->
        Dx=hipotiroidismo_posible,  Conf=0.50,
        make_exp('hipotiroidismo', CoreHipo, ModHipo, Exp)
    ;   CoreHipo =:= 1, ModHipo >= 1 ->
        Dx=hipotiroidismo_posible,  Conf=0.50,
        make_exp('hipotiroidismo', CoreHipo, ModHipo, Exp)
    ;   % --- HIPER ---
        count_true([Calor, PPeso, Temblor], CoreHiper),
        ((Palp==true ; Taqui==true) -> H1=1 ; H1=0),
        (number(BMI), BMI<20.0, PPeso==true -> H2=1 ; H2=0),
        ModHiper is min(1, H1+H2),

        (   CoreHiper >= 3 ->
            Dx=hipertiroidismo_probable, Conf=0.55,
            make_exp('hipertiroidismo', CoreHiper, ModHiper, Exp)
        ;   CoreHiper =:= 2, ModHiper >= 1 ->
            Dx=hipertiroidismo_probable, Conf=0.55,
            make_exp('hipertiroidismo', CoreHiper, ModHiper, Exp)
        ;   CoreHiper =:= 2 ->
            Dx=hipertiroidismo_posible,  Conf=0.50,
            make_exp('hipertiroidismo', CoreHiper, ModHiper, Exp)
        ;   CoreHiper =:= 1, ModHiper >= 1 ->
            Dx=hipertiroidismo_posible,  Conf=0.50,
            make_exp('hipertiroidismo', CoreHiper, ModHiper, Exp)
        ;   Dx=indeterminado, Conf=0.3,
            Exp=['Sintomas insuficientes para concluir']
        )
    ).

/* ========= Helpers ========= */
b(K, true)  :- get_sesion(K, V), V == true, !.
b(_, false).

count_true(List, N) :-
    maplist(bool_to_int, List, Ints),
    sum_list(Ints, N).

bool_to_int(true, 1)  :- !.
bool_to_int(_,    0).

make_exp(DxTxt, Core, Mod, ExpAtom) :-
    number_string(Core, Cs),
    number_string(Mod,  Ms),
    atomic_list_concat(['Sintomas nucleo=',Cs,' + modificadores=',Ms,
                        ' compatibles con ', DxTxt], ExpAtom).
