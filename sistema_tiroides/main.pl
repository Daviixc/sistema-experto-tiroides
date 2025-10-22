:- encoding(utf8).
% main.pl — Punto de entrada

:- style_check(-singleton).

:- consult('config.pl').
:- consult('knowledge.pl').
:- consult('util.pl').
:- consult('rules.pl').
:- consult('engine.pl').
:- consult('storage.pl').
:- consult('ui_main.pl').
:- consult('ui_intake.pl').
:- consult('ui_sintomas.pl').
:- consult('ui_labs.pl').
:- consult('ui_resultado.pl').
:- consult('pacientes_db.pl').    % DB de consultas
:- consult('expediente_db.pl').    % DB de expedientes (nuevo)

start :-
    limpiar_sesion,
    ui_mostrar_menu.
