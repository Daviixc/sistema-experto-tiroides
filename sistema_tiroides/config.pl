:- encoding(utf8).
% config.pl — Parámetros editables

obtener_config(paths(db_pl, 'pacientes_db.pl')).
obtener_config(paths(csv,   'export/pacientes.csv')).
obtener_config(paths(json,  'export/pacientes.json')).

obtener_config(pesos(labs, 0.7)).
obtener_config(pesos(sintomas, 0.3)).
