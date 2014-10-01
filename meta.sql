-- Semilla seleccionada aleatoriamente:
select setseed(0.42);



create table "Evaluador"
  ( "nombre" text not null
  , primary key ("nombre")
  )
;

insert into "Evaluador"
  ("nombre"         ) values
  ('David Lilue'    )
, ('Karen Troiano'  )
, ('Manuel Gómez'   )
, ('Matteo Ferrando')
;



create table "Sección"
  ( "número" integer not null check ("número" >= 1)
  , primary key ("número")
  )
;

create table "Estudiante"
  ( "carné"   char(7) not null
  , "nombre"  text    not null
  , primary key ("carné")
  )
;

create table "Inscripción"
  ( "estudiante" char(7) not null
  , "sección"    integer not null
  , primary key ("estudiante")
  , foreign key ("estudiante") references "Estudiante" ("carné")
  , foreign key ("sección") references "Sección" ("número")
  )
;

with
  "Datos de estudiantes" as (
    select *
    from
      ( values
        ('0436808', 'Arquímedes D Carrasquel Blanco'      ,         1)
      , ('0538956', 'Alexánder J Simoes Centeno'          ,         1)
      , ('0741206', 'Brian-Marcial Mendoza Bollam'        ,         1)
      , ('0741329', 'Dennis Esteban Pérez Camacho'        ,         1)
      , ('0741654', 'Nílver Fernando Viera Araujo'        ,         1)
      , ('0810398', 'Alejandro Jose Garbi Moya'           ,         1)
      , ('0810479', 'David Enrique Goudet Díaz'           ,         1)
      , ('0910123', 'David Gonzalo Caicedo Mora'          ,         1)
      , ('0910236', 'Andrea Carolina Díaz Mora'           ,         1)
      , ('0910329', 'María Gabriela Giménez Rodríguez'    ,         1)
      , ('0910430', 'Alessandro Augusto La Corte Romagni' ,         1)
      , ('0910672', 'Carlo Emigdio Polisano Sánchez'      ,         1)
      , ('0910797', 'Nelson Avelino Saturno Teles'        ,         1)
      , ('0911020', 'Luiscarlo Rivera Arias'              ,         1)
      , ('1010088', 'Stéfany Daniela Botero Mendoza'      ,         1)
      , ('1010231', 'Paolángela Espinal Useche'           ,         1)
      , ('1010445', 'Cristian Adrián Medina Villarroel'   ,         1)
      , ('1010539', 'Daniel Enrique Pelayo Useche'        ,         1)
      , ('1010757', 'Reinaldo Enrique Verdugo Chávez'     ,         1)

      , ('0538087', 'Víctor M De Ponte Olivares'          ,         2)
      , ('0740913', 'Ángel Roso Franco Carvajal'          ,         2)
      , ('0741249', 'Jónathan J Moreno Correa'            ,         2)
      , ('0810223', 'Susana Charara Charara'              ,         2)
      , ('0810462', 'Betzabeth Carolina González Canónico',         2)
      , ('0910029', 'Gabriel Armando Álvarez Ramírez'     ,         2)
      , ('0910219', 'José Ignacio Delgado Campos'         ,         2)
      , ('0910270', 'Carlos Adrián Farinha Couce'         ,         2)
      , ('0910381', 'Christhian Jesús Guevara Valencia'   ,         2)
      , ('0910502', 'Francisco Antonio Martínez Medina'   ,         2)
      , ('0910832', 'Víctor Julio Suniaga Martín'         ,         2)
      , ('0910971', 'Alejandro Jesús Guevara Allen'       ,         2)
      , ('0911207', 'Gustavo Enrique Siñóvsky Sánchez'    ,         2)
      , ('1010132', 'Fabio Alexánder Castro Martinez'     ,         2)
      , ('1010353', 'Andrés Rafael Hernández Monterola'   ,         2)
      , ('1010534', 'Jesús Adolfo Parra Parra'            ,         2)
      , ('1010608', 'Vanessa Eleonora Rívas Serio'        ,         2)
      , ('1010738', 'Abelardo Jesús Valiño Ovalle'        ,         2)
      , ('1010898', 'Jesús Humberto Sánchez Pérez'        ,         2)
      , ('1011247', 'Rosángelis Verónica García Dorta'    ,         2)

      , ('0910794', 'Miguel  Saraiva'                     ,      NULL)
      , ('0910894', 'Ricardo Vethencourt'                 ,      NULL)
      , ('1010640', 'Donato Rolo'                         ,      NULL)
      ) as "Datos de estudiantes"
        ("carné"  , "nombre"                              , "sección")
  ),
  "Insertar secciones" as (
    insert into "Sección" ("número")
    select distinct "Datos de estudiantes"."sección"
    from            "Datos de estudiantes"
    where           "Datos de estudiantes"."sección" is not null
  ),
  "Insertar estudiantes" as (
    insert into "Estudiante" ("carné", "nombre")
    select "Datos de estudiantes"."carné", "Datos de estudiantes"."nombre"
    from   "Datos de estudiantes"
  )
insert into "Inscripción" ("estudiante", "sección")
select "Datos de estudiantes"."carné", "Datos de estudiantes"."sección"
from   "Datos de estudiantes"
where  "Datos de estudiantes"."sección" is not null
;



create table "Evaluación"
  ( "nombre"  text not null
  , "fecha"   date not null
  , "puntaje" real not null
  , primary key ("nombre")
  )
;

insert into "Evaluación"
  ("nombre"              , "fecha"     , "puntaje") values
  ('Exámen 1'            , '2014-10-01',        20)
, ('Exámen 2'            , '2014-10-22',        25)
, ('Exámen 3'            , '2014-11-24',        25)
, ('Etapa 1 del proyecto', '2014-09-26',         5)
, ('Etapa 2 del proyecto', '2014-10-10',         8)
, ('Etapa 3 del proyecto', '2014-10-31',         7)
, ('Etapa 4 del proyecto', '2014-11-21',        10)
;



create type "lenguaje" as enum
  ( 'Haskell'
  , 'Python'
  , 'Ruby'
  )
;

-- Aunque en principio no van a cambiar los grupos ni el lenguaje de una entrega a otra, hay que dejar abierta la posibilidad.
create table "Grupo"
  ( "número"     integer  not null check ("número" >= 0) -- Nota: La asignación de números de grupo es manual a propósito.
  , "evaluación" text     not null
  , "lenguaje"   lenguaje not null
  , primary key ("número", "evaluación")
  , foreign key ("evaluación") references "Evaluación" ("nombre")
  )
;

-- Aunque en principio no van a cambiar los integrantes de los grupos de una entrega a otra, hay que dejar abierta la posibilidad.
create table "Integrante"
  ( "estudiante" char(7) not null
  , "grupo"      integer not null
  , "evaluación" text    not null
  , primary key ("estudiante", "grupo", "evaluación")
  , foreign key ("grupo", "evaluación") references "Grupo" ("número", "evaluación")
  )
;

with
  "Datos de entregas" as (
    select *
    from
      ( values               (      1, 'Etapa 1 del proyecto', array['0436808'           ], 'Haskell' )
      ,                      (      2, 'Etapa 1 del proyecto', array['0538087', '0910502'], 'Python'  )
      ,                      (      3, 'Etapa 1 del proyecto', array['0740913', '0910894'], 'Python'  )
      ,                      (      4, 'Etapa 1 del proyecto', array['0741206'           ], 'Haskell' )
      ,                      (      5, 'Etapa 1 del proyecto', array['0741249'           ], 'Python'  )
      ,                      (      6, 'Etapa 1 del proyecto', array['0741329'           ], 'Python'  )
      ,                      (      7, 'Etapa 1 del proyecto', array['0741654', '0810462'], 'Python'  )
      ,                      (      8, 'Etapa 1 del proyecto', array['0810223', '0810479'], 'Ruby'    )
      ,                      (      9, 'Etapa 1 del proyecto', array['0810398', '0910430'], 'Haskell' )
      ,                      (     10, 'Etapa 1 del proyecto', array['0910029', '0910794'], 'Python'  )
      ,                      (     11, 'Etapa 1 del proyecto', array['0910123', '0910381'], 'Ruby'    )
      ,                      (     12, 'Etapa 1 del proyecto', array['0910219', '0910832'], 'Python'  )
      ,                      (     13, 'Etapa 1 del proyecto', array['0910236'           ], 'Python'  )
      ,                      (     14, 'Etapa 1 del proyecto', array['0910270'           ], 'Python'  )
      ,                      (     15, 'Etapa 1 del proyecto', array['0910329', '1010088'], 'Python'  )
      ,                      (     16, 'Etapa 1 del proyecto', array['0910672', '0910971'], 'Python'  )
      ,                      (     17, 'Etapa 1 del proyecto', array['0910797', '1010539'], 'Python'  )
      ,                      (     18, 'Etapa 1 del proyecto', array['0911020'           ], 'Haskell' )
      ,                      (     19, 'Etapa 1 del proyecto', array['0911207'           ], 'Python'  )
      ,                      (     20, 'Etapa 1 del proyecto', array['1010132', '1010640'], 'Python'  )
      ,                      (     21, 'Etapa 1 del proyecto', array['1010231', '1011247'], 'Python'  )
      ,                      (     22, 'Etapa 1 del proyecto', array['1010353', '1010738'], 'Python'  )
      ,                      (     23, 'Etapa 1 del proyecto', array['1010445', '1010534'], 'Python'  )
      ,                      (     24, 'Etapa 1 del proyecto', array['1010608', '1010757'], 'Ruby'    )
      ,                      (     25, 'Etapa 1 del proyecto', array['1010898'           ], 'Python'  )
      ) as "Datos de entrega"("grupo", "evaluación"          , "integrantes"              , "lenguaje")
  ),
  "Insertar grupos" as (
    insert into "Grupo" ("número", "evaluación", "lenguaje")
    select "grupo", "evaluación", "lenguaje" :: "lenguaje"
    from   "Datos de entregas"
  )
insert into "Integrante" ("estudiante", "grupo", "evaluación")
select "Datos de entregas"."integrantes"[i], "grupo", "evaluación"
from   "Datos de entregas"
,      generate_subscripts("Datos de entregas"."integrantes", 1) as i
;



create table "Asignación de evaluación"
  ( "grupo"      integer not null
  , "evaluación" text    not null
  , "evaluador"  text    not null
  , primary key ("grupo", "evaluación")
  , foreign key ("grupo", "evaluación") references "Grupo" ("número", "evaluación")
  , foreign key ("evaluador") references "Evaluador" ("nombre")
  )
;

-- Matteo evalúa el proyecto de Reinaldo y Vanessa (entrega 1):
insert into     "Asignación de evaluación" ("grupo", "evaluación", "evaluador")
select distinct "Grupo"."número", "Grupo"."evaluación", 'Matteo Ferrando'
from
  "Grupo"
    inner join "Integrante" on true
      and "Grupo"."número"     = "Integrante"."grupo"
      and "Grupo"."evaluación" = "Integrante"."evaluación"
      inner join "Estudiante" on true
        and "Integrante"."estudiante" = "Estudiante"."carné"
        and "Estudiante"."nombre" in
          ( 'Reinaldo Enrique Verdugo Chávez'
          , 'Vanessa Eleonora Rívas Serio'
          )
    inner join "Evaluación" on true
      and "Grupo"."evaluación" = "Evaluación"."nombre"
      and "Evaluación"."nombre" = 'Etapa 1 del proyecto'
;

-- Los demás proyectos en Ruby son de David (entrega 1):
insert into "Asignación de evaluación" ("grupo", "evaluación", "evaluador")
select "Grupo"."número", "Grupo"."evaluación", 'David Lilue'
from
  "Grupo"
    inner join "Evaluación" on true
      and "Grupo"."evaluación" = "Evaluación"."nombre"
      and "Evaluación"."nombre" = 'Etapa 1 del proyecto'
    left outer join "Asignación de evaluación" on true
      and "Grupo"."número"     = "Asignación de evaluación"."grupo"
      and "Grupo"."evaluación" = "Asignación de evaluación"."evaluación"
where true
  and "Grupo"."evaluación" = 'Etapa 1 del proyecto'
  and "Grupo"."lenguaje"   = 'Ruby'
  and "Asignación de evaluación"."evaluador" is null
;

-- Los de Haskell son para Manuel y Matteo (entrega 1):
insert into "Asignación de evaluación" ("grupo", "evaluación", "evaluador")
select
  "Grupo"."número",
  "Grupo"."evaluación",
  case        row_number() over (order by random()) % 2
  when 0 then 'Manuel Gómez'
  else        'Matteo Ferrando'
  end
from
  "Grupo"
    left outer join "Asignación de evaluación" on true
      and "Grupo"."número"     = "Asignación de evaluación"."grupo"
      and "Grupo"."evaluación" = "Asignación de evaluación"."evaluación"
where true
  and "Grupo"."evaluación" = 'Etapa 1 del proyecto'
  and "Grupo"."lenguaje"   = 'Haskell'
;

-- Los demás se distribuyen hasta que todos tengan más o menos la misma cantidad:
create function "Asignar evaluación"("evaluación" text)
returns void
language "plpgsql"
as $$
  declare
    "entrega" record;
  begin
    for "entrega" in
      select
        "Grupo"."número" as "grupo",
        "Grupo"."evaluación"
      from
        "Grupo"
          left outer join "Asignación de evaluación" on true
            and "Grupo"."número"     = "Asignación de evaluación"."grupo"
            and "Grupo"."evaluación" = "Asignación de evaluación"."evaluación"
      where true
        and "Grupo"."evaluación" = $1
        and "Asignación de evaluación"."evaluador" is null
      order by
        random()
    loop
      insert into "Asignación de evaluación" ("grupo", "evaluación", "evaluador")
      select "entrega"."grupo", "entrega"."evaluación", "Evaluador"."nombre"
      from
        "Evaluador"
          left outer join "Asignación de evaluación" on true
            and "Evaluador"."nombre" = "Asignación de evaluación"."evaluador"
            and "Asignación de evaluación"."evaluación" = $1
      group by
        "Evaluador"."nombre"
      order by
        count("Asignación de evaluación"."evaluador") asc
      limit
        1
      ;
    end loop;
  end;
$$;

select "Asignar evaluación"('Etapa 1 del proyecto');

-- Mostrar asignación:
select
  string_agg("Integrante"."estudiante", ', ') as "Grupo",
  "Asignación de evaluación"."evaluador" as "Evaluador"
from
  "Grupo"
    inner join "Asignación de evaluación" on true
      and "Grupo"."número" = "Asignación de evaluación"."grupo"
      and "Grupo"."evaluación" = "Asignación de evaluación"."evaluación"
      inner join "Integrante" on true
        and "Grupo"."número" = "Integrante"."grupo"
        and "Grupo"."evaluación" = "Integrante"."evaluación"
group by
  "Grupo"."número",
  "Grupo"."evaluación",
  "Asignación de evaluación"."evaluador"
order by
  "Evaluador"
;
