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
      ( values               (      1, array['0436808'           ], 'Haskell' )
      ,                      (      2, array['0538087', '0910502'], 'Python'  )
      ,                      (      3, array['0740913', '0910894'], 'Python'  )
      ,                      (      4, array['0741206'           ], 'Haskell' )
      ,                      (      5, array['0741249'           ], 'Python'  )
      ,                      (      6, array['0741329'           ], 'Python'  )
      ,                      (      7, array['0741654', '0810462'], 'Python'  )
      ,                      (      8, array['0810223', '0810479'], 'Ruby'    )
      ,                      (      9, array['0810398', '0910430'], 'Haskell' )
      ,                      (     10, array['0910029', '0910794'], 'Python'  )
      ,                      (     11, array['0910123', '0910381'], 'Ruby'    )
      ,                      (     12, array['0910219', '0910832'], 'Python'  )
      ,                      (     13, array['0910236'           ], 'Python'  )
      ,                      (     14, array['0910270'           ], 'Python'  )
      ,                      (     15, array['0910329', '1010088'], 'Python'  )
      ,                      (     16, array['0910672', '0910971'], 'Python'  )
      ,                      (     17, array['0910797', '1010539'], 'Python'  )
      ,                      (     18, array['0911020'           ], 'Haskell' )
      ,                      (     19, array['0911207'           ], 'Python'  )
      ,                      (     20, array['1010132', '1010640'], 'Python'  )
      ,                      (     21, array['1010231', '1011247'], 'Python'  )
      ,                      (     22, array['1010353', '1010738'], 'Python'  )
      ,                      (     23, array['1010445', '1010534'], 'Python'  )
      ,                      (     24, array['1010608', '1010757'], 'Ruby'    )
      ,                      (     25, array['1010898'           ], 'Python'  )
      ) as "Datos de entrega"("grupo", "integrantes"              , "lenguaje")
  ),
  "Insertar grupos" as (
    insert into "Grupo" ("número", "evaluación", "lenguaje")
    select "grupo", 'Etapa 1 del proyecto', "lenguaje" :: "lenguaje"
    from   "Datos de entregas"
  )
insert into "Integrante" ("estudiante", "grupo", "evaluación")
select "Datos de entregas"."integrantes"[i], "grupo", 'Etapa 1 del proyecto'
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

create function "Asignar evaluación"("evaluación" text)
returns void
language "plpgsql"
as $$
  declare
    "entrega" record;
  begin
    -- Matteo evalúa el proyecto de Reinaldo y Vanessa:
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
          and "Evaluación"."nombre" = $1
    ;

    -- Los demás proyectos en Ruby son de David:
    insert into "Asignación de evaluación" ("grupo", "evaluación", "evaluador")
    select "Grupo"."número", "Grupo"."evaluación", 'David Lilue'
    from
      "Grupo"
        inner join "Evaluación" on true
          and "Grupo"."evaluación" = "Evaluación"."nombre"
          and "Evaluación"."nombre" = $1
        left outer join "Asignación de evaluación" on true
          and "Grupo"."número"     = "Asignación de evaluación"."grupo"
          and "Grupo"."evaluación" = "Asignación de evaluación"."evaluación"
    where true
      and "Grupo"."evaluación" = $1
      and "Grupo"."lenguaje"   = 'Ruby'
      and "Asignación de evaluación"."evaluador" is null
    ;

    -- Los de Haskell son para Manuel y Matteo, excepto en las últimas dos etapas donde son solo para Manuel:
    insert into "Asignación de evaluación" ("grupo", "evaluación", "evaluador")
    select
      "Grupo"."número",
      "Grupo"."evaluación",
      case        row_number() over (order by random()) % 2
      when 0 then 'Manuel Gómez'
      else (
        case $1
        when 'Etapa 1 del proyecto' then 'Matteo Ferrando'
        when 'Etapa 2 del proyecto' then 'Matteo Ferrando'
        else 'Manuel Gómez'
        end
      )
      end
    from
      "Grupo"
        left outer join "Asignación de evaluación" on true
          and "Grupo"."número"     = "Asignación de evaluación"."grupo"
          and "Grupo"."evaluación" = "Asignación de evaluación"."evaluación"
    where true
      and "Grupo"."evaluación" = $1
      and "Grupo"."lenguaje"   = 'Haskell'
    ;

    -- Los demás se distribuyen hasta que todos tengan más o menos la misma cantidad, excepto en las últimas dos etapas donde Matteo no participa:
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
      having false
        or $1 = 'Etapa 1 del proyecto'
        or $1 = 'Etapa 2 del proyecto'
        or "Evaluador"."nombre" <> 'Matteo Ferrando'
      order by
        count("Asignación de evaluación"."evaluador") asc
      limit
        1
      ;
    end loop;
  end;
$$;



---



-- Etapa 1 del proyecto:

-- Semilla seleccionada aleatoriamente:
select setseed(0.42);

select "Asignar evaluación"('Etapa 1 del proyecto');



create table "Calificación"
  ( "grupo"        integer      not null
  , "evaluación"   text         not null
  , "calificación" numeric(4,2) not null check (("calificación" * 4) % 1 = 0)
  , primary key ("grupo", "evaluación")
  , foreign key ("grupo", "evaluación") references "Grupo" ("número", "evaluación")
  )
;

insert into "Calificación" ("grupo", "evaluación", "calificación")
select "grupo", 'Etapa 1 del proyecto', "calificación"
from
  -- David Lilue
  ( values     (      3,           1.25)
  ,            (      8,           3.25)
  ,            (     11,           4.75)
  ,            (     14,           4.75)
  ,            (     15,           5.00)
  ,            (     17,           5.00)

  -- Karen Troiano
  ,            (      2,           4.25)
  ,            (      5,           5.00)
  ,            (     12,           4.50)
  ,            (     19,           4.75)
  ,            (     21,           5.00)
  ,            (     23,           4.25)

  -- Manuel Gómez
  ,            (      6,           1.50)
  ,            (      9,           4.75)
  ,            (     10,           2.00)
  ,            (     18,           4.75)
  ,            (     20,           2.50)
  ,            (     25,           3.25)

  -- Matteo Ferrando
  ,            (      1,           4.50)
  ,            (      4,           5.00)
  ,            (      7,           3.00)
  ,            (     16,           4.50)
  ,            (     13,           4.00)
  ,            (     22,           4.25)
  ,            (     24,           2.50)

  ) as "Datos" ("grupo", "calificación")
;



---



-- Etapa 2 del proyecto:
with
  "Datos de entregas" as (
    select *
    from
      ( values               (      1, array['0436808'           ], 'Haskell' )
      ,                      (      2, array['0538087', '0910502'], 'Python'  )
      ,                      (      4, array['0741206'           ], 'Haskell' )
      ,                      (      7, array['0741654', '0810462'], 'Python'  )
      ,                      (      8, array['0810223', '0810479'], 'Ruby'    )
      ,                      (      9, array['0810398', '0910430'], 'Haskell' )
      ,                      (     10, array['0910029', '0910794'], 'Python'  )
      ,                      (     12, array['0910219', '0910832'], 'Python'  )
      ,                      (     14, array['0910270'           ], 'Python'  )
      ,                      (     15, array['0910329', '1010088'], 'Python'  )
      ,                      (     16, array['0910672', '0910971'], 'Python'  )
      ,                      (     17, array['0910797', '1010539'], 'Python'  )
      ,                      (     18, array['0911020'           ], 'Haskell' )
      ,                      (     19, array['0911207'           ], 'Python'  )
      ,                      (     20, array['1010132', '1010640'], 'Python'  )
      ,                      (     21, array['1010231', '1011247'], 'Python'  )
      ,                      (     22, array['1010353', '1010738'], 'Python'  )
      ,                      (     23, array['1010445', '1010534'], 'Python'  )
      ,                      (     24, array['1010608', '1010757'], 'Ruby'    )
      ,                      (     25, array['1010898'           ], 'Python'  )
      ) as "Datos de entrega"("grupo", "integrantes"              , "lenguaje")
  ),
  "Insertar grupos" as (
    insert into "Grupo" ("número", "evaluación", "lenguaje")
    select "grupo", 'Etapa 2 del proyecto', "lenguaje" :: "lenguaje"
    from   "Datos de entregas"
  )
insert into "Integrante" ("estudiante", "grupo", "evaluación")
select "Datos de entregas"."integrantes"[i], "grupo", 'Etapa 2 del proyecto'
from   "Datos de entregas"
,      generate_subscripts("Datos de entregas"."integrantes", 1) as i
;

select "Asignar evaluación"('Etapa 2 del proyecto');



insert into "Calificación" ("grupo", "evaluación", "calificación")
select "grupo", 'Etapa 2 del proyecto', "calificación"
from
  -- David Lilue
  ( values     (      7,           2.25)
  ,            (      8,           1.50)
  ,            (     16,           2.50)
  ,            (     17,           1.25)
  ,            (     23,           1.75)

  -- Karen Troiano
  ,            (      2,           5.00)
  ,            (     12,           2.00)
  ,            (     15,           4.50)
  ,            (     21,           4.25)
  ,            (     22,           3.50)

  -- Manuel Gómez
  ,            (      4,           5.00)
  ,            (      9,           4.75)
  ,            (     20,           2.75)
  ,            (     14,           0.00)
  ,            (     25,           2.50)

  -- Matteo Ferrando
  ,            (      1,           4.75)
  ,            (     10,           4.50)
  ,            (     18,           4.00)
  ,            (     19,           1.75)
  ,            (     24,           1.50)

  ) as "Datos" ("grupo", "calificación")
;



---



-- Etapa 3 del proyecto:
with
  "Datos de entregas" as (
    select *
    from
      ( values               (      2, array['0538087', '0910502'], 'Python'  )
      ,                      (      4, array['0741206'           ], 'Haskell' )
      ,                      (      8, array['0810223', '0810479'], 'Ruby'    )
      ,                      (      9, array['0810398', '0910430'], 'Haskell' )
      ,                      (     10, array['0910029', '0910794'], 'Python'  )
      ,                      (     11, array['0910123', '0910381'], 'Ruby'    )
      ,                      (     12, array['0910219', '0910832'], 'Python'  )
      ,                      (     15, array['0910329', '1010088'], 'Python'  )
      ,                      (     19, array['0911207'           ], 'Python'  )
      ,                      (     20, array['1010132', '1010640'], 'Python'  )
      ,                      (     21, array['1010231', '1011247'], 'Python'  )
      ,                      (     22, array['1010353', '1010738'], 'Python'  )
      ,                      (     23, array['1010445', '1010534'], 'Python'  )
      ,                      (     24, array['1010608', '1010757'], 'Ruby'    )
      ) as "Datos de entrega"("grupo", "integrantes"              , "lenguaje")
  ),
  "Insertar grupos" as (
    insert into "Grupo" ("número", "evaluación", "lenguaje")
    select "grupo", 'Etapa 3 del proyecto', "lenguaje" :: "lenguaje"
    from   "Datos de entregas"
  )
insert into "Integrante" ("estudiante", "grupo", "evaluación")
select "Datos de entregas"."integrantes"[i], "grupo", 'Etapa 3 del proyecto'
from   "Datos de entregas"
,      generate_subscripts("Datos de entregas"."integrantes", 1) as i
;

-- Manuel evaluó la tercera entrega del grupo 20:
insert into "Asignación de evaluación"
  ("grupo", "evaluación"          , "evaluador"   ) values
  (     20, 'Etapa 3 del proyecto', 'Manuel Gómez')
;

select "Asignar evaluación"('Etapa 3 del proyecto');



insert into "Calificación" ("grupo", "evaluación", "calificación")
select "grupo", 'Etapa 3 del proyecto', "calificación"
from
  -- David Lilue
  ( values     (      8,           9.00)
  ,            (     11,           6.75)
  ,            (     12,           6.00)
  ,            (     23,           6.75)

  -- Karen Troiano
  ,            (      2,           3.50)
  ,            (     10,           9.00)
  ,            (     15,           8.50)
  ,            (     19,           5.50)
  ,            (     21,           6.50)


  -- Manuel Gómez
  ,            (      4,           9.00)
  ,            (      9,           6.50)
  ,            (     20,           6.50)
  ,            (     22,           5.50)

  -- Matteo Ferrando
  ,            (     24,           5.00)

  ) as "Datos" ("grupo", "calificación")
;



---



-- Etapa 4 del proyecto:
with
  "Datos de entregas" as (
    select *
    from
      ( values               (      1, array['0436808'           ], 'Haskell' )
      ,                      (      2, array['0538087', '0910502'], 'Python'  )
      ,                      (      4, array['0741206'           ], 'Haskell' )
      ,                      (      8, array['0810223', '0810479'], 'Ruby'    )
      ,                      (      9, array['0810398', '0910430'], 'Haskell' )
      ,                      (     10, array['0910029', '0910794'], 'Python'  )
      ,                      (     11, array['0910123', '0910381'], 'Ruby'    )
      ,                      (     12, array['0910219', '0910832'], 'Python'  )
      ,                      (     15, array['0910329', '1010088'], 'Python'  )
      ,                      (     19, array['0911207'           ], 'Python'  )
      ,                      (     20, array['1010132', '1010640'], 'Python'  )
      ,                      (     21, array['1010231', '1011247'], 'Python'  )
      ,                      (     22, array['1010353', '1010738'], 'Python'  )
      ,                      (     23, array['1010445', '1010534'], 'Python'  )
      ) as "Datos de entrega"("grupo", "integrantes"              , "lenguaje")
  ),
  "Insertar grupos" as (
    insert into "Grupo" ("número", "evaluación", "lenguaje")
    select "grupo", 'Etapa 4 del proyecto', "lenguaje" :: "lenguaje"
    from   "Datos de entregas"
  )
insert into "Integrante" ("estudiante", "grupo", "evaluación")
select "Datos de entregas"."integrantes"[i], "grupo", 'Etapa 4 del proyecto'
from   "Datos de entregas"
,      generate_subscripts("Datos de entregas"."integrantes", 1) as i
;

select "Asignar evaluación"('Etapa 4 del proyecto');



insert into "Calificación" ("grupo", "evaluación", "calificación")
select "grupo", 'Etapa 4 del proyecto', "calificación"
from
  -- David Lilue
  ( values     (      8,           6.50)
  ,            (     11,          11.00)
  ,            (     12,           6.50)
  ,            (     15,           8.00)

  -- Karen Troiano
  ,            (      2,           2.75)
  ,            (     19,           5.50)
  ,            (     21,           9.50)
  ,            (     22,           4.25)
  ,            (     23,           9.00)

  -- Manuel Gómez
  ,            (      1,           0.00)
  ,            (      4,          11.00)
  ,            (      9,           7.25)
  ,            (     10,           9.25)
  ,            (     20,           8.00)

  ) as "Datos" ("grupo", "calificación")
;



---



-- Las entregas 3 y 4 del grupo 16 no se habían registrado.  Oops.

with
  "Datos de entregas" as (
    select *
    from
      ( values               (     16, array['0910672', '0910971'], 'Python'  )
      ) as "Datos de entrega"("grupo", "integrantes"              , "lenguaje")
  ),
  "Insertar grupos" as (
    insert into "Grupo" ("número", "evaluación", "lenguaje")
    select "grupo", 'Etapa 3 del proyecto', "lenguaje" :: "lenguaje"
    from   "Datos de entregas"
  )
insert into "Integrante" ("estudiante", "grupo", "evaluación")
select "Datos de entregas"."integrantes"[i], "grupo", 'Etapa 3 del proyecto'
from   "Datos de entregas"
,      generate_subscripts("Datos de entregas"."integrantes", 1) as i
;

with
  "Datos de entregas" as (
    select *
    from
      ( values               (     16, array['0910672', '0910971'], 'Python'  )
      ) as "Datos de entrega"("grupo", "integrantes"              , "lenguaje")
  ),
  "Insertar grupos" as (
    insert into "Grupo" ("número", "evaluación", "lenguaje")
    select "grupo", 'Etapa 4 del proyecto', "lenguaje" :: "lenguaje"
    from   "Datos de entregas"
  )
insert into "Integrante" ("estudiante", "grupo", "evaluación")
select "Datos de entregas"."integrantes"[i], "grupo", 'Etapa 4 del proyecto'
from   "Datos de entregas"
,      generate_subscripts("Datos de entregas"."integrantes", 1) as i
;

-- Manuel las evaluó.
insert into "Asignación de evaluación"
  ("grupo", "evaluación"          , "evaluador"   ) values
  (     16, 'Etapa 3 del proyecto', 'Manuel Gómez')
, (     16, 'Etapa 4 del proyecto', 'Manuel Gómez')
;

insert into "Calificación"
  ("grupo", "evaluación"          , "calificación") values
  (     16, 'Etapa 3 del proyecto',           8.50)
, (     16, 'Etapa 4 del proyecto',          10.50)
;



---



create view
  "Calificaciones individuales"
as
  select
    "Estudiante"."carné"                   as "Carné",
    "Estudiante"."nombre"                  as "Nombre",
    "Grupo"."número"                       as "Número de grupo",
    "Asignación de evaluación"."evaluador" as "Evaluador",
    "Grupo"."lenguaje"                     as "Lenguaje",
    "Evaluación"."nombre"                  as "Evaluación",
    "Calificación"."calificación"          as "Calificación"
  from
    "Grupo"
      inner join "Evaluación" on true
        and "Grupo"."evaluación" = "Evaluación"."nombre"
      inner join "Asignación de evaluación" on true
        and "Grupo"."número" = "Asignación de evaluación"."grupo"
        and "Grupo"."evaluación" = "Asignación de evaluación"."evaluación"
      inner join "Integrante" on true
        and "Grupo"."número" = "Integrante"."grupo"
        and "Grupo"."evaluación" = "Integrante"."evaluación"
          inner join "Estudiante" on true
            and "Integrante"."estudiante" = "Estudiante"."carné"
          left outer join "Calificación" on true
            and "Grupo"."número" = "Calificación"."grupo"
            and "Grupo"."evaluación" = "Calificación"."evaluación"
  order by
    "Carné"      asc,
    "Evaluación" asc
;

create view
  "Calificaciones grupales"
as
  select
    "Número de grupo",
    string_agg("Carné" , ', ' order by "Carné" asc) as "Grupo",
    string_agg("Nombre", ', ' order by "Carné" asc) as "Nombres",
    "Evaluador",
    "Lenguaje",
    "Evaluación",
    "Calificación"
  from
    "Calificaciones individuales"
  group by
    "Número de grupo",
    "Evaluador",
    "Lenguaje",
    "Evaluación",
    "Calificación"
  order by
    "Número de grupo" asc,
    "Evaluación"      asc
;

create view
  "Totales individuales"
as
  select
    "Carné",
    "Nombre",
    sum("Calificación") as "Total",
    count(*) || '/4 (' || string_agg(
      case "Evaluación"
        when 'Etapa 1 del proyecto' then '1'
        when 'Etapa 2 del proyecto' then '2'
        when 'Etapa 3 del proyecto' then '3'
        when 'Etapa 4 del proyecto' then '4'
      end,
      ', '
    ) || ')' as "Entregas realizadas"
  from
    "Calificaciones individuales"
  group by
    "Carné",
    "Nombre"
  order by
    "Carné" asc
;

create view
  "Totales grupales"
as
  select
    "Número de grupo",
    "Grupo",
    sum("Calificación") as "Total",
    count(*) || '/4 (' || string_agg(
      case "Evaluación"
        when 'Etapa 1 del proyecto' then '1'
        when 'Etapa 2 del proyecto' then '2'
        when 'Etapa 3 del proyecto' then '3'
        when 'Etapa 4 del proyecto' then '4'
      end,
      ', '
    ) || ')' as "Entregas realizadas"
  from
    "Calificaciones grupales"
  group by
    "Número de grupo",
    "Grupo"
  order by
    "Número de grupo" asc
;
