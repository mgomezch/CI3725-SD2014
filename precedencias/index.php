<?php require_once '../../../../content-type.php'; ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <meta http-equiv="Content-type" content="application/xhtml+xml; charset=UTF-8"/>
    <title>Manuel Gómez — CI3725 — Septiembre–Diciembre de 2014 — Precedencia y asociatividad en Trinity</title>
    <style type="text/css">
      p { text-align:justify; }
      table, th, td { border: 1px solid black; border-collapse: collapse; padding: 0.4em; }
      th { text-align: left; background: black; color: white; border: white 1px solid; }
    </style>
  </head>
  <body>
    <h1>
      <a href="../../../..">Manuel Gómez</a>
      — <a href="../../..">Cursos</a>
      — <a href="../..">CI3725</a>
      — <a href="..">Septiembre–Diciembre de 2014</a>
      — Precedencia y asociatividad en Trinity
    </h1>

    <hr/>

    <p>Los operadores en una misma fila de una tabla tienen todos el mismo nivel de precedencia y la misma asociatividad.</p>
    <p>Los operadores unarios no tienen asociatividad izquierda ni derecha, sino que simplemente son asociativos, pero en herramientas generadoras de analizadores sintácticos LR, deben especificarse como de asociatividad izquierda si son operadores unarios sufijos, y de asociatividad derecha si son operadores unarios prefijos.</p>
    <p>La precedencia se enumera de máxima a mínima en esta tabla.</p>

    <table>
      <tbody>
        <tr><th>Asociatividad</th> <th>Operadores                                           </th></tr>
        <tr><td>Asociativo   </td> <td><code>[] '                                    </code></td></tr>
        <tr><td>Asociativo   </td> <td><code><abbr title="unario">-</abbr>           </code></td></tr>
        <tr><td>Izquierda    </td> <td><code>* / % div mod .*. ./. .%. .div. .mod.   </code></td></tr>
        <tr><td>Izquierda    </td> <td><code>+ <abbr title="binario">-</abbr> .+. .-.</code></td></tr>
        <tr><td>Asociativo   </td> <td><code>not                                     </code></td></tr>
        <tr><td>No asociativo</td> <td><code><![CDATA[== /= <= < >= >             ]]></code></td></tr>
        <tr><td>Izquierda    </td> <td><code><![CDATA[&                           ]]></code></td></tr>
        <tr><td>Izquierda    </td> <td><code>|                                       </code></td></tr>
      </tbody>
    </table>

    <hr/>

    <p>
      <a href="http://validator.w3.org/check?uri=referer">
        <img src="http://www.w3.org/Icons/valid-xhtml11" alt="Valid XHTML 1.1" height="31" width="88"/>
      </a>
    </p>
  </body>
</html>
