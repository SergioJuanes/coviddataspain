# Covid DataSet Spain
Conjunto de datos sobre la pandemia por el COVID-19 en España, segmentada por comunidades autónomas. Los datos son actualizables automáticamente a través de la herramienta GitHub Actions. Para ello, se conecta al PDF que el Gobierno español publica diariamente (excepto fines de semana y festivos), y realiza un proceso de limpieza y tratamiento de la información (scripts update.R para los casos, fallecidos, incidencia acumulada y hospitalización y update_vac.R para el ritmo de vacunación). Además, se descargan los datos correspondientes a la OMS para todos los países del mundo dos veces al día (update_who.R).

Los ficheros se encuentran, junto a otros ficheros que pueden ser útiles para la realización de calculos o de gráficas, en la carpeta data. 

Los datos de España pueden ser inconsistentes en ciertos puntos (el número de casos o muertes diario resulta negativo) debido a reajustes que los gobiernos autonómicos han realizado a lo largo de la pandemia.
