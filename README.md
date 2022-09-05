# Repositoiro para el curso Big Data and Machine Learning for Applied Economics Problem Set 1

Este repositorio corresponde al Problem Set 1 del curso Big Data and Machine Learning for Applied Economics 2022.

Datos y archivos de réplica para "Repositoiro para el curso Big Data and Machine Learning for Applied Economics Problem Set 1" por 
[Camilo Bonilla](https://github.com/cabonillah),  [Nicolás Velásquez](https://github.com/Nicolas-Velasquez-Oficial) y  [Rafael Santofimio](https://github.com/rasantofimior/)
# Resumen

Para el sector fiscal, la correcta implemetación de políticas públicas y reformas tributarias han sido temáticas importantes a la hora de poder formar naciones con sociedades crecientes en ingreso pero con redistribuciones que pérmitan una equidad para los sectores marginales con igualdad de oportunidades. En un mundo ideal el gobierno tendría control total al observar los ingresos de todas las personas, sin embargo actualmente las personas suelen subreportar estas cifras con temor a ser segmentada bajo una tasa proporsional a sus ingresos. Con base en lo anterior, se vuelve de suma importancia obtener herramientas que nos permitan tener estimaciones de los ingresos de las personas y llegar a cuestionarnos si debido a un subreporte de los mismos se estan teniendo impuestos suboptimos. Más importante aún si dentro de los multiples modelos posibles, el gobierno y entidades reguladoras se debería enfocar en ciertas observaciones atipicas para llegar a una estimación cercana.

Este repositorio contiene las siguientes carpetas:

## Carpeta Document

-Problem_Set_1.pdf:
Este docuemnto contiene el trabajo escrito del Problem Set 1 con los resultados totalmente replicables.

## Carpeta Stores

-   data.csv:
    Para la extracción de la base de datos, se relizó un raspado de la pagina web \href{https://ignaciomsarmiento.github.io/GEIH2018_sample/}{GEIH2018Sample} . De allí se obtuvo 10 tablas, estas se agruparon por nombre de columna, dando como resultado una base con 178 variables y 32.177 observaciones correspondientes a la GEIH del Dane del año 2008.

## Carpeta File:

-   El análisis de datos se realiza utilizando el software R version 4.0.2 (2022-09-05)

-   main.sh: Contains the sequence of execution of dofiles and Rscripts to produce the figures and tables in the paper and appendix. Dofiles and Los Escripts en R se nombran segun la función que tienen:
    -   La carpeta scripts contine los codigos nombrados a continución:

        -   data_extraction.R
        -   data_cleaning.R
        -   descriptives_punto2.R
        -   Punto_3.R
        -   performance_tests.R

## Carpeta Views:

Las Figuras y tablas estan alojadas en la carpeta "views" nombradas de la siguiente manera: - Correlogram.png - Correlogram_Deps.png - Estrato.png - Histogram_Ing.png - Max_educlev.png
-Modelo_Age_Residuals.png
-Salarios_Predichos.png
-data.tex
-data.tex
-Modelo_age_earnings_controls_2.tex
-influencias1.png
-influencias2.png

Notas:

-   Los Scripts descriptives_punto2.R, Punto_3.R y performance_tests.R, llaman al modulo dentro del script data_cleaning.R el cual estandariza la data utilizada.
-   Si se ejecutan los scripts desde programas como R Studio, se debe asegurar antes que el directorio base se configure en "Problem-Set1\scripts".
