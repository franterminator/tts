# TTS
## Descripción del programa
Se trata de un programa escrito en FORTRAN 90 por franterminator. 
Permite calcular la variación de la temperatura de una placa la cual se calienta por el lateral derecho y superior, el lado izquierdo no transmite calor y el lado inferior esta en contacto con otro medio que absorvera el calor cedido por la placa.

## Requisitos del programa
El programa se puede descargar compilado. El archivo ha sido compilado para un ordenador con windows 10 de 64 bits. En cualquier caso es muy probable que este archivo no funcione en todos los ordenadores.

![Esquema de la placa y sus condiciones](https://github.com/franterminator/tts/blob/master/esquemas/placa_metalica.png)

En el repositorio se encuentra el codigo fuente del programa. Para compilarlo se necesita un compilador, recomiendo el GNU GCC para FORTRAN llamado [GFORTRAN](https://gcc.gnu.org/wiki/GFortran). Una vez compilado se puede ejecutar y trabajar con el programa, no necesita mas requisitos.

Junto al programa principal vienen incluidos un par de programas en python que sirven para representar graficamente los resultados del programa. Estan escritos en Python 3.x, por lo que es necesario tener instalado Python 3 para poder trabajar con estos programas. Otro requisito para poder usar estos programas es tener instalado la libería de [Matplotlib](https://matplotlib.org/users/installing.html#installing-an-official-release), la cual es facil de instalar con PIP.

Como dije antes, estos programas son una buena opción para obtener resultados visuales, pero no son la única. El programa en FORTRAN devuelve los resultados de las iteraciones en un archivo llamado [resultados.txt](https://github.com/franterminator/tts#resultados), el cual se puede abrir con hojas de calculo como Excel y representar los valores en gráficas.

## Inputs
Los inputs son los datos que necesita el programa para hayar los resultados, es decir, la temperatura de la placa en un instante determinado. En la siguiente tabla se recogen toda esta información que se debe suministrar al programa:

| Dimensiones de la placa | Descripcion                                                | Unidades (SI) |
|:-----------------------:|------------------------------------------------------------|----------|
| Nombre                  | Descripción                                                | Unidades |
| n                       | Número de puntos en el largo de la placa                   |          |
| m                       | Número de puntos en el ancho de la placa                   |          |
| long                    | Longitud de la placa                                       | m        |
| ancho                   | Ancho de la placa                                          | m        |
|   Propiedades térmicas  |                                                            |          |
| Nombre                  | Descripción                                                | Unidades |
| Te                      | Temperatura exterior o temperatura del medio en contacto   | K        |
| Tf                      | Temperatura de la fuente de calor                          | K        |
| h                       | Coeficiente de transmisión superficial                     | W/m2 K   |
| k                       | Conductividad térmica                                      | W/m K    |
| Dx                      | Coeficiente de difusión en dirección x (largo de la placa) | m/s2     |
| Dy                      | Coeficiente de difusión en dirección y (ancho de la placa) |          |
|    Tiempo de cálculo    |                                                            |          |
| Nombre                  | Descripción                                                | Unidades |
| deltaT                  | incremento de tiempo en cada iteración                     | s        |
| Tfinal                  | en que tiempo se para el cálculo                           | s        |


## Resultados
La temperatura en cada punto e iteración de la placa son escritos en un archivo de texto plano llamado resultados.txt.

El formato de este archivo es como sigue:
![Formato del archivo resultados.txt](https://github.com/franterminator/tts/blob/master/esquemas/formato_resultados_edited.png)

Es importante fijarse en los datos de la placa, 

