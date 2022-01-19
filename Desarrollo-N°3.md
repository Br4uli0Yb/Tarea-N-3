TAREA N°3
================
Rodríguez Conde Marko, Rodríguez Chapoñan Emanuel, Yucra Baquerizo
Braulio
9/1/2022

##PARTE 1: DPLYR - FILTER #Cargar la data de nycfights13

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.1.2

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.1.2

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v tibble  3.1.6     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.1.1     v forcats 0.5.1
    ## v purrr   0.3.4

    ## Warning: package 'tidyr' was built under R version 4.1.2

    ## Warning: package 'purrr' was built under R version 4.1.2

    ## Warning: package 'dplyr' was built under R version 4.1.2

    ## Warning: package 'forcats' was built under R version 4.1.2

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(nycflights13)
```

    ## Warning: package 'nycflights13' was built under R version 4.1.2

``` r
nycflights13::flights
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
View(flights)
```

#1.Encuentra todos los vuelos que: #A.Encuentra los vueles que tuvieron
un retraso de llegada de dos o más horas:

``` r
vuelo01 <- filter(flights, arr_delay >= 120)
vuelo01
```

    ## # A tibble: 10,200 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      811            630       101     1047            830
    ##  2  2013     1     1      848           1835       853     1001           1950
    ##  3  2013     1     1      957            733       144     1056            853
    ##  4  2013     1     1     1114            900       134     1447           1222
    ##  5  2013     1     1     1505           1310       115     1638           1431
    ##  6  2013     1     1     1525           1340       105     1831           1626
    ##  7  2013     1     1     1549           1445        64     1912           1656
    ##  8  2013     1     1     1558           1359       119     1718           1515
    ##  9  2013     1     1     1732           1630        62     2028           1825
    ## 10  2013     1     1     1803           1620       103     2008           1750
    ## # ... with 10,190 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

#B.Volaron a Houston(IAH o HOU)

``` r
vuelo02 <- filter(flights, dest == "IAH" | dest == "HOU")
vuelo02
```

    ## # A tibble: 9,313 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      623            627        -4      933            932
    ##  4  2013     1     1      728            732        -4     1041           1038
    ##  5  2013     1     1      739            739         0     1104           1038
    ##  6  2013     1     1      908            908         0     1228           1219
    ##  7  2013     1     1     1028           1026         2     1350           1339
    ##  8  2013     1     1     1044           1045        -1     1352           1351
    ##  9  2013     1     1     1114            900       134     1447           1222
    ## 10  2013     1     1     1205           1200         5     1503           1505
    ## # ... with 9,303 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

#C.Fueron operados por United, American o Delta

``` r
vuelo03 <- filter(flights, carrier == "UA"| carrier == "AA"|carrier =="DL" )
vuelo03
```

    ## # A tibble: 139,504 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      554            600        -6      812            837
    ##  5  2013     1     1      554            558        -4      740            728
    ##  6  2013     1     1      558            600        -2      753            745
    ##  7  2013     1     1      558            600        -2      924            917
    ##  8  2013     1     1      558            600        -2      923            937
    ##  9  2013     1     1      559            600        -1      941            910
    ## 10  2013     1     1      559            600        -1      854            902
    ## # ... with 139,494 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

#D.Encuentra todos los vuelos que partieron en invierno del hemisferio
sur (julio, agosto, septiembre)

``` r
vuelo04 <- filter(flights, month == 7|month == 8|month == 9)
vuelo04
```

    ## # A tibble: 86,326 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     7     1        1           2029       212      236           2359
    ##  2  2013     7     1        2           2359         3      344            344
    ##  3  2013     7     1       29           2245       104      151              1
    ##  4  2013     7     1       43           2130       193      322             14
    ##  5  2013     7     1       44           2150       174      300            100
    ##  6  2013     7     1       46           2051       235      304           2358
    ##  7  2013     7     1       48           2001       287      308           2305
    ##  8  2013     7     1       58           2155       183      335             43
    ##  9  2013     7     1      100           2146       194      327             30
    ## 10  2013     7     1      100           2245       135      337            135
    ## # ... with 86,316 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

#E.Encuentra todos los vuelos que llegaron más de dos horas tarde, pero
no salieron tarde.

``` r
vuelo05 <- filter(flights, arr_delay > 120, dep_delay <= 0 )
vuelo05
```

    ## # A tibble: 29 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1    27     1419           1420        -1     1754           1550
    ##  2  2013    10     7     1350           1350         0     1736           1526
    ##  3  2013    10     7     1357           1359        -2     1858           1654
    ##  4  2013    10    16      657            700        -3     1258           1056
    ##  5  2013    11     1      658            700        -2     1329           1015
    ##  6  2013     3    18     1844           1847        -3       39           2219
    ##  7  2013     4    17     1635           1640        -5     2049           1845
    ##  8  2013     4    18      558            600        -2     1149            850
    ##  9  2013     4    18      655            700        -5     1213            950
    ## 10  2013     5    22     1827           1830        -3     2217           2010
    ## # ... with 19 more rows, and 11 more variables: arr_delay <dbl>, carrier <chr>,
    ## #   flight <int>, tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>,
    ## #   distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

#F.Se retrasaron por lo menos una hora, pero repusieron más de 30
minutos en vuelo.

``` r
vuelo06 <- filter(flights, dep_delay >= 60, arr_delay < 30)
vuelo06
```

    ## # A tibble: 206 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     3     1850           1745        65     2148           2120
    ##  2  2013     1     3     1950           1845        65     2228           2227
    ##  3  2013     1     3     2015           1915        60     2135           2111
    ##  4  2013     1     6     1019            900        79     1558           1530
    ##  5  2013     1     7     1543           1430        73     1758           1735
    ##  6  2013     1    11     1020            920        60     1311           1245
    ##  7  2013     1    12     1706           1600        66     1949           1927
    ##  8  2013     1    12     1953           1845        68     2154           2137
    ##  9  2013     1    19     1456           1355        61     1636           1615
    ## 10  2013     1    21     1531           1430        61     1843           1815
    ## # ... with 196 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

#G.Partieron entre la medianoche y las 6 a.m.

``` r
vuelo07 <- filter(flights, dep_time >= 0 & dep_time <= 600)
vuelo07
```

    ## # A tibble: 9,344 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 9,334 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

#2.Otra función de DPLYR que es útil para usar filtros es between().
#2.1.¿Qué hace? Nos Devuelve un valor lógico que indica si el valor
especificado está dentro de un rango. #2.2.¿Puedes usarla para
simplificar el código necesario para responder a los desafíos
anteriores?

``` r
filter02 <- filter(flights, between(month, 7,9))
filter02
```

    ## # A tibble: 86,326 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     7     1        1           2029       212      236           2359
    ##  2  2013     7     1        2           2359         3      344            344
    ##  3  2013     7     1       29           2245       104      151              1
    ##  4  2013     7     1       43           2130       193      322             14
    ##  5  2013     7     1       44           2150       174      300            100
    ##  6  2013     7     1       46           2051       235      304           2358
    ##  7  2013     7     1       48           2001       287      308           2305
    ##  8  2013     7     1       58           2155       183      335             43
    ##  9  2013     7     1      100           2146       194      327             30
    ## 10  2013     7     1      100           2245       135      337            135
    ## # ... with 86,316 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

#3.1.¿Cuántos vuelos tienen datos faltantes en horario_salida?

``` r
count(filter(flights, is.na(dep_time)))
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1  8255

#3.2.Qué otras variables tienen valores faltantes?

``` r
(filter(flights, is.na(dep_time)))
```

    ## # A tibble: 8,255 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1       NA           1630        NA       NA           1815
    ##  2  2013     1     1       NA           1935        NA       NA           2240
    ##  3  2013     1     1       NA           1500        NA       NA           1825
    ##  4  2013     1     1       NA            600        NA       NA            901
    ##  5  2013     1     2       NA           1540        NA       NA           1747
    ##  6  2013     1     2       NA           1620        NA       NA           1746
    ##  7  2013     1     2       NA           1355        NA       NA           1459
    ##  8  2013     1     2       NA           1420        NA       NA           1644
    ##  9  2013     1     2       NA           1321        NA       NA           1536
    ## 10  2013     1     2       NA           1545        NA       NA           1910
    ## # ... with 8,245 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
# dep_delay, arr_time, arr_delay, tailnum, air_time.
```

#3.3.Qué otras variables tienen valores faltantes? Las filas con datos
faltantes(NA) representan los vuelos que fueron cancelados. No presenta
datos de retraso en salida o en llegada ya que el vuelo nunca despegó,
pero sí se conoce el nombre de las areolíneas.

#PARTE 2: DPLYR-ARRANGE #A.¿Cómo podrías usar arrange() para ordenar
todos los valores faltantes al comienzo? (Sugerencia: usa is.na())

``` r
arrange(flights,is.na(air_time))
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

#B.Ordena vuelos para encontrar los vuelos más retrasados.Encuentra los
vuelos que salieron más temprano.

``` r
View(arrange(flights, desc(dep_delay)))
View(arrange(flights, dep_delay > 0)) 
```

#C.Ordena vuelos para encontrar los vuelos más rápidos(que viajaron a
mayor velocidad).

``` r
vuelos02 <- arrange( flights, desc(distance / air_time))
View(head(vuelos02))
```

#D.¿Cuáles vuelos viajaron más lejos? ¿Cuál viajó más cerca?.

``` r
máslejos <- arrange(flights, desc(distance))
View(máslejos)
máscerca <- arrange(flights, distance)
máscerca
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     7    27       NA            106        NA       NA            245
    ##  2  2013     1     3     2127           2129        -2     2222           2224
    ##  3  2013     1     4     1240           1200        40     1333           1306
    ##  4  2013     1     4     1829           1615       134     1937           1721
    ##  5  2013     1     4     2128           2129        -1     2218           2224
    ##  6  2013     1     5     1155           1200        -5     1241           1306
    ##  7  2013     1     6     2125           2129        -4     2224           2224
    ##  8  2013     1     7     2124           2129        -5     2212           2224
    ##  9  2013     1     8     2127           2130        -3     2304           2225
    ## 10  2013     1     9     2126           2129        -3     2217           2224
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

#PARTE 3: DPLYR-SELECT #A.Haz una lluvia de ideas sobre tantas maneras
como sea posible para seleccionar dep_time, dep_delay, arr_time and
arr_delay de fligths.

``` r
select(flights, dep_time, dep_delay, arr_time, arr_delay)
```

    ## # A tibble: 336,776 x 4
    ##    dep_time dep_delay arr_time arr_delay
    ##       <int>     <dbl>    <int>     <dbl>
    ##  1      517         2      830        11
    ##  2      533         4      850        20
    ##  3      542         2      923        33
    ##  4      544        -1     1004       -18
    ##  5      554        -6      812       -25
    ##  6      554        -4      740        12
    ##  7      555        -5      913        19
    ##  8      557        -3      709       -14
    ##  9      557        -3      838        -8
    ## 10      558        -2      753         8
    ## # ... with 336,766 more rows

``` r
select(flights, starts_with("carrier"), starts_with("time"))
```

    ## # A tibble: 336,776 x 2
    ##    carrier time_hour          
    ##    <chr>   <dttm>             
    ##  1 UA      2013-01-01 05:00:00
    ##  2 UA      2013-01-01 05:00:00
    ##  3 AA      2013-01-01 05:00:00
    ##  4 B6      2013-01-01 05:00:00
    ##  5 DL      2013-01-01 06:00:00
    ##  6 UA      2013-01-01 05:00:00
    ##  7 B6      2013-01-01 06:00:00
    ##  8 EV      2013-01-01 06:00:00
    ##  9 B6      2013-01-01 06:00:00
    ## 10 AA      2013-01-01 06:00:00
    ## # ... with 336,766 more rows

``` r
select(flights, ends_with("time"), ends_with("delay"))
```

    ## # A tibble: 336,776 x 7
    ##    dep_time sched_dep_time arr_time sched_arr_time air_time dep_delay arr_delay
    ##       <int>          <int>    <int>          <int>    <dbl>     <dbl>     <dbl>
    ##  1      517            515      830            819      227         2        11
    ##  2      533            529      850            830      227         4        20
    ##  3      542            540      923            850      160         2        33
    ##  4      544            545     1004           1022      183        -1       -18
    ##  5      554            600      812            837      116        -6       -25
    ##  6      554            558      740            728      150        -4        12
    ##  7      555            600      913            854      158        -5        19
    ##  8      557            600      709            723       53        -3       -14
    ##  9      557            600      838            846      140        -3        -8
    ## 10      558            600      753            745      138        -2         8
    ## # ... with 336,766 more rows

``` r
select(flights, contains("arr"), contains("time"))
```

    ## # A tibble: 336,776 x 8
    ##    arr_time sched_arr_time arr_delay carrier dep_time sched_dep_time air_time
    ##       <int>          <int>     <dbl> <chr>      <int>          <int>    <dbl>
    ##  1      830            819        11 UA           517            515      227
    ##  2      850            830        20 UA           533            529      227
    ##  3      923            850        33 AA           542            540      160
    ##  4     1004           1022       -18 B6           544            545      183
    ##  5      812            837       -25 DL           554            600      116
    ##  6      740            728        12 UA           554            558      150
    ##  7      913            854        19 B6           555            600      158
    ##  8      709            723       -14 EV           557            600       53
    ##  9      838            846        -8 B6           557            600      140
    ## 10      753            745         8 AA           558            600      138
    ## # ... with 336,766 more rows, and 1 more variable: time_hour <dttm>

#B.¿Qué sucede si incluyes el nombre de una variable varias veces en una
llamada a select()?

``` r
select(flights, dep_delay, arr_time, dep_delay, carrier, carrier)
```

    ## # A tibble: 336,776 x 3
    ##    dep_delay arr_time carrier
    ##        <dbl>    <int> <chr>  
    ##  1         2      830 UA     
    ##  2         4      850 UA     
    ##  3         2      923 AA     
    ##  4        -1     1004 B6     
    ##  5        -6      812 DL     
    ##  6        -4      740 UA     
    ##  7        -5      913 B6     
    ##  8        -3      709 EV     
    ##  9        -3      838 B6     
    ## 10        -2      753 AA     
    ## # ... with 336,766 more rows

``` r
#Solo se considerará una vez.
```

#C.¿Qué hace la función any_of()? ¡¿Por qué podría ser útil en conjunto
con este vector?

``` r
valors <- c("month","day","arr_delay","arr_time","carrier")
select(flights, any_of(valors))
```

    ## # A tibble: 336,776 x 5
    ##    month   day arr_delay arr_time carrier
    ##    <int> <int>     <dbl>    <int> <chr>  
    ##  1     1     1        11      830 UA     
    ##  2     1     1        20      850 UA     
    ##  3     1     1        33      923 AA     
    ##  4     1     1       -18     1004 B6     
    ##  5     1     1       -25      812 DL     
    ##  6     1     1        12      740 UA     
    ##  7     1     1        19      913 B6     
    ##  8     1     1       -14      709 EV     
    ##  9     1     1        -8      838 B6     
    ## 10     1     1         8      753 AA     
    ## # ... with 336,766 more rows

``` r
#Podemos indicar las variables que queremos seleccionar con el nombre del vector que las contiene.
```

#PARTE 4: DPLYR-MUTATE #A.Las variables horario_salida y
salida_programada tienen un formato conveniente para leer, pero es
difícil realizar cualquier cálculo con ellas porque no son realmente
números continuos. Transfórmalas hacia un formato más conveniente como
número de minutos desde la medianoche.

``` r
#Vemos que las variables son de la forma 6.59 AM y se toman como 659, entonces el número de horas desde la medianoche es: 
659%/%100
```

    ## [1] 6

``` r
#La cantidad de minutos en esas 6 horas 
(659%/%100)*60
```

    ## [1] 360

``` r
#sumamos los 59 minutos
659%%100
```

    ## [1] 59

``` r
#El número de minutos transcurridos desde las 0:00 hasta las 6:59 AM son:
(659%/%100*60)+659 %% 100
```

    ## [1] 419

``` r
#La medianoche toma el valor de 24:00 con lo cual el número de minutos será 1440 en lugar de 0.
(2400 %/% 100*60 + 2400 %% 100) %% 1440
```

    ## [1] 0

``` r
#Entonces...
mutate(flights, horario_salida_min = (dep_time %/% 100*60 + dep_time %% 100) %% 1440)
```

    ## # A tibble: 336,776 x 20
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 336,766 more rows, and 12 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>,
    ## #   horario_salida_min <dbl>

``` r
mutate(flights, salida_programada_min = (dep_delay %/% 100*60 + dep_delay %% 100) %% 1440)
```

    ## # A tibble: 336,776 x 20
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 336,766 more rows, and 12 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>,
    ## #   salida_programada_min <dbl>

#B.Compara tiempo_vuelo con horario_llegada - horario_salida. ¿Qué
esperas ver? ¿Qué ves? ¿Qué necesitas hacer para arreglarlo?

``` r
vuelos01 <- select(flights, dep_time, arr_time)
mutate(flights, 
       tiempo_vuelo = arr_time - dep_time)
```

    ## # A tibble: 336,776 x 20
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 336,766 more rows, and 12 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>,
    ## #   tiempo_vuelo <int>

``` r
#Se espera ver air_time = arr_time - dep_time, con los datos verdaderos pero nos sale otra respuesta. Veo que influye bastante el atraso de salida y el atraso de llegada.
#Para solucionarlo deberíamos calcular la diferencia de llegada_programada - salida_programada para que así, nos dé el tiempo_vuelo correcto.
```

#C.Compara horario_salida, salida_programada, y atraso_salida. ¿Cómo
esperarías que esos tres números estén relacionados?

``` r
#Podemos indicar que el atraso_salida = horario_salida - salida_programada
select(flights, dep_time, sched_dep_time, dep_delay)
```

    ## # A tibble: 336,776 x 3
    ##    dep_time sched_dep_time dep_delay
    ##       <int>          <int>     <dbl>
    ##  1      517            515         2
    ##  2      533            529         4
    ##  3      542            540         2
    ##  4      544            545        -1
    ##  5      554            600        -6
    ##  6      554            558        -4
    ##  7      555            600        -5
    ##  8      557            600        -3
    ##  9      557            600        -3
    ## 10      558            600        -2
    ## # ... with 336,766 more rows

``` r
transmute(flights, tiempo_vuel0 = dep_time - sched_dep_time)
```

    ## # A tibble: 336,776 x 1
    ##    tiempo_vuel0
    ##           <int>
    ##  1            2
    ##  2            4
    ##  3            2
    ##  4           -1
    ##  5          -46
    ##  6           -4
    ##  7          -45
    ##  8          -43
    ##  9          -43
    ## 10          -42
    ## # ... with 336,766 more rows

#D.Encuentra los 10 vuelos más retrasados utilizando una función de
ordenamiento. ¿Cómo quieres manejar los empates? Lee atentamente la
documentación de min_rank().

``` r
vuelos03<-head(arrange(flights,desc(arr_delay)),10)
vuelos03
```

    ## # A tibble: 10 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     9      641            900      1301     1242           1530
    ##  2  2013     6    15     1432           1935      1137     1607           2120
    ##  3  2013     1    10     1121           1635      1126     1239           1810
    ##  4  2013     9    20     1139           1845      1014     1457           2210
    ##  5  2013     7    22      845           1600      1005     1044           1815
    ##  6  2013     4    10     1100           1900       960     1342           2211
    ##  7  2013     3    17     2321            810       911      135           1020
    ##  8  2013     7    22     2257            759       898      121           1026
    ##  9  2013    12     5      756           1700       896     1058           2020
    ## 10  2013     5     3     1133           2055       878     1250           2215
    ## # ... with 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
vuelos_Re<-vuelos03$arr_delay
vuelos_Re
```

    ##  [1] 1272 1127 1109 1007  989  931  915  895  878  875

``` r
min_rank(vuelos_Re)
```

    ##  [1] 10  9  8  7  6  5  4  3  2  1

#E.¿Qué devuelve 1:3 + 1:10? ¿Por qué?

``` r
1:3 + 1:10
```

    ## Warning in 1:3 + 1:10: longitud de objeto mayor no es múltiplo de la longitud de
    ## uno menor

    ##  [1]  2  4  6  5  7  9  8 10 12 11

``` r
#Nos devuelve que la longitud del objeto más largo(1:10) no es un múltiplo de la longitud del objeto más corto(1:3)
```

#F.¿Qué funciones trigonométricas proporciona R?

``` r
#Nos brinda Seno en R, Coseno en R, Tangente en R y Cotangente en R:
sin(pi/3)
```

    ## [1] 0.8660254

``` r
cos(pi)
```

    ## [1] -1

``` r
tan(pi/2)
```

    ## [1] 1.633124e+16

``` r
1/tan(pi)
```

    ## [1] -8.16562e+15

``` r
tan(pi)
```

    ## [1] -1.224647e-16

``` r
tan(2*pi)
```

    ## [1] -2.449294e-16

``` r
a=asin(1)
sin(a)
```

    ## [1] 1

#PARTE 5: DPLYR-GROUP BY & SUMMARIZE #A.Haz una lluvia de ideas de al
menos 5 formas diferentes de evaluar las características de un retraso
típico de un grupo de vuelos. Considera los siguientes escenarios:

``` r
#Un vuelo llega 15 minutos antes 50% del tiempo, y 15 minutos tarde 50% del tiempo.

#Un vuelo llega siempre 10 minutos tarde.
filter(flights, arr_delay == 10) %>%
  group_by(year, month, day, flight)
```

    ## # A tibble: 3,373 x 19
    ## # Groups:   year, month, day, flight [3,370]
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      624            630        -6      840            830
    ##  2  2013     1     1      717            720        -3      850            840
    ##  3  2013     1     1      745            745         0     1135           1125
    ##  4  2013     1     1      805            805         0     1015           1005
    ##  5  2013     1     1      811            815        -4     1026           1016
    ##  6  2013     1     1      921            900        21     1237           1227
    ##  7  2013     1     1     1158           1205        -7     1530           1520
    ##  8  2013     1     1     1211           1215        -4     1423           1413
    ##  9  2013     1     1     1455           1459        -4     1655           1645
    ## 10  2013     1     1     1554           1600        -6     1830           1820
    ## # ... with 3,363 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
#Un vuelo llega 30 minutos antes 50% del tiempo, y 30 minutos tarde 50% del tiempo.
filter(flights, arr_delay > 30) %>%
  group_by(year, month, day) %>%
  summarise(min30_tarde = quantile(arr_delay, 0.5, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'year', 'month'. You can override using the `.groups` argument.

    ## # A tibble: 365 x 4
    ## # Groups:   year, month [12]
    ##     year month   day min30_tarde
    ##    <int> <int> <int>       <dbl>
    ##  1  2013     1     1        54  
    ##  2  2013     1     2        58  
    ##  3  2013     1     3        51  
    ##  4  2013     1     4        49  
    ##  5  2013     1     5        65  
    ##  6  2013     1     6        52  
    ##  7  2013     1     7        59  
    ##  8  2013     1     8        50  
    ##  9  2013     1     9        48.5
    ## 10  2013     1    10        48  
    ## # ... with 355 more rows

``` r
filter(flights, arr_delay < 30) %>%
  group_by(year, month, day) %>%
  summarise(min30_antes = quantile(arr_delay, 0.5, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'year', 'month'. You can override using the `.groups` argument.

    ## # A tibble: 365 x 4
    ## # Groups:   year, month [12]
    ##     year month   day min30_antes
    ##    <int> <int> <int>       <dbl>
    ##  1  2013     1     1          -1
    ##  2  2013     1     2           0
    ##  3  2013     1     3          -2
    ##  4  2013     1     4         -10
    ##  5  2013     1     5          -8
    ##  6  2013     1     6          -4
    ##  7  2013     1     7         -11
    ##  8  2013     1     8          -9
    ##  9  2013     1     9          -7
    ## 10  2013     1    10         -12
    ## # ... with 355 more rows

``` r
#Un vuelo llega a tiempo en el 99% de los casos. 1% de las veces llega 2 horas tarde. ¿Qué es más importante: retraso de la llegada o demora de salida?
```

#B.Sugiere un nuevo enfoque que te dé el mismo output que no_cancelados
%>% count(destino) y no_cancelado %>% count(codigo_cola, wt = distancia)
(sin usar count()).

#C.Nuestra definición de vuelos cancelados (is.na(atraso_salida) \|
is.na (atraso_llegada)) es un poco subóptima. ¿Por qué? ¿Cuál es la
columna más importante?

#D.Mira la cantidad de vuelos cancelados por día. ¿Hay un patrón? ¿La
proporción de vuelos cancelados está relacionada con el retraso
promedio?

#E.¿Qué compañía tiene los peores retrasos? Desafío: ¿puedes desenredar
el efecto de malos aeropuertos vs. el efecto de malas aerolíneas? ¿Por
qué o por qué no? (Sugerencia: piensa en vuelos %>% group_by(aerolinea,
destino) %>% summarise(n()))

#F.¿Qué hace el argumento sort a count(). ¿Cuándo podrías usarlo?

#PARTE 7: DPLYR - TRANSFORMACIONES AGRUPADAS #A.Remítete a las listas de
funciones útiles de mutación y filtrado. Describe cómo cambia cada
operación cuando las combinas con la agrupación.

``` r
#El comando filter es para poner una condición a alguna de nuestras variables.
#El comando mutate es para agregar una columna en función de algunas columnas existentes.
#El comando select nos ayuda a reducir rápidamente a las variables o columnas que nos interesan.
```

#B.¿Qué avión (codigo_cola) tiene el peor registro de tiempo?

``` r
flights %>%
  filter(!is.na(tailnum), is.na(arr_time) | !is.na(arr_delay)) %>%
  mutate(on_time = !is.na(arr_time) & (arr_delay <= 0)) %>%
  group_by(tailnum) %>%
  summarise(on_time = mean(on_time), n = n()) %>%
  filter(n >= 20) %>%
  filter(min_rank(on_time) == 1)
```

    ## # A tibble: 1 x 3
    ##   tailnum on_time     n
    ##   <chr>     <dbl> <int>
    ## 1 N988AT    0.189    37

#C.¿A qué hora del día deberías volar si quieres evitar lo más posible
los retrasos?

#D.Para cada destino, calcula los minutos totales de demora. Para cada
vuelo, calcula la proporción de la demora total para su destino.

``` r
flights %>%
  filter(arr_delay > 0) %>%
  group_by(dest) %>%
  mutate(
    arr_delay_total = sum(arr_delay),
    min_demora = arr_delay / arr_delay_total
  ) %>%
  select(dest, month, day, dep_time, carrier, flight,
         arr_delay, min_demora) %>%
  arrange(dest, desc(min_demora))
```

    ## # A tibble: 133,004 x 8
    ## # Groups:   dest [103]
    ##    dest  month   day dep_time carrier flight arr_delay min_demora
    ##    <chr> <int> <int>    <int> <chr>    <int>     <dbl>      <dbl>
    ##  1 ABQ       7    22     2145 B6        1505       153     0.0341
    ##  2 ABQ      12    14     2223 B6          65       149     0.0332
    ##  3 ABQ      10    15     2146 B6          65       138     0.0308
    ##  4 ABQ       7    23     2206 B6        1505       137     0.0305
    ##  5 ABQ      12    17     2220 B6          65       136     0.0303
    ##  6 ABQ       7    10     2025 B6        1505       126     0.0281
    ##  7 ABQ       7    30     2212 B6        1505       118     0.0263
    ##  8 ABQ       7    28     2038 B6        1505       117     0.0261
    ##  9 ABQ      12     8     2049 B6          65       114     0.0254
    ## 10 ABQ       9     2     2212 B6        1505       109     0.0243
    ## # ... with 132,994 more rows

``` r
#2
flights %>%
  filter(arr_delay > 0) %>%
  group_by(dest, origin, carrier, flight) %>%
  summarise(arr_delay = sum(arr_delay)) %>%
  group_by(dest) %>%
  mutate(
    min_demora_total = arr_delay / sum(arr_delay)
  ) %>%
  arrange(dest, desc(min_demora_total)) %>%
  select(carrier, flight, origin, dest, min_demora_total)
```

    ## `summarise()` has grouped output by 'dest', 'origin', 'carrier'. You can override using the `.groups` argument.

    ## # A tibble: 8,834 x 5
    ## # Groups:   dest [103]
    ##    carrier flight origin dest  min_demora_total
    ##    <chr>    <int> <chr>  <chr>            <dbl>
    ##  1 B6        1505 JFK    ABQ             0.567 
    ##  2 B6          65 JFK    ABQ             0.433 
    ##  3 B6        1191 JFK    ACK             0.475 
    ##  4 B6        1491 JFK    ACK             0.414 
    ##  5 B6        1291 JFK    ACK             0.0898
    ##  6 B6        1195 JFK    ACK             0.0208
    ##  7 EV        4309 EWR    ALB             0.174 
    ##  8 EV        4271 EWR    ALB             0.137 
    ##  9 EV        4117 EWR    ALB             0.0951
    ## 10 EV        4088 EWR    ALB             0.0865
    ## # ... with 8,824 more rows

#E.Los retrasos suelen estar temporalmente correlacionados: incluso una
vez que el problema que causó el retraso inicial se ha resuelto, los
vuelos posteriores se retrasan para permitir que salgan los vuelos
anteriores. Usando lag(), explora cómo el retraso de un vuelo está
relacionado con el retraso del vuelo inmediatamente anterior.

``` r
lagged_delays <- flights %>%
  arrange(origin, month, day, dep_time) %>%
  group_by(origin) %>%
  mutate(dep_delay_lag = lag(dep_delay)) %>%
  filter(!is.na(dep_time), !is.na(dep_delay_lag))
lagged_delays 
```

    ## # A tibble: 327,649 x 20
    ## # Groups:   origin [3]
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      554            558        -4      740            728
    ##  2  2013     1     1      555            600        -5      913            854
    ##  3  2013     1     1      558            600        -2      923            937
    ##  4  2013     1     1      559            600        -1      854            902
    ##  5  2013     1     1      601            600         1      844            850
    ##  6  2013     1     1      606            610        -4      858            910
    ##  7  2013     1     1      607            607         0      858            915
    ##  8  2013     1     1      608            600         8      807            735
    ##  9  2013     1     1      615            615         0      833            842
    ## 10  2013     1     1      622            630        -8     1017           1014
    ## # ... with 327,639 more rows, and 12 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>,
    ## #   dep_delay_lag <dbl>

``` r
lagged_delays %>%
  group_by(dep_delay_lag) %>%
  summarise(dep_delay_mean = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay_mean, x = dep_delay_lag)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 1500, by = 120)) +
  labs(y = "Departure Delay", x = "Previous Departure Delay")
```

![](Desarrollo-N°3_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

``` r
lagged_delays %>%
  group_by(origin, dep_delay_lag) %>%
  summarise(dep_delay_mean = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay_mean, x = dep_delay_lag)) +
  geom_point() +
  facet_wrap(~ origin, ncol=1) +
  labs(y = "Departure Delay", x = "Previous Departure Delay")
```

    ## `summarise()` has grouped output by 'origin'. You can override using the `.groups` argument.

![](Desarrollo-N°3_files/figure-gfm/unnamed-chunk-35-2.png)<!-- -->
#F.Mira cada destino. ¿Puedes encontrar vuelos sospechosamente rápidos?
(es decir, vuelos que representan un posible error de entrada de datos).
Calcula el tiempo en el aire de un vuelo relativo al vuelo más corto a
ese destino. ¿Cuáles vuelos se retrasaron más en el aire?

``` r
standardized_flights <- flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest, origin) %>%
  mutate(
    air_time_mean = mean(air_time),
    air_time_sd = sd(air_time),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(air_time_standard = (air_time - air_time_mean) / (air_time_sd + 1))

ggplot(standardized_flights, aes(x = air_time_standard)) +
  geom_density()
```

    ## Warning: Removed 4 rows containing non-finite values (stat_density).

![](Desarrollo-N°3_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
standardized_flights %>%
  arrange(air_time_standard) %>%
  select(
    carrier, flight, origin, dest, month, day,
    air_time, air_time_mean, air_time_standard
  ) %>%
  head(10) %>%
  print(width = Inf)
```

    ## # A tibble: 10 x 9
    ##    carrier flight origin dest  month   day air_time air_time_mean
    ##    <chr>    <int> <chr>  <chr> <int> <int>    <dbl>         <dbl>
    ##  1 DL        1499 LGA    ATL       5    25       65         114. 
    ##  2 EV        4667 EWR    MSP       7     2       93         151. 
    ##  3 EV        4292 EWR    GSP       5    13       55          93.2
    ##  4 EV        3805 EWR    BNA       3    23       70         115. 
    ##  5 EV        4687 EWR    CVG       9    29       62          96.1
    ##  6 B6        2002 JFK    BUF      11    10       38          57.1
    ##  7 DL        1902 LGA    PBI       1    12      105         146. 
    ##  8 DL         161 JFK    SEA       7     3      275         329. 
    ##  9 EV        5486 LGA    PIT       4    28       40          57.7
    ## 10 B6          30 JFK    ROC       3    25       35          51.9
    ##    air_time_standard
    ##                <dbl>
    ##  1             -4.56
    ##  2             -4.46
    ##  3             -4.20
    ##  4             -3.73
    ##  5             -3.60
    ##  6             -3.38
    ##  7             -3.34
    ##  8             -3.34
    ##  9             -3.15
    ## 10             -3.10

``` r
standardized_flights2 <- flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest, origin) %>%
  mutate(
    air_time_median = median(air_time),
    air_time_iqr = IQR(air_time),
    n = n(),
    air_time_standard = (air_time - air_time_median) / air_time_iqr)

ggplot(standardized_flights2, aes(x = air_time_standard)) +
  geom_density()
```

    ## Warning: Removed 4 rows containing non-finite values (stat_density).

![](Desarrollo-N°3_files/figure-gfm/unnamed-chunk-36-2.png)<!-- -->

``` r
standardized_flights2 %>%
  arrange(air_time_standard) %>%
  select(
    carrier, flight, origin, dest, month, day, air_time,
    air_time_median, air_time_standard
  ) %>%
  head(10) %>%
  print(width = Inf)
```

    ## # A tibble: 10 x 9
    ## # Groups:   dest, origin [10]
    ##    carrier flight origin dest  month   day air_time air_time_median
    ##    <chr>    <int> <chr>  <chr> <int> <int>    <dbl>           <dbl>
    ##  1 EV        4667 EWR    MSP       7     2       93             149
    ##  2 DL        1499 LGA    ATL       5    25       65             112
    ##  3 US        2132 LGA    BOS       3     2       21              37
    ##  4 B6          30 JFK    ROC       3    25       35              51
    ##  5 B6        2002 JFK    BUF      11    10       38              57
    ##  6 EV        4292 EWR    GSP       5    13       55              92
    ##  7 EV        4249 EWR    SYR       3    15       30              39
    ##  8 EV        4580 EWR    BTV       6    29       34              46
    ##  9 EV        3830 EWR    RIC       7     2       35              53
    ## 10 EV        4687 EWR    CVG       9    29       62              95
    ##    air_time_standard
    ##                <dbl>
    ##  1             -3.5 
    ##  2             -3.36
    ##  3             -3.2 
    ##  4             -3.2 
    ##  5             -3.17
    ##  6             -3.08
    ##  7             -3   
    ##  8             -3   
    ##  9             -3   
    ## 10             -3

``` r
flights %>%
  mutate(mph = distance / (air_time / 60)) %>%
  ggplot(aes(x = mph)) +
  geom_histogram(binwidth = 10)
```

    ## Warning: Removed 9430 rows containing non-finite values (stat_bin).

![](Desarrollo-N°3_files/figure-gfm/unnamed-chunk-36-3.png)<!-- -->

``` r
flights %>%
  mutate(mph = distance / (air_time / 60)) %>%
  arrange(desc(mph)) %>%
  select(mph, flight, carrier, flight, month, day, dep_time) %>%
  head(5)
```

    ## # A tibble: 5 x 6
    ##     mph flight carrier month   day dep_time
    ##   <dbl>  <int> <chr>   <int> <int>    <int>
    ## 1  703.   1499 DL          5    25     1709
    ## 2  650.   4667 EV          7     2     1558
    ## 3  648    4292 EV          5    13     2040
    ## 4  641.   3805 EV          3    23     1914
    ## 5  591.   1902 DL          1    12     1559

``` r
flights %>%
  mutate(mph = distance / (air_time / 60)) %>%
  arrange(desc(mph)) %>%
  select(
    origin, dest, mph, year, month, day, dep_time, flight, carrier,
    dep_delay, arr_delay
  )
```

    ## # A tibble: 336,776 x 11
    ##    origin dest    mph  year month   day dep_time flight carrier dep_delay
    ##    <chr>  <chr> <dbl> <int> <int> <int>    <int>  <int> <chr>       <dbl>
    ##  1 LGA    ATL    703.  2013     5    25     1709   1499 DL              9
    ##  2 EWR    MSP    650.  2013     7     2     1558   4667 EV             45
    ##  3 EWR    GSP    648   2013     5    13     2040   4292 EV             15
    ##  4 EWR    BNA    641.  2013     3    23     1914   3805 EV              4
    ##  5 LGA    PBI    591.  2013     1    12     1559   1902 DL             -1
    ##  6 JFK    SJU    564   2013    11    17      650    315 DL             -5
    ##  7 JFK    SJU    557.  2013     2    21     2355    707 B6             -3
    ##  8 JFK    STT    556.  2013    11    17      759    936 AA             -1
    ##  9 JFK    SJU    554.  2013    11    16     2003    347 DL             38
    ## 10 JFK    SJU    554.  2013    11    16     2349   1503 B6            -10
    ## # ... with 336,766 more rows, and 1 more variable: arr_delay <dbl>

``` r
air_time_delayed <-
  flights %>%
  group_by(origin, dest) %>%
  mutate(
    air_time_min = min(air_time, na.rm = TRUE),
    air_time_delay = air_time - air_time_min,
    air_time_delay_pct = air_time_delay / air_time_min * 100
  )
```

    ## Warning in min(air_time, na.rm = TRUE): ningún argumento finito para min;
    ## retornando Inf

``` r
air_time_delayed %>%
  arrange(desc(air_time_delay)) %>%
  select(
    air_time_delay, carrier, flight,
    origin, dest, year, month, day, dep_time,
    air_time, air_time_min
  ) %>%
  head() %>%
  print(width = Inf)
```

    ## # A tibble: 6 x 11
    ## # Groups:   origin, dest [5]
    ##   air_time_delay carrier flight origin dest   year month   day dep_time air_time
    ##            <dbl> <chr>    <int> <chr>  <chr> <int> <int> <int>    <int>    <dbl>
    ## 1            189 DL         841 JFK    SFO    2013     7    28     1727      490
    ## 2            165 DL         426 JFK    LAX    2013    11    22     1812      440
    ## 3            163 AA         575 JFK    EGE    2013     1    28     1806      382
    ## 4            147 DL          17 JFK    LAX    2013     7    10     1814      422
    ## 5            145 UA         745 LGA    DEN    2013     9    10     1513      331
    ## 6            143 UA         587 EWR    LAS    2013    11    22     2142      399
    ##   air_time_min
    ##          <dbl>
    ## 1          301
    ## 2          275
    ## 3          219
    ## 4          275
    ## 5          186
    ## 6          256

``` r
air_time_delayed %>%
  arrange(desc(air_time_delay)) %>%
  select(
    air_time_delay_pct, carrier, flight,
    origin, dest, year, month, day, dep_time,
    air_time, air_time_min
  ) %>%
  head() %>%
  print(width = Inf)
```

    ## # A tibble: 6 x 11
    ## # Groups:   origin, dest [5]
    ##   air_time_delay_pct carrier flight origin dest   year month   day dep_time
    ##                <dbl> <chr>    <int> <chr>  <chr> <int> <int> <int>    <int>
    ## 1               62.8 DL         841 JFK    SFO    2013     7    28     1727
    ## 2               60   DL         426 JFK    LAX    2013    11    22     1812
    ## 3               74.4 AA         575 JFK    EGE    2013     1    28     1806
    ## 4               53.5 DL          17 JFK    LAX    2013     7    10     1814
    ## 5               78.0 UA         745 LGA    DEN    2013     9    10     1513
    ## 6               55.9 UA         587 EWR    LAS    2013    11    22     2142
    ##   air_time air_time_min
    ##      <dbl>        <dbl>
    ## 1      490          301
    ## 2      440          275
    ## 3      382          219
    ## 4      422          275
    ## 5      331          186
    ## 6      399          256

#G.Encuentra todos los destinos que son volados por al menos dos
operadores. Usa esta información para clasificar a las aerolíneas.

``` r
flights %>%
  group_by(dest) %>%
  mutate(n_carriers = n_distinct(carrier)) %>%
  filter(n_carriers > 1)
```

    ## # A tibble: 325,397 x 20
    ## # Groups:   dest [76]
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 325,387 more rows, and 12 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>,
    ## #   n_carriers <int>

``` r
flights %>%
group_by(carrier) %>%
  summarize(n_dest = n_distinct(dest)) %>%
  arrange(desc(n_dest))
```

    ## # A tibble: 16 x 2
    ##    carrier n_dest
    ##    <chr>    <int>
    ##  1 EV          61
    ##  2 9E          49
    ##  3 UA          47
    ##  4 B6          42
    ##  5 DL          40
    ##  6 MQ          20
    ##  7 AA          19
    ##  8 WN          11
    ##  9 US           6
    ## 10 OO           5
    ## 11 VX           5
    ## 12 FL           3
    ## 13 YV           3
    ## 14 AS           1
    ## 15 F9           1
    ## 16 HA           1

#H.Para cada avión, cuenta el número de vuelos antes del primer retraso
de más de 1 hora.

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](Desarrollo-N°3_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
