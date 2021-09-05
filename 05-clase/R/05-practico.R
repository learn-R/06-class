
# Práctico 5: Manipulación de tidydata ------------------------------------


# 1. Instalación de paquetes ----------------------------------------------

library(pacman)
pacman::p_load(tidyverse, #Universo de librerías para manipular datos
               tidyr, #Para pivotear la estructura de los datos
               dplyr) #Para manipular datos 


# 2. Importar datos ---------------------------------------------------------

datos <- readRDS(gzcon(url("https://github.com/learn-R/05-class/blob/main/input/data/ESI2020.rds?raw=true")))

# 3. Explorar datos -------------------------------------------------

frq(datos$sector)
frq(datos$sexo)
frq(datos$prevision)
frq(datos$salud)
frq(datos$contrato_dur)

descr(datos$ss_t) #Ingresos por sueldo y salarios netos
descr(datos$svar_t) #Ingresos por sueldos y salarios variables
descr(datos$reg_t) #Total ingresos por regalías (beneficios entregados por el empleador)

# 4. Agrupar por filas con `rowwise()` ---------------------------------------------------------

datos <- datos %>% #Especificamos que trabajaremos con el dataframe datos
  rowwise() %>% #Especificamos que agruparemos por filas
  mutate(ing_tot = sum(ss_t, svar_t, reg_t)) #Creamos una nueva variable llamada ing_tot, 
                                             #sumando los valores de ss_t, svar_t y reg_t para cada fila 


#Des-seleccionamos los datos que no seguiremos usando
datos <- datos %>% 
  select(-c(ss_t, svar_t, reg_t))


# 5. Agrupar por columnas con `group_by()` --------------------------------

datos %>% 
  group_by(sexo) %>% #Espeficicamos que agruparemos por sexo
  summarise(media = mean(ing_tot)) #Creamos una columna llamada media, calculando la 
                                   #media ingresos con la función `mean`


## a) Con frq() --------------------------------------------------------------

datos %>% 
  group_by(sector) %>% 
  frq(sexo)

## b) Con descr() ----------------------------------------------------------

datos %>% 
  group_by(sexo) %>% 
  descr(ing_tot)

# 6. Manipulación de datos (tidydata) -------------------------------------

## a) `pivot_longer()` -----------------------------------------------------

long <- datos %>% #Creamos un nuevo objeto a partir de datos
  select(-ing_tot) %>%  #Des-seleccionamos la variable ing_tot, pues es de class numeric
  pivot_longer(cols=-c(1, id), #Pivoteamos a lo largo todas las columnas salvo la primera (sector) 
               names_to = "variable", #Especificamos que la columna "variable" incluirá los nombres de cada una de las columnas pivoteadas
               values_to = "value") #Especificamos que la columna "value" incluirá los valores de cada una de las columnas pivoteadas

## b) `pivot_wider()` ------------------------------------------------------

wide <- long %>% #Creamos un nuevo objeto a partir de los datos de long 
  pivot_wider(names_from = "variable", #Pivoteamos, tomando el nombre de las columnas desde la columna "variable"
              values_from = "value") 


# 7. `separate()` ---------------------------------------------------------

separate <- datos %>% #Creamos un nuevo objeto desde datos
  mutate(sobre_prom = case_when(ing_tot <= mean(ing_tot) ~ "Bajo media", 
                                ing_tot > mean(ing_tot) ~ "Sobre media")) %>% #Creamos una variable condicional con case_when(), 
                                                                              #para identificar quienes estan bajo y sobre la media de ingreso
  separate(sobre_prom, #Especificamos la columna que queremos separar
           into = c("sobre_o_bajo", "media")) #Especificamos en cuántas columnas 
                                              #queremos separar los characteres, y cómo se llamarán tales columnas


# 8. `unite()` ------------------------------------------------------------

separate <- separate %>% #Modificamos el objeto separate
  unite(col = sobre_media, #Unimos en una nueva columna llamada sobre_media
        sobre_o_bajo:media, #Especificamos las columnas que deseamos unir
        sep = " ") #Especificamos el separador


# 9. Unir datos -----------------------------------------------------------


## a) `merge()` ------------------------------------------------------------

#Cargamos los datos que deseamos unir
esi2020_m <- readRDS(gzcon(url("https://github.com/learn-R/05-class/blob/main/input/data/ESI2020m.rds?raw=true"))) 

merge(x = datos, #Unimos el dataframe datos
      y = esi2020_m, #con el dataframe esi2020_m
      by = c("sexo", "ing_tot")) #a partir de las columnas "sexo" e "ing_tot"

## b) `bind_cols()` --------------------------------------------------------

a <- select(datos, 1:5) # Seleccionamos las primeras cinco columnas de datos
b <- select(datos, 3:7) # Seleccionamos las últimas cinco columnas de datos
col_un <- bind_cols(a, b) #Unimos ambos dataframes a partir de sus columnas

## c) `bind_rows()` --------------------------------------------------------

a <- a[1:10000,] # Seleccionamos las primeras 10.000 filas de a
b <- b[10001:18985,] # Seleccionamos las últimas 8.985 filas de b
row_un <- bind_rows(a, b)






