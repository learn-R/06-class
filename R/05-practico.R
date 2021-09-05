
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
