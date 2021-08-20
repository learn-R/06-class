
# Práctico 5: Manipulación de tidydata ------------------------------------


# 1. Instalación de paquetes ----------------------------------------------

library(pacman)
pacman::p_load(tidyverse,
               tidyr,
               lubridate)


# 2. Importar datos ---------------------------------------------------------

datos <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

# 3. Seleccionar columnas -------------------------------------------------

datos <- datos %>% select(-c(3:4))


# 4. pivot_longer() ---------------------------------------------------------

datos <- datos %>% pivot_longer(cols=-(1:2), names_to = "fecha", values_to = "casos")


## Bonus: Eliminar caracteres con gsub -------------------------------------

datos$fecha <- gsub("X", "", datos$fecha)

# 5. pivot_wider() --------------------------------------------------------

datos <- datos %>% pivot_wider(names_from = "fecha", values_from = "casos")
