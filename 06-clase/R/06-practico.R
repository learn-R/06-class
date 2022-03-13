
# Práctico 6: Transformar variables ------------------------------------


# 1. Instalación de paquetes ----------------------------------------------

pacman::p_load(tidyverse,
               haven, 
               forcats,
               car,
               sjmisc)


# 2. Importar datos ---------------------------------------------------------

datos <- readRDS("input/data/datos_proc.rds")

# 3. Explorar datos -------------------------------------------------

head(datos)


# 4. mutate() -------------------------------------------------------------



## a) Cálculo --------------------------------------------------------------

mutate(datos, nueva_variable = 3+2)
mutate(datos, nueva_variable = 3+2,
       ingreso_percapita = ytoth/tot_per)


## b) recode() ----------------------------------------------------------------------

## dplyr
datos %>% 
  mutate(sexo = dplyr::recode(sexo, "Mujer" = "Femenino", "Hombre" = "Masculino"))

## car::recode()

datos %$% 
  car::recode(.$ife, '9=NA')
datos %>% 
  mutate(sexo = car::recode(.$sexo, c('"Mujer"="Femenino";"Hombre"= "Masculino"'), 
                            as.factor = T, # Transformar en factor
                            levels = c("Masculino", "Femenino"))) #Ordenamos los niveles del factor


## c) ifelse() -----------------------------------------------------------

datos %>% 
  mutate(ife_d= ifelse(ife == 1, 1, 0))

datos %>% 
  mutate(ife_d= ifelse(ife == 1 & sexo == 'Mujer', 1, 0))

datos %>% 
  mutate(validador_ingreso = ifelse(is.na(ytoth), FALSE, TRUE))

## d) case_when() --------------------------------------------------------

datos %>% 
  mutate(edad_tramo = case_when(edad <=39 ~  "Joven",
                                edad > 39 & edad <=59 ~ "Adulto",
                                edad > 59 ~ "Adulto mayor",
                                TRUE ~ NA_character_)) %>% 
  select(edad, edad_tramo)


datos %>% 
  mutate(sexo_edad_tramo = case_when(sexo == "Hombre" & edad <=39 ~  "Hombre joven",
                                     sexo == "Mujer" & edad <=39 ~  "Mujer joven",
                                     sexo == 'Hombre' & (edad > 39 & edad <=59) ~ "Hombre adulto",
                                     sexo == 'Mujer' & (edad > 39 & edad <=59) ~ "Mujer adulta",
                                     sexo == 'Hombre' & edad > 59 ~ "Adulto mayor",
                                     sexo == 'Mujer' & edad > 59 ~ "Adulta mayor",
                                     TRUE ~ NA_character_)) %>% 
  select(sexo, edad, sexo_edad_tramo)


## rowwise() para trabajar por filas ---------------------------------------

datos %>% #Especificamos que trabajaremos con el dataframe datos
  rowwise() %>% #Especificamos que agruparemos por filas
  mutate(ing_pc = ytoth/tot_per) %>% #Creamos una nueva variable llamada ing_tot, sumando los valores de ss_t, svar_t y reg_t para cada fila 
  ungroup() # Desagrupamos (dejamos de trabajar en razón de filas)


# 5. mutate_at() -------------------------------------------------------------

## Con mutate()

datos %>% 
  mutate(ocupacion = as.numeric(.$ocupacion),
         ife = as.numeric(.$ife)) %>% 
  mutate(ocupacion = car::recode(.$ocupacion, c("1 = 'Si'; 2 = 'No'"), as.factor = T),
         ife = car::recode(.$ife, c("1 = 'Si'; 2 = 'No'"), as.factor = T))

# Con mutate_at()

datos %>% 
  mutate_at(vars(ocupacion, ife), ~(as.numeric(.))) %>% 
  mutate_at(vars(ocupacion, ife), ~(car::recode(., c("1 = 'Si'; 2 = 'No'"), as.factor = T)))

# 6. mutate_if() -------------------------------------------------------------

datos %>% 
  mutate_if(is.labelled, ~(forcats::as_factor(.)))

# 7. mutate_all() ------------------------------------------------------------

datos %>% 
  mutate_all(~(as.character(.)))


# 8. Resumen --------------------------------------------------------------


### Revisamos antes de modificar
datos %>% 
  mutate(sexo = car::recode(.$sexo, c('"Mujer"="Femenino";"Hombre"= "Masculino"'), 
                            as.factor = T, # Transformar en factor
                            levels = c("Masculino", "Femenino")),
         ife_d= ifelse(ife == 1 & sexo == 'Mujer', 1, 0),
         sexo_edad_tramo = case_when(sexo == "Masculino" & edad <=39 ~  "Hombre joven",
                                     sexo == "Femenino" & edad <=39 ~  "Mujer joven",
                                     sexo == 'Masculino' & (edad > 39 & edad <=59) ~ "Hombre adulto",
                                     sexo == 'Femenino' & (edad > 39 & edad <=59) ~ "Mujer adulta",
                                     sexo == 'Masculino' & edad > 59 ~ "Adulto mayor",
                                     sexo == 'Femenino' & edad > 59 ~ "Adulta mayor",
                                     TRUE ~ NA_character_)) %>% 
  mutate_if(is.labelled, ~(forcats::as_factor(.))) %>% 
  rowwise() %>% 
  mutate(ing_pc = ytoth/tot_per) %>%  
  ungroup() 

### Modificamos

datos_proc <- datos %>% 
  mutate(sexo = car::recode(.$sexo, c('"Mujer"="Femenino";"Hombre"= "Masculino"'), 
                            as.factor = T, # Transformar en factor
                            levels = c("Masculino", "Femenino")),
         ife_d= ifelse(ife == 1 & sexo == 'Mujer', 1, 0),
         sexo_edad_tramo = case_when(sexo == "Masculino" & edad <=39 ~  "Hombre joven",
                                     sexo == "Femenino" & edad <=39 ~  "Mujer joven",
                                     sexo == 'Masculino' & (edad > 39 & edad <=59) ~ "Hombre adulto",
                                     sexo == 'Femenino' & (edad > 39 & edad <=59) ~ "Mujer adulta",
                                     sexo == 'Masculino' & edad > 59 ~ "Adulto mayor",
                                     sexo == 'Femenino' & edad > 59 ~ "Adulta mayor",
                                     TRUE ~ NA_character_)) %>% 
  mutate_if(is.labelled, ~(forcats::as_factor(.))) %>% 
  rowwise() %>% 
  mutate(ing_pc = ytoth/tot_per) %>%  
  ungroup() 


# 9. Exportamos los datos ----------------------------------------------------

saveRDS(datos_proc, file = "output/data/datos_proc.rds")
