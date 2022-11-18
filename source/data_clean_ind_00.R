###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################


## libraries
library(tidyverse)
library(lubridate)



## adquisición de datos
url <- "https://www.inegi.org.mx/contenidos/temas/economia/cn/igae/tabulados/des/igae_indice.xlsx"

download.file(url, "file.xlsx")

df00 <- readxl::read_xlsx("file.xlsx", skip = 5, trim_ws = TRUE)

# df00 <- readxl::read_xlsx("raw/igae_indice.xlsx",
#                           skip = 5, 
#                           trim_ws = TRUE)

glimpse(df00)



## limpieza de datos
# total de columnas en el dataset original menos la que será removida
tot_cols <- ncol(df00) - 1

# código de mes
mes_cod <- c("ENE" = 1, "FEB" = 2, "MAR" = 3, "ABR" = 4,
             "MAY" = 5, "JUN" = 6, "JUL" = 7, "AGO" = 8,
             "SEP" = 9, "OCT" = 10, "NOV" = 11, "DIC" = 12)

# función para poner fecha columna de fecha
asigna_fecha <- function(base){
  base %>% 
    mutate(fecha = seq.Date(from = as_date("1993-01-01"),
                            along.with = base$num_mes,
                            by = "1 month"))
}


# dataframe con solo total y raíces
df01 <- df00 %>% 
  drop_na(ENE...3) %>% 
  rename(concepto = ...1) %>% 
  drop_na(concepto) %>% 
  select(- ...2) %>% 
  pivot_longer(cols = 2:all_of(tot_cols), 
               names_to = "mes",
               values_to = "valor") %>% 
  mutate(mes = str_remove(mes, "[:punct:]+\\d+"),
         num_mes = recode(mes, !!!mes_cod)) %>%
  nest(data = -concepto) %>% 
  mutate(nuevo = map(data, asigna_fecha)) %>% 
  select(-data) %>% 
  unnest(nuevo) %>% 
  relocate(fecha, .before = valor)


