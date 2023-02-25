###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################


## libraries
library(tidyverse)
library(ggtext)
library(lubridate)
library(ggrepel)



## adquisición de datos
suppressMessages(source("source/data_clean_ind_00.R"))

# grafica previa
df01 %>% 
  filter(fecha < "2018-07-02") %>% 
  filter(concepto == "Indicador Global de la Actividad Económica") %>% 
  ggplot(aes(x = fecha, y = valor)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "darkred")

# ultimo registro
df01 %>% 
  slice_max(order_by = fecha, n =1)

# modelo lineal
mode_lineal <- lm(valor ~ fecha, data = df01 %>% 
                    filter(fecha < "2018-07-02") %>% 
                    filter(concepto == "Indicador Global de la Actividad Económica"))


## comparación con la tendencia
df01 %>% 
  filter(concepto == "Indicador Global de la Actividad Económica") %>% 
  mutate(predi = predict(mode_lineal, newdata = .)) %>% 
  summarize(pct_cambio = (last(.) %>% pull(valor) - last(.) %>% pull(predi)) /
              last(.) %>% pull(predi)) %>% 
  knitr::kable(digits = 4, 
               col.names = "Diferencia respecto a la tendencia")


## visualización de la serie
df01 %>% 
  filter(concepto == "Indicador Global de la Actividad Económica") %>% 
  mutate(predi = predict(mode_lineal, newdata = .)) %>% 
  ggplot(aes(x = fecha, y = valor)) +
    geom_rect(xmin = as_date("2018-07-02"),
              xmax = max(df01$fecha),
              ymin = -Inf,
              ymax = Inf,
              fill = "#F7F0E0",
              alpha = 0.35, 
              show.legend = FALSE) +
    geom_line(alpha = 0.7,
              color = "steelblue",
              linewidth = 0.6) +
    geom_smooth(aes(y = predi), 
                se = FALSE, 
                color = "darkred", 
                linewidth = 0.5) +
    annotate(geom = "text",
             label = "Administración de López Obrador",
             x = as_date("2021-07-31"),
             y = 80, 
             angle = 90,
             size = 3.0, 
             color = "darkgrey",
             family = "Encode Sans Condensed") +
    theme(text = element_text(family = "Encode Sans Condensed"),
          plot.title.position = "plot",
          plot.title = element_text(face = "bold", 
                                    size = 16),
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.line = element_line(color = "darkgrey"),
          panel.grid = element_line(color = "grey95"),
          plot.caption.position = "plot",
          plot.caption = element_markdown(color = "darkgrey", 
                                          hjust = 0)) +
    labs(title = "Efecto de la Administración de López Obrador en la Economía",
         subtitle = "La línea roja indica la tendencia de la Economía Mexicana hasta la última elección presidencial.",
         x = NULL,
         y = "Índice (Base 100 = 2013)",
         caption = "Fuente: INEGI, 
           Indicador Global de la Actividad Económica, 
           series desestacionalizadas. <br>
           Visualización: Juan L. Bretón, PMP | @BretonPmp") +
    scale_x_date(date_labels = "%Y",
                 breaks = seq.Date(from = min(df01$fecha),
                                   to = max(df01$fecha),
                                   by = "4 year"))
  

  
  