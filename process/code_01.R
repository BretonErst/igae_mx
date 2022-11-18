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



## ddquisición de datos
suppressMessages(source("source/data_clean_ind_00.R"))



## visualización de la serie
df01 %>% 
  filter(concepto == "Indicador Global de la Actividad Económica") %>% 
  ggplot(aes(x = fecha, y = valor)) +
    geom_line(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "darkred") +
    theme(text = element_text(family = "Encode Sans Condensed"),
          plot.title.position = "plot",
          plot.title = element_text(face = "bold", 
                                    size = 16),
          plot.caption.position = "plot",
          plot.caption = element_markdown(color = "darkgrey", 
                                          hjust = 0)) +
    labs(title = "Desempeño del IGAE México",
         subtitle = "Base 100 = 2013",
         x = NULL,
         y = "Índice",
         caption = "Fuente: Elaboración propia con datos del INEGI <br>
             Juan L. Bretón, PMP")
  


# función para poner numero de índice
pon_num <- function(base) {
  seq_along(along.with = base$num_mes)
}

# función para calcular valor escalado
calc_escala <- function(base) {
  fac <- base %>% 
    slice_min(order_by = fecha) %>% 
    pull(valor)
  
  (base$valor - fac) / fac
}


# integrar presidente con base en fecha
df_global_01 <- df01 %>% 
  filter(concepto == "Indicador Global de la Actividad Económica") %>%
  mutate(presidente = as_factor(case_when(
    fecha >= as_date("1994-12-01") & fecha < as_date("2000-12-01") ~ "Zedillo",
    fecha >= as_date("2000-12-01") & fecha < as_date("2006-12-01") ~ "Fox",
    fecha >= as_date("2006-12-01") & fecha < as_date("2012-12-01") ~ "Calderón",
    fecha >= as_date("2012-12-01") & fecha < as_date("2018-12-01") ~ "Peña",
    fecha >= as_date("2018-12-01") ~ "López",
    TRUE ~ "Salinas"
  ))) %>%
  filter(presidente != "Salinas") %>%
  nest(data = -presidente)


# incorpora escala y mes de sexenio
df_global_02 <- df_global_01 %>% 
  mutate(mes_sexenio = map(data, pon_num),
         escalado = map(data, calc_escala)) %>% 
  unnest(everything())


# visualización escalada
df_global_02 %>%
  drop_na(valor) %>% 
  mutate(last_val = ifelse(mes_sexenio == max(mes_sexenio), 
                           as.character(presidente), 
                           NA)) %>%
  ggplot(aes(x = mes_sexenio,
             y = escalado,
             color = presidente)) +
    geom_line(alpha = 0.6) +
    # geom_text_repel(aes(label = last_val),
    #                 direction = "y",
    #                 hjust = -3,
    #                 segment.size = 1,
    #                 segment.alpha = 0.5,
    #                 nudge_y = -0.01,
    #                 segment.linetype = "dotted") +
    scale_x_continuous(limits = c(0, 75)) +
    scale_color_manual(name = "Presidente",
                       values = c("#34495E", "#F39C12", "#16A085", 
                                  "#8E44AD", "#C0392B")) +
    theme(text = element_text(family = "Encode Sans Condensed"),
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.line = element_line(color = "darkgrey"),
          panel.grid = element_line(color = "grey95"),
          plot.title.position = "plot",
          plot.title = element_text(face = "bold", 
                                    size = 16),
          plot.caption.position = "plot",
          plot.caption = element_markdown(color = "darkgrey", 
                                          hjust = 0),
          legend.position = "top") +
    labs(title = "Desempeño de la Economía en cada Periodo Presidencial",
         subtitle = "IGAE a valores constantes, base 2013",
         x = "Meses del Sexenio",
         y = "Índice: 0 = día 1 de cada sexenio",
         caption = "Fuente: INEGI, 
         Indicador Global de la Actividad Económica, 
         series desestacionalizadas. Último registro: agosto 2022<br>
         Visualización: Juan L. Bretón, PMP | @BretonPmp" )

ggsave("figures/plot01.jpg", plot = last_plot())