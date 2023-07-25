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
  slice_max(order_by = fecha_final, n =1)

# modelo lineal
model_lineal <- 
  lm(valor ~ fecha, data = df01 %>% 
       filter(fecha < "2018-07-02") %>% 
       filter(concepto == "Indicador Global de la Actividad Económica"))


## visualización de la serie
df01 %>% 
  filter(concepto == "Indicador Global de la Actividad Económica") %>% 
  mutate(predi = predict(model_lineal, newdata = .)) %>% 
  ggplot(aes(x = fecha_final, y = valor)) +
    geom_line(alpha = 0.7,
              color = "steelblue",
              linewidth = 0.6) +
    geom_smooth(aes(y = predi), 
                se = FALSE, 
                color = "darkred", 
                linewidth = 0.5) +
    geom_vline(xintercept = as_date("2018-07-02"),
               color = "darkgrey") +
    annotate(geom = "text",
             label = "Elección presidencial",
             x = as_date("2018-12-31"),
             y = 75, 
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
    labs(title = "Desempeño Histórico de la Economía Mexicana",
         subtitle = "Indicador Global de la Actividad Económica; la línea roja indica la tendencia de la Economía Mexicana hasta la última elección presidencial.",
         x = NULL,
         y = "Índice (Base 100 = 2013)",
         caption = "Fuente: INEGI, 
           Indicador Global de la Actividad Económica, 
           series desestacionalizadas. <br>
           Visualización: Juan L. Bretón, PMP | @juanlbreton") +
    scale_x_date(breaks  = seq(min(df01$fecha_final), 
                               max(df01$fecha_final), 
                               length.out = 6),
                 date_labels = "%Y-%m")

ggsave("figures/plot01.jpg", plot = last_plot(), device = "jpeg", dpi = "retina")


# función para poner numero de índice
pon_num <- function(base) {
  seq_along(along.with = base$num_mes)
}

# función para calcular valor escalado
calc_escala <- function(base) {
  fac <- base %>% 
    slice_min(order_by = fecha_final) %>% 
    pull(valor)
  
  (base$valor - fac) / fac
}


# integrar presidente con base en fecha
df_global_01 <- df01 %>% 
  filter(concepto == "Indicador Global de la Actividad Económica") %>%
  mutate(presidente = as_factor(case_when(
    fecha_final >= as_date("1994-12-01") & fecha_final < as_date("2000-12-01") ~ "Zedillo",
    fecha_final >= as_date("2000-12-01") & fecha_final < as_date("2006-12-01") ~ "Fox",
    fecha_final >= as_date("2006-12-01") & fecha_final < as_date("2012-12-01") ~ "Calderón",
    fecha_final >= as_date("2012-12-01") & fecha_final < as_date("2018-12-01") ~ "Peña",
    fecha_final >= as_date("2018-12-01") ~ "López",
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
  labs(title = "Indicador Anticipado del Desempeño de la Economía en cada Periodo Presidencial",
       subtitle = "IGAE a valores constantes, base 2013",
       x = "Meses del Sexenio",
       y = "Índice: 0 = Primer mes de cada sexenio",
       caption = "Fuente: INEGI, 
         Indicador Global de la Actividad Económica, 
         series desestacionalizadas.<br>
         Visualización: Juan L. Bretón, PMP | @juanlbreton" ) +
  scale_x_continuous(breaks = round(seq(min(df_global_02$mes_sexenio),
                                        max(df_global_02$mes_sexenio),
                                        length.out = 10),
                                    digits = 0))

ggsave("figures/plot02.jpg", plot = last_plot(), device = "jpeg", dpi = "retina")


# relación con el máximo histórico de la serie
df01 %>% 
  filter(concepto == "Indicador Global de la Actividad Económica") %>% 
  summarize(cambio_respecto_al_max = (tail(valor, 1) - max(valor)) / max(valor)) %>% 
  knitr::kable(digits = 6, 
               col.names = "Cambio respecto al máximo histórico")



# cambio a 1 año
año_anterior <- df01 %>% 
  filter(concepto == "Indicador Global de la Actividad Económica") %>% 
  filter(fecha == max(fecha) - years(1)) %>% 
  pull(valor)

año_actual <- df01 %>% 
  filter(concepto == "Indicador Global de la Actividad Económica") %>% 
  filter(fecha == max(fecha)) %>% 
  pull(valor)

as_tibble((año_actual - año_anterior) / año_anterior) %>% 
  knitr::kable(digits = 6,
               col.names = "Cambio respecto al año anterior")


# cambio a 1 mes
mes_anterior <- df01 %>% 
  filter(concepto == "Indicador Global de la Actividad Económica") %>% 
  filter(fecha == max(fecha) - months(1)) %>% 
  pull(valor)

mes_actual <- df01 %>% 
  filter(concepto == "Indicador Global de la Actividad Económica") %>% 
  filter(fecha == max(fecha)) %>% 
  pull(valor)

as_tibble((mes_actual - mes_anterior) / mes_anterior) %>% 
  knitr::kable(digits = 6,
               col.names = "Cambio respecto al mes anterior")


# cambio hasta antes de la pandemia de COVID
año_anterior <- df01 %>% 
  filter(concepto == "Indicador Global de la Actividad Económica") %>% 
  filter(fecha == as_date("2020-02-01")) %>% 
  pull(valor)

año_actual <- df01 %>% 
  filter(concepto == "Indicador Global de la Actividad Económica") %>% 
  filter(fecha == max(fecha)) %>% 
  pull(valor)

as_tibble((año_actual - año_anterior) / año_anterior) %>% 
  knitr::kable(digits = 6,
               col.names = "Cambio respecto a antes de COVID")


