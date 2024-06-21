###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################


## libraries
library(tidyverse)
library(styleBreton)
library(ggrepel)



## adquisición de datos
suppressMessages(source("source/data_clean_b18_00.R"))



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
  scale_color_manual(name = "Presidente",
                     values = c("#34495E", "#F39C12", "#16A085", 
                                "#8E44AD", "#9F0D08")) +
  theme_breton() +
  theme(legend.position = "top") +
  labs(title = "Indicador Anticipado del Desempeño de la Economía en cada Periodo Presidencial",
       subtitle = "IGAE.",
       x = "Meses del Sexenio",
       y = "Índice: 0 = Primer mes de cada sexenio",
       caption = "Fuente: INEGI, 
         Indicador Global de la Actividad Económica, 
         series desestacionalizadas. Valores constantes, base 2018.<br>
         Visualización: Juan L. Bretón, PMP | @juanlbreton" ) +
  scale_x_continuous(breaks = round(seq(min(df_global_02$mes_sexenio),
                                        max(df_global_02$mes_sexenio),
                                        length.out = 10),
                                    digits = 0))

# guarda visualizacion
ggsave("figures/plot02.jpg", plot = last_plot(), device = "jpeg", dpi = "retina")


# crecimiento de inicio a fin de sexenio 
inicio_lopez <- 
  df_global_02 |> 
  filter(presidente == "López") |> 
  head(n = 1) |> 
  pull(valor)


ultimo_lopez <- 
  df_global_02 |> 
  filter(presidente == "López") |> 
  tail(n = 1) |> 
  pull(valor)

(ultimo_lopez - inicio_lopez) / inicio_lopez
  
