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



# modelo lineal hasta la elección presidencial 2018
model_lineal <- 
  lm(valor ~ fecha, 
     data = df01 %>% 
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
  theme_breton() +
  labs(title = "Desempeño Histórico de la Economía Mexicana",
       subtitle = "Indicador Global de la Actividad Económica; la línea roja indica la tendencia de la Economía Mexicana hasta la última elección presidencial.",
       x = NULL,
       y = "Índice (Base 100 = 2018)",
       caption = "Fuente: INEGI, 
           Indicador Global de la Actividad Económica, 
           series desestacionalizadas. <br>
           Visualización: Juan L. Bretón, PMP | @juanlbreton") +
  scale_x_date(breaks  = seq(min(df01$fecha_final), 
                             max(df01$fecha_final), 
                             length.out = 6),
               date_labels = "%Y-%m")

ggsave("figures/plot01.jpg", plot = last_plot(), device = "jpeg", dpi = "retina")




df01 |> 
  filter(concepto == "Indicador Global de la Actividad Económica") |> 
  predict(model_lineal, newdata = _) |> 
  bind_cols(
    df01 |>
      filter(concepto == "Indicador Global de la Actividad Económica")) |> 
  slice_max(order_by = ...1) |> 
  select(...1, valor) |> 
  reframe(brecha = (valor - ...1) / valor)


