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


## preparación de secuencia
secuen <- function(base){
  seq_along(along.with = base$valor)
}


# implementación de secuencia
df02 <- 
  df01 |> 
  mutate(id_mes = str_c(mes, 
                        str_extract(year(fecha_final), "\\d{2}$"), 
                        sep = "_")) |> 
  nest(.by = concepto) |> 
  mutate(sec = map(data, secuen)) |> 
  unnest(cols = everything())


# visualización de 4 series
df02 |> 
  ggplot() +
  geom_line(data = df02 |> 
              filter(concepto == "Actividades primarias"),
            aes(x = fecha_final, 
                y = valor, 
                group = concepto,
                color = "Act. Primarias"),
            alpha = 0.35) +
  geom_line(data = df02 |> 
              filter(concepto == "Actividades secundarias"),
            aes(x = fecha_final, 
                y = valor, 
                group = concepto,
                color = "Act. Secundarias"),
            alpha = 0.35) +
  geom_line(data = df02 |> 
              filter(concepto == "Actividades terciarias"),
            aes(x = fecha_final, 
                y = valor, 
                group = concepto,
                color = "Act. Terciarias"),
            alpha = 0.35) +
  geom_line(data = df02 |> 
              filter(concepto == "Indicador Global de la Actividad Económica"),
            aes(x = fecha_final, 
                y = valor, 
                group = concepto,
                color = "Global"),
            alpha = 0.85,
            linewidth = 0.85) +
  scale_color_manual(name = "Indicador",
                     breaks = c("Global", "Act. Primarias",
                                "Act. Secundarias", "Act. Terciarias"),
                     values = c("#B22203", "#06B304", 
                                "#00238D", "#9003C2")) +
  labs(title = "Desemepeño Histórico del IGAE y sus Componentes",
       subtitle = "Componentes del IGAE",
       y = "Índice a valores constantes (base 2018)",
       x = "Mes",
       caption = "Fuente: INEGI: 
         Indicador Global de la Actividad Económica, base 2018, 
         series desestacionalizadas.<br>
         Visualización: Juan L. Bretón, PMP | @juanlbreton") +
  theme_breton() +
  theme(legend.position = "top",
        axis.text.x = element_text(size = rel(0.85)),
        panel.grid.major = element_line(color = "grey97"),
        panel.grid.minor = element_blank()) +
  scale_x_date(breaks = seq(min(df02$fecha_final),
                            max(df02$fecha_final),
                            length.out = 10),
               date_labels = "%Y_%m")

# guarda imagen
ggsave(filename = "figures/compo_01.jpg", device = "jpeg", dpi = "retina")


## cálculo de tasas de cambio GLOBAL
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
               col.names = "IGAE Cambio respecto a 12 meses anteriores")


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
               col.names = "IGAE Cambio respecto al mes anterior")


## cálculo de tasas de cambio PRIMARIAS
# cambio a 1 año
año_anterior <- df01 %>% 
  filter(concepto == "Actividades primarias") %>% 
  filter(fecha == max(fecha) - years(1)) %>% 
  pull(valor)

año_actual <- df01 %>% 
  filter(concepto == "Actividades primarias") %>% 
  filter(fecha == max(fecha)) %>% 
  pull(valor)

as_tibble((año_actual - año_anterior) / año_anterior) %>% 
  knitr::kable(digits = 6,
               col.names = "PRIMARIAS Cambio respecto a 12 meses anteriores")


# cambio a 1 mes
mes_anterior <- df01 %>% 
  filter(concepto == "Actividades primarias") %>% 
  filter(fecha == max(fecha) - months(1)) %>% 
  pull(valor)

mes_actual <- df01 %>% 
  filter(concepto == "Actividades primarias") %>% 
  filter(fecha == max(fecha)) %>% 
  pull(valor)

as_tibble((mes_actual - mes_anterior) / mes_anterior) %>% 
  knitr::kable(digits = 6,
               col.names = "PRIMARIAS Cambio respecto al mes anterior")


## cálculo de tasas de cambio SECUNDARIAS
# cambio a 1 año
año_anterior <- df01 %>% 
  filter(concepto == "Actividades secundarias") %>% 
  filter(fecha == max(fecha) - years(1)) %>% 
  pull(valor)

año_actual <- df01 %>% 
  filter(concepto == "Actividades secundarias") %>% 
  filter(fecha == max(fecha)) %>% 
  pull(valor)

as_tibble((año_actual - año_anterior) / año_anterior) %>% 
  knitr::kable(digits = 6,
               col.names = "SECUNDARIAS Cambio respecto a 12 meses anteriores")


# cambio a 1 mes
mes_anterior <- df01 %>% 
  filter(concepto == "Actividades secundarias") %>% 
  filter(fecha == max(fecha) - months(1)) %>% 
  pull(valor)

mes_actual <- df01 %>% 
  filter(concepto == "Actividades secundarias") %>% 
  filter(fecha == max(fecha)) %>% 
  pull(valor)

as_tibble((mes_actual - mes_anterior) / mes_anterior) %>% 
  knitr::kable(digits = 6,
               col.names = "SECUNDARIAS Cambio respecto al mes anterior")


## cálculo de tasas de cambio TERCIARIAS
# cambio a 1 año
año_anterior <- df01 %>% 
  filter(concepto == "Actividades terciarias") %>% 
  filter(fecha == max(fecha) - years(1)) %>% 
  pull(valor)

año_actual <- df01 %>% 
  filter(concepto == "Actividades terciarias") %>% 
  filter(fecha == max(fecha)) %>% 
  pull(valor)

as_tibble((año_actual - año_anterior) / año_anterior) %>% 
  knitr::kable(digits = 6,
               col.names = "TERCIARIAS Cambio respecto a 12 meses anteriores")


# cambio a 1 mes
mes_anterior <- df01 %>% 
  filter(concepto == "Actividades terciarias") %>% 
  filter(fecha == max(fecha) - months(1)) %>% 
  pull(valor)

mes_actual <- df01 %>% 
  filter(concepto == "Actividades terciarias") %>% 
  filter(fecha == max(fecha)) %>% 
  pull(valor)

as_tibble((mes_actual - mes_anterior) / mes_anterior) %>% 
  knitr::kable(digits = 6,
               col.names = "TERCIARIAS Cambio respecto al mes anterior")
