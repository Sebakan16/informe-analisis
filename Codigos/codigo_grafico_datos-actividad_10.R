library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)

datos = read_csv2("Tabla_conexiones_por_compañia_y_tipo.csv",na = "na", locale = locale(decimal_mark = ","))
attach(datos)

datos$Mes = factor(datos$Mes, levels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))

datos %>%
  filter(Año == 2020) %>%
  ggplot(aes(x = Mes, y = Movistar))+
  geom_col(fill = "turquoise3")+
  labs(title = "Conexiones a internet movil 2G en el año 2020",
       subtitle = "En Movistar",
       y = "cantidad",
       x = "meses")
