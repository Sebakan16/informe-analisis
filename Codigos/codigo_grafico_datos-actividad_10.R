library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(readxl)

#read_excel()
datos = read_excel(file.choose(), sheet = 3, col_names = TRUE, skip = 7)

datos$Mes...2 = factor(datos$Mes...2, levels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))

datos %>%
  filter(Año...1 == 2020) %>%
  ggplot(aes(x = Mes...2, y = Movistar...3))+
  geom_col(fill = "firebrick4")+
  labs(title = "Conexiones a internet movil 2G en el año 2020",
       subtitle = "En Movistar",
       y = "Conexiones",
       x = "Meses")







