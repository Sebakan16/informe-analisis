library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(readxl)

#read_excel()
datos = read_excel(file.choose(), sheet = 3, col_names = TRUE, skip = 7)

datos$Mes...2 = factor(datos$Mes...2, levels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))

View(datos)
# no lo puedo pasar a grafico de lineas  D:
datos %>%
  filter(Año...1 == 2020) %>%
  ggplot(aes(x = Mes...2, y = Movistar...3))+
  #geom_line()+
  geom_col(fill = "firebrick4")+
  labs(title = "Conexiones a internet movil 2G en el año 2020",
       subtitle = "En Movistar",
       y = "Conexiones",
       x = "Meses")

datos %>%
  filter(Año...1 == 2020) %>%
  ggplot(aes(x = Mes...2, y = Movistar...11))+
  geom_col(fill = "royalblue4")+
  labs(title = "Conexiones a internet movil 3G en el año 2020",
       subtitle = "En Movistar",
       y = "Conexiones",
       x = "Meses")+
  scale_y_continuous(breaks = seq(0,700000, by = 175000))

datos %>%
  filter(Año...1 == 2020) %>%
  ggplot(aes(x = Mes...2, y = Movistar...25))+
  geom_col(fill = "chartreuse4")+
  labs(title = "Conexiones a internet movil 4G en el año 2020",
       subtitle = "En Movistar",
       y = "Conexiones",
       x = "Meses")+
  scale_y_continuous(breaks = seq(0, 4000000, by = 1000000))

View(datos)

opciones = c("Cajon", "paseo")
respuesta_sacamos_a = sample(opciones, 1)



install.packages("gt")
install.packages("gtsummary")
remotes::install_github("jthomasmock/gtExtras")
install.packages("broom")
install.packages("palmerpenguins")
