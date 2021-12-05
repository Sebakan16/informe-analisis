library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(readxl)
library(janitor)
library(stringr)
library(ggtext)

### !GG
### Crtl + Shift + C  background


# cargar datos más fáciles ------------------------------------------------

columnas_1 = read_excel(file.choose(), sheet = 3,range = "B7:AK144", .name_repair = "minimal", n_max = 0) %>% names()


nombre_columnas_sin_repetir = columnas_1 %>% 
  str_subset(".") 

contador = 1:length(nombre_columnas_sin_repetir)

nombre_columnas = c()

for (i in contador) {
  
  if (nombre_columnas_sin_repetir[i] == "Fecha"){
    for (j in 1:2) {
      nombre_columnas = c(nombre_columnas, nombre_columnas_sin_repetir[i])
    }
  }
  if (nombre_columnas_sin_repetir[i] == "Conexiones con Tecnología 2G"){
    for (j in 1:8) {
      nombre_columnas = c(nombre_columnas, nombre_columnas_sin_repetir[i])
    }
  }
  if (nombre_columnas_sin_repetir[i] == "Conexiones con Tecnología 3G"){
    for (j in 1:14) {
      nombre_columnas = c(nombre_columnas, nombre_columnas_sin_repetir[i])
    }
  }
  if (nombre_columnas_sin_repetir[i] == "Conexiones con Tecnología 4G"){
    for (j in 1:11) {
      nombre_columnas = c(nombre_columnas, nombre_columnas_sin_repetir[i])
    }
  }
}
nombre_columnas = c(nombre_columnas, "Total")

nombre_columnas_2 = read_excel(file.choose(), sheet = 3,range = "B8:AK144", .name_repair = "minimal", n_max = 0) %>% names()

nombres_columnas = paste(nombre_columnas, "-", nombre_columnas_2)

datos_3 = read_excel(file.choose(), sheet = 3, range = "B9:AK144", col_names = nombres_columnas)


# Separando datos --------------------------------------------------------

# 2G
datos_3_2G = select(datos_3, 1, 2, starts_with("Conexiones Con Tecnología 2G"))

datos_2G_piv = pivot_longer(datos_3_2G, cols = 3:10, names_to = "Compañia", values_to = "Conexiones")

datos_2G_piv$`Fecha - Mes` = factor(datos_2G_piv$`Fecha - Mes`, levels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))

#3G
datos_3G = select(datos_3, 1, 2, starts_with("Conexiones Con Tecnología 3G"))

datos_3G_piv = pivot_longer(datos_3G, cols = 3:16, names_to = "Compañia", values_to = "Conexiones")

datos_3G_piv$`Fecha - Mes` = factor(datos_3G_piv$`Fecha - Mes`, levels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))

#4G
datos_4G = select(datos_3, 1, 2, starts_with("Conexiones Con Tecnología 4G"))

datos_4G_piv = pivot_longer(datos_4G, cols = 3:13, names_to = "Compañia", values_to = "Conexiones")

datos_4G_piv$`Fecha - Mes` = factor(datos_4G_piv$`Fecha - Mes`, levels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))

# Graficos ---------------------------------------------------------------

# 2G
datos_2G_piv %>% 
  filter(`Fecha - Año`==2020) %>% 
  filter(Compañia %in% c("Conexiones con Tecnología 2G - Movistar", "Conexiones con Tecnología 2G - Claro", "Conexiones con Tecnología 2G - Entel PCS")) %>% 
  ggplot(aes(`Fecha - Mes`, Conexiones, fill = Compañia))+
  geom_col(position = "dodge", show.legend = FALSE)+
  theme_minimal()+
  scale_fill_manual(values = c("#c80815", "#6aa84f", "#0a39bc"))+
  labs(x = "Meses",
       y = "Total de conexiones",
       title = "Conexiones 2G de <b style='color:#c80815'>Claro</b>, <b style='color:#6aa84f'>Entel</b> y <b style='color:#0a39bc'>Movistar</b>",
       subtitle = "Inicios de pandemia (año 2020)",
       fill = "Compañia")+
  theme(plot.title = element_markdown())

# 3G
datos_3G_piv %>% 
  filter(`Fecha - Año`==2020) %>% 
  filter(Compañia %in% c("Conexiones con Tecnología 3G - Movistar", "Conexiones con Tecnología 3G - Claro", "Conexiones con Tecnología 3G - Entel PCS")) %>% 
  ggplot(aes(`Fecha - Mes`, Conexiones, fill = Compañia))+
  geom_col(position = "dodge", show.legend = FALSE)+
  theme_minimal()+
  scale_fill_manual(values = c("#c80815", "#6aa84f", "#0a39bc"))+
  labs(x = "Meses",
       y = "Total de conexiones",
       title = "Conexiones 3G de <b style='color:#c80815'>Claro</b>, <b style='color:#6aa84f'>Entel</b> y <b style='color:#0a39bc'>Movistar</b>",
       subtitle = "Inicios de pandemia (año 2020)",
       fill = "Compañia")+
  theme(plot.title = element_markdown())

# 4G
datos_4G_piv %>% 
  filter(`Fecha - Año`==2020) %>% 
  filter(Compañia %in% c("Conexiones con Tecnología 4G - Movistar", "Conexiones con Tecnología 4G - Claro", "Conexiones con Tecnología 4G - Entel PCS")) %>% 
  ggplot(aes(`Fecha - Mes`, Conexiones, fill = Compañia))+
  geom_col(position = "dodge", show.legend = FALSE)+
  theme_minimal()+
  scale_fill_manual(values = c("#c80815", "#6aa84f", "#0a39bc"))+
  labs(x = "Meses",
       y = "Total de conexiones",
       title = "Conexiones 4G de <b style='color:#c80815'>Claro</b>, <b style='color:#6aa84f'>Entel</b> y <b style='color:#0a39bc'>Movistar</b>",
       subtitle = "Inicios de pandemia (año 2020)",
       fill = "Compañia")+
  theme(plot.title = element_markdown())+
  scale_y_continuous(breaks = c(0, 1625000, 3250000, 4875000,6500000),
                     limits = c(0, 6500000))



