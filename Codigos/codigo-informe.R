library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(readxl)
library(janitor)
library(stringr)
library(ggtext)
library(hrbrthemes)
library(magick)

### !GG
### Crtl + Shift + C  background

# cargar datos más fáciles ------------------------------------------------

logo_movistar = image_read("Figuras/Logo_movistar.png")
logo_entel = image_read("Figuras/Logo_entel.png")
logo_claro = image_read("Figuras/Logo_claro.png")

{
direc_datos = "Datos/Series_de_conexiones.xlsx"

columnas_1 = read_excel(direc_datos, sheet = 3,range = "B7:AK144", .name_repair = "minimal", n_max = 0) %>% names()


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

nombre_columnas_2 = read_excel(direc_datos, sheet = 3,range = "B8:AK144", .name_repair = "minimal", n_max = 0) %>% names()

nombres_columnas = paste(nombre_columnas, "-", nombre_columnas_2)

datos_3 = read_excel(direc_datos, sheet = 3, range = "B9:AK144", col_names = nombres_columnas)


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

# Datos Totales
datos_totales = select(datos_3, 1, 2, starts_with("Total"))

datos_totales$`Fecha - Mes`= factor(datos_totales$`Fecha - Mes`, levels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))

# Datos por empresa
datos_empresas = select(datos_3, 1, 2, starts_with("Conexiones Con Tecnología"))
datos_emp = datos_empresas %>% 
  mutate(`General Movistar` = datos_empresas$`Conexiones con Tecnología 2G - Movistar` + datos_empresas$`Conexiones con Tecnología 3G - Movistar` + datos_empresas$`Conexiones con Tecnología 4G - Movistar`) %>% 
  mutate(`General Entel` = datos_empresas$`Conexiones con Tecnología 2G - Entel PCS` + datos_empresas$`Conexiones con Tecnología 3G - Entel PCS` + datos_empresas$`Conexiones con Tecnología 4G - Entel PCS`) %>% 
  mutate(`General Claro` = datos_empresas$`Conexiones con Tecnología 2G - Claro` + datos_empresas$`Conexiones con Tecnología 3G - Claro` + datos_empresas$`Conexiones con Tecnología 4G - Claro`) 

datos_e = select(datos_emp, 1, 2, starts_with("General"))

datos_em = pivot_longer(datos_e, cols = 3:5, names_to = "Compañia", values_to = "Conexiones")
datos_em$`Fecha - Mes`= factor(datos_em$`Fecha - Mes`, levels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
}
# Graficos ---------------------------------------------------------------

# Total

datos_totales %>% 
  filter(`Fecha - Año` == c(2019))%>% 
  ggplot(aes(x = `Fecha - Mes`, y = `Total - Total de Conexiones Móviles`, group = `Fecha - Año`))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  labs(x = "Meses",
       y = "Total de Conexiones",
       title = "Total de conexiones en el año 2019")

datos_totales %>% 
  filter(`Fecha - Año` == c(2020))%>% 
  ggplot(aes(x = `Fecha - Mes`, y = `Total - Total de Conexiones Móviles`, group = `Fecha - Año`))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  labs(x = "Meses",
       y = "Total de Conexiones",
       title = "Total de conexiones en el año 2020")
  #geom_text(aes(label = `Total - Total de Conexiones Móviles`), vjust = -1, hjust = 0.5)


# 2G
datos_2G_piv %>% 
  filter(`Fecha - Año`==2020) %>% 
  filter(Compañia %in% c("Conexiones con Tecnología 2G - Movistar", "Conexiones con Tecnología 2G - Claro", "Conexiones con Tecnología 2G - Entel PCS")) %>% 
  ggplot(aes(`Fecha - Mes`, Conexiones, group = Compañia, color = Compañia))+
  geom_line(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  scale_colour_manual(values = c("#FF0033", "#002EFF", "#019DF4"))+
  labs(x = "Meses",
       y = "Total de conexiones",
       title = "Conexiones 2G de <b style='color:#FF0033'>Claro</b>, <b style='color:#002EFF'>Entel</b> y <b style='color:#019DF4'>Movistar</b>",
       subtitle = "Inicios de pandemia (año 2020)",
       fill = "Compañia")+
  theme(plot.title = element_markdown())

grid::grid.raster(logo_movistar, x = 0.98, y = 0.27, just = c("right", "top"), width = unit(0.65, "inches"))
grid::grid.raster(logo_entel, x = 0.98, y = 0.5, just = c("right", "top"), width = unit(0.65, "inches"))
grid::grid.raster(logo_claro, x = 0.98, y = 0.22, just = c("right", "top"), width = unit(0.65, "inches"))

# 3G

datos_3G_piv %>% 
  filter(`Fecha - Año`==2020) %>% 
  filter(Compañia %in% c("Conexiones con Tecnología 3G - Movistar", "Conexiones con Tecnología 3G - Claro", "Conexiones con Tecnología 3G - Entel PCS")) %>% 
  ggplot(aes(`Fecha - Mes`, Conexiones, group = Compañia, color = Compañia))+
  geom_line(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  scale_colour_manual(values = c("#FF0033", "#002EFF", "#019DF4"))+
  labs(x = "Meses",
       y = "Total de conexiones",
       title = "Conexiones 3G de <b style='color:#FF0033'>Claro</b>, <b style='color:#002EFF'>Entel</b> y <b style='color:#019DF4'>Movistar</b>",
       subtitle = "Inicios de pandemia (año 2020)",
       fill = "Compañia")+
  theme(plot.title = element_markdown())+
  scale_y_continuous(breaks = c(300000, 440000, 580000, 720000, 860000))

grid::grid.raster(logo_movistar, x = 1, y = 0.41, just = c("right", "top"), width = unit(0.65, "inches"))
grid::grid.raster(logo_entel, x = 1, y = 0.75, just = c("right", "top"), width = unit(0.65, "inches"))
grid::grid.raster(logo_claro, x = 1, y = 0.215, just = c("right", "top"), width = unit(0.65, "inches"))


# 4G

datos_4G_piv %>% 
  filter(`Fecha - Año`==2020) %>% 
  filter(Compañia %in% c("Conexiones con Tecnología 4G - Movistar", "Conexiones con Tecnología 4G - Claro", "Conexiones con Tecnología 4G - Entel PCS")) %>% 
  ggplot(aes(`Fecha - Mes`, Conexiones, group = Compañia, color = Compañia))+
  geom_line(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  scale_colour_manual(values = c("#FF0033", "#002EFF", "#019DF4"))+
  labs(x = "Meses",
       y = "Total de conexiones",
       title = "Conexiones 4G de <b style='color:#FF0033'>Claro</b>, <b style='color:#002EFF'>Entel</b> y <b style='color:#019DF4'>Movistar</b>",
       subtitle = "Inicios de pandemia (año 2020)",
       fill = "Compañia")+
  theme(plot.title = element_markdown())+
  scale_y_continuous(breaks = c(300000, 440000, 580000, 720000, 860000))

grid::grid.raster(logo_movistar, x = 1, y = 0.38, just = c("right", "top"), width = unit(0.65, "inches"))
grid::grid.raster(logo_entel, x = 1, y = 0.85, just = c("right", "top"), width = unit(0.65, "inches"))
grid::grid.raster(logo_claro, x = 1, y = 0.20, just = c("right", "top"), width = unit(0.65, "inches"))

# Generales por empresa

datos_em %>% 
  filter(`Fecha - Año`==2020) %>% 
  ggplot(aes(`Fecha - Mes`, Conexiones, group = Compañia, color = Compañia))+
  geom_line(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  scale_colour_manual(values = c("#FF0033", "#002EFF", "#019DF4"))+
  labs(x = "Meses",
       y = "Total de conexiones",
       title = "Conexiones totales de <b style='color:#FF0033'>Claro</b>, <b style='color:#002EFF'>Entel</b> y <b style='color:#019DF4'>Movistar</b>",
       subtitle = "Inicios de pandemia (año 2020)",
       fill = "Compañia")+
  theme(plot.title = element_markdown())+
  scale_y_continuous(breaks = c(3300000, 4300000, 5300000, 6300000, 7300000))

grid::grid.raster(logo_movistar, x = 1, y = 0.41, just = c("right", "top"), width = unit(0.65, "inches"))
grid::grid.raster(logo_entel, x = 1, y = 0.81, just = c("right", "top"), width = unit(0.65, "inches"))
grid::grid.raster(logo_claro, x = 1, y = 0.24, just = c("right", "top"), width = unit(0.65, "inches"))





