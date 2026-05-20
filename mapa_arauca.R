library(chilemapas) # mapas de chile
library(dplyr) # manipulación de datos
library(ggplot2) # visualización de datos
library(scales) # utilidad para visualización de datos
library(sf) # manipulación de datos geográficos
library(plyr)
library(rvest)
library(janitor)
library(stringr)
library(readr)

#Codigos de comuna
terr <- read_rds("~/R/electoral/data/terr_elec.RData", refhook = NULL) %>% 
  filter(Circ_Senatorial == 11) %>% 
  select(Distrito,Comuna,Cod_Comuna)

#Mapas
mapa_comunas <- chilemapas::mapa_comunas
mapa_zona <- chilemapas::mapa_zonas
mapa_zona_09 <- mapa_zona|> 
  filter(codigo_region == "09")
#mapa_regiones <- mapa_comunas %>% group_by(codigo_region) %>% summarize(geometry = st_union(geometry)) # resumir los datos agrupados uniéndolos
mapa_regiones <- ddply(mapa_comunas, .(codigo_region), summarize, geometry = st_union(geometry)) # resumir los datos agrupados uniéndolos

#url <- "https://es.wikipedia.org/wiki/Anexo:Comunas_de_Chile"

# obtener tabla con datos de comunas con web scraping
#tabla <- session(url) |> 
#  read_html() |> 
#  html_table(convert = FALSE)

#datos_comunas <- tabla[[1]] |> 
#  clean_names() |> 
  # seleccionar y renombrar columnas
#  select(codigo_comuna = cut_codigo_unico_territorial,
#         nombre, region, superficie_km2,
#         poblacion = poblacion2020) |> 
  # eliminar espacios de la columna de población
# mutate(poblacion = str_remove_all(poblacion, " "),
#         poblacion = as.numeric(poblacion)) |> 
  # eliminar los separadores de miles
#  mutate(superficie_km2 = str_remove_all(superficie_km2, "\\."),
         # convertir comas a puntos
#         superficie_km2 = str_replace(superficie_km2, ",", "."),
#         superficie_km2 = as.numeric(superficie_km2))

#datos para mapa comunal
paso_data <- read_rds("~/R/electoral/data/dip_2021.RData", refhook = NULL)
#paso_data <- read_rds("~/R/electoral/data/Diputados_2021.RData", refhook = NULL)
paso_data <- paso_data %>% filter(Pacto == "APRUEBO DIGNIDAD" & Distrito == 22)
paso_data <- paso_data %>% left_join(terr, by = join_by(Comuna))
names(paso_data)[names(paso_data) == 'Cod_Comuna'] <- 'codigo_comuna'
paso_data$codigo_comuna <- as.character(paso_data$codigo_comuna)
#paso_data$Part <- signif((paso_data$Voto / paso_data$Insc)*100,4)
paso_data <- paso_data %>% select(codigo_comuna,Porcentaje)

mapa_comunas <- mapa_comunas |> 
  filter(codigo_region == "09")

mapa_comunas_2 <- mapa_comunas |> 
  # adjuntar datos al mapa, coincidiendo por columna de código de comunas
  left_join(paso_data,
            by = join_by(codigo_comuna)) |> 
  relocate(geometry, .after = 0) # tirar geometría al final
write_csv(mapa_comunas_2,"~/R/electoral/data/resultado.csv")

mapa_comunas_2 <- st_as_sf(mapa_comunas_2)
#mapa_comunas_2$poblacion <- as.numeric(mapa_comunas_2$poblacion)

mapa_comunas_2 %>%
  ggplot() +
  aes(fill = Porcentaje) +
  geom_sf(linewidth = 0.12, color = "white") +
  geom_sf_text(aes(label = scales::comma(Porcentaje, big.mark = ".")), 
               size = 2, color = "white", check_overlap = TRUE) +
  theme_classic() +
  scale_fill_distiller(type = "seq", palette = "Blues",
                       labels = scales::label_comma(big.mark = ".")) + 
  theme(legend.key.width = unit(3, "mm")) +
  theme(axis.title = element_blank())

