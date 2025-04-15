rm(list = ls())
library(sf)
library(tidyverse)
library(openxlsx)
sf_use_s2(FALSE)

name_padron <- "padron_general_ica"
data_sisfhog <- read_csv2("processed/data_sisfog_ica.csv") %>% 
  select(-fuente)

# --------------------------------------------------------
data_cofopri <- st_read("processed/lotes_wgs84_ica.gpkg")
titulares <- read_csv2("processed/titulares_v2.csv") %>% 
  mutate(
    estado_civil = case_when(
      estado_civil == "S" ~ "Soltero(a)",
      estado_civil == "C" ~ "Casado(a)",
      estado_civil == "V" ~ "Viudo(a)",
      .default = "Sin Dato")
  )

data.match.cofopri <- data_cofopri %>% 
  left_join(titulares, by = c("id_predio"="codigo_predio"),relationship = "many-to-many")

rm(data_cofopri)
#----------------------------------------------------------

data_sisfhog.cofopri <- data.match.cofopri %>% 
  inner_join(data_sisfhog,by = c("nro_doc_identidad"="persona_nro_doc"))

zonificacion <- st_read("raw/microzonificacion/Otros/microzonificacion_sismica_ica_departamento.gpkg") %>% 
  select(id_zona, fuente,elaborac)

rm(data.match.cofopri)
rm(data_sisfhog)
# ----------------------------------------------------------
layer_intersect <- st_join(x = data_sisfhog.cofopri,y = zonificacion,join = st_intersects, left = FALSE)
pueblo <- st_read("processed/pueblo_wgs84.gpkg") %>% select(nom_pueblo)
layer_intersect2 <- st_join(x = layer_intersect,y = pueblo,join = st_within)

# Filtrar registros SIN pueblo asignado (NA en columnas de 'pueblo')
sin_pueblo <- layer_intersect2 %>% filter(is.na(nom_pueblo))
con_pueblo <- layer_intersect2 %>% filter(!is.na(nom_pueblo))

indices_cercanos <- st_nearest_feature(sin_pueblo, pueblo)
sin_pueblo_con_cercano <- pueblo[indices_cercanos, ] %>% 
  st_drop_geometry()

sin_pueblo_edited <- sin_pueblo %>%
  mutate(nom_pueblo = sin_pueblo_con_cercano$nom_pueblo)

resultado_final <- bind_rows(con_pueblo, sin_pueblo_edited)

rm(indices_cercanos)
rm(sin_pueblo_con_cercano)
rm(sin_pueblo_edited)
rm(con_pueblo)
rm(sin_pueblo)
rm(layer_intersect)
# ----------------------------------------------------------
padron <- resultado_final %>%
  group_by(id_predio) %>% 
  mutate(cant_prop = n()) %>% 
  filter(id_zona %in% c("Zona I", "Zona II")) # Caso de Chincha

padron_coords <- padron %>% 
  st_centroid() %>% 
  mutate(
    lat = st_coordinates(geom)[,2],
    lon = st_coordinates(geom)[,1]
  ) %>% 
  rename(
    PREDIO = id_predio,
    DEPARTAMENTO = hogar_departamento,
    PROVINCIA = hogar_provincia,
    DISTRITO = hogar_distrito,
    PUEBLO = nom_pueblo,
    DNI = nro_doc_identidad,             
    `APELLIDO PATERNO` = apellido_paterno,
    `APELLIDO MATERNO` = apellido_materno,
    NOMBRES = nombres,
    `ESTADO CIVIL` = estado_civil,
    SEXO = persona_sexo,
    SISFOH = hogar_cse_niv_pobreza,
    `FECHA DE VIGENCIA` = hogar_cse_vigencia_fin,
    `CANT PROPIETARIOS` = cant_prop,  
    ZONIFICACION = id_zona,
    coord_x = lon,
    coord_y = lat,
    LT = nro_lote,
    MZ = nro_mzna
  ) %>% 
  select(
    PREDIO,
    DEPARTAMENTO,
    PROVINCIA,
    DISTRITO,
    PUEBLO,
    MZ,
    LT,
    DNI,
    `APELLIDO PATERNO`,
    `APELLIDO MATERNO`,
    NOMBRES,
    `ESTADO CIVIL`,
    SEXO,
    SISFOH,
    `FECHA DE VIGENCIA`,
    `CANT PROPIETARIOS`,
    ZONIFICACION,
    coord_x,
    coord_y
  ) %>% 
  st_drop_geometry()

rm(zonificacion)
rm(layer_intersect2)
rm(titulares)
rm(data_sisfhog.cofopri)
# -----------------------------------------------------------------------
if(!dir.exists("output")){dir.create("output")}
write.xlsx(padron_coords,paste0("output/",name_padron,".xlsx"))

spatial_geometry <- padron %>% 
  group_by(id_predio) %>% 
  distinct(id_predio,.keep_all = TRUE)

write_sf(spatial_geometry,paste0("output/spatial_",name_padron,".gpkg"))