rm(list = ls())
library(sf)
library(tidyverse)
library(openxlsx)
sf_use_s2(FALSE)

name_padron <- "padron_general_lima"
data_sisfhog <- read_csv("processed/data_sisfog_lima.csv")

# --------------------------------------------------------
data_cofopri <- st_read("processed/lima_lotes_wgs84.gpkg")
titulares <- read_csv("processed/titulares.csv") %>% 
  mutate(
    estado_civil = case_when(
      estado_civil == "S" ~ "Soltero(a)",
      estado_civil == "C" ~ "Casado(a)",
      estado_civil == "V" ~ "Viudo(a)",
      .default = "Sin Dato")
  )

data.match.cofopri <- data_cofopri %>% 
  left_join(titulares, by = c("id_predio"="codigo_predio"))

rm(data_cofopri)
#----------------------------------------------------------

data_sisfhog.cofopri <- data.match.cofopri %>% 
  inner_join(data_sisfhog,by = c("nro_doc_identidad"="persona_nro_doc"))

zonificacion <- st_read("raw/microzonificacion/Otros/microzonificacion_sismica_lima.gpkg") %>% 
  rename(
    id_zona = reg_zona,
    fuente  = reg_fuente,
    elaborac = reg_anio) %>% 
  select(id_zona, fuente,elaborac)

rm(data.match.cofopri)
rm(data_sisfhog)
# ----------------------------------------------------------
layer_intersect_pre <- st_intersection(data_sisfhog.cofopri,zonificacion)
pueblo <- st_read("processed/pueblo_wgs84.gpkg") %>% select(nom_pueblo)
layer_intersect_post <- st_intersection(layer_intersect_pre, pueblo)
rm(layer_intersect_pre)
rm(pueblo)
# ----------------------------------------------------------
condiciones_validas <- list(
  "BELLAVISTA" = c("ZONA I", "ZONA II"),
  "CARMEN DE LA LEGUA REYNOSO" = c("ZONA I"),
  "CALLAO" = c("ZONA I", "ZONA II"),
  "LA PERLA" = c("ZONA II"),
  "VENTANILLA" = c("ZONA I", "ZONA II"),
  "CARABAYLLO" = c("ZONA I", "ZONA II"),
  "CHORRILLOS" = c("ZONA I", "ZONA II"),
  "CIENEGUILLA" = c("ZONA I", "ZONA II"),
  "EL AGUSTINO" = c("ZONA I", "ZONA II"),
  "LA VICTORIA" = c("ZONA I"),
  "PUEBLO LIBRE" = c("ZONA I"),
  "PUENTE PIEDRA" = c("ZONA I", "ZONA II", "ZONA III"),
  "SAN BARTOLO" = c("ZONA I", "ZONA II"),
  "SAN JUAN DE MIRAFLORES" = c("ZONA I", "ZONA II", "ZONA III"),
  "SAN MIGUEL" = c("ZONA I", "ZONA II"),
  "SANTA ANITA" = c("ZONA I", "ZONA II"),
  "SANTA ROSA" = c("ZONA I", "ZONA II"),
  "SANTIAGO DE SURCO" = c("ZONA I", "ZONA II", "ZONA III"),
  "SURQUILLO" = c("ZONA I"),
  "VILLA SALVADOR" = c("ZONA I", "ZONA II", "ZONA III"),
  "LA MOLINA" = c("ZONA I", "ZONA II", "ZONA III"),
  "LIMA" = c("ZONA I"),
  "COMAS" = c("ZONA I", "ZONA II", "ZONA III"),
  "SAN JUAN DE LURIGANCHO" = c("ZONA I", "ZONA II")
)


padron <- layer_intersect_post %>%
  group_by(id_predio) %>% 
  mutate(cant_prop = n()) %>% 
  filter(
    map2_lgl(hogar_distrito_0, id_zona, ~ {
      if (.x %in% names(condiciones_validas)) {
        .y %in% condiciones_validas[[.x]]
      } else {
        FALSE
      }
    })
  )

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
writexl::write_xlsx(padron_coords,paste0("output/",name_padron,".xlsx"))

spatial_geometry <- padron %>% 
  group_by(id_predio) %>% 
  distinct(id_predio,.keep_all = TRUE)

write_sf(spatial_geometry,paste0("output/spatial_",name_padron,".gpkg"))