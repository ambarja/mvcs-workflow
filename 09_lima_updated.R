rm(list = ls())
library(sf)
library(tidyverse)
library(openxlsx)
library(janitor)
library(writexl)

filtro <- read_csv2('output/lista_ya_ejecutada.csv')
lima_benificarios <- read.xlsx('output/padron_general_lima.xlsx') %>% 
  clean_names()

lima_actualizado <- lima_benificarios %>% 
  filter(!predio %in% filtro$predio) %>% 
  mutate(
    pueblo = gsub('xml:space="preserve">',"", pueblo),
    apellido_materno = gsub("¥","Ñ",apellido_materno)
    ) %>% 
  rename(
    PREDIO = predio,
    DEPARTAMENTO = departamento,
    PROVINCIA = provincia,
    DISTRITO = distrito,
    PUEBLO = pueblo,
    DNI = dni,             
    `APELLIDO PATERNO` = apellido_paterno,
    `APELLIDO MATERNO` = apellido_materno,
    NOMBRES = nombres,
    `ESTADO CIVIL` = estado_civil,
    SEXO = sexo,
    SISFOH = sisfoh,
    `FECHA DE VIGENCIA` = fecha_de_vigencia,
    `CANT PROPIETARIOS` = cant_propietarios,  
    ZONIFICACION = zonificacion,
    coord_x = coord_x,
    coord_y = coord_y,
    LT = lt,
    MZ = mz
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

write.xlsx(lima_actualizado,'output/padron_general_lima.csv')

lima_benificarios_sf <- st_read("output/spatial_padron_general_lima.gpkg") %>% 
  filter(!id_predio %in% filtro$predio)

write_sf(lima_benificarios_sf,'output/spatial_padron_general_lima.gpkg')