rm(list = ls())
library(tidyverse)
library(janitor)
library(sf)

# 1. Microzonificacion ----------------------------------------------------
variables <- c(
  "objectid",
  "departamen",
  "provincia",
  "distrito",
  "id_zona",
  "desc_zona",
  "elaborac",
  "fuente")
pisco <- st_read("raw/microzonificacion/Otros/microzonificacion_sismica_pisco.gpkg") %>% 
  select(all_of(variables)) %>% 
  st_make_valid()

ica <- st_read("raw/microzonificacion/Otros/microzonificacion_sismica_chincha.gpkg") %>% 
  select(all_of(variables)) %>% 
  st_make_valid()

ica_total <- rbind(pisco,ica) %>% 
  st_make_valid()

write_sf(ica_total ,'raw/microzonificacion/Otros/microzonificacion_sismica_ica_departamento.gpkg')