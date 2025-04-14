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
    )

write.xlsx(lima_actualizado,'output/padron_general_lima.csv')

lima_benificarios_sf <- st_read("output/spatial_padron_general_lima.gpkg") %>% 
  filter(!id_predio %in% filtro$predio)

write_sf(lima_benificarios_sf,'output/spatial_padron_general_lima.gpkg')
