rm(list = ls())
library(tidyverse)
library(sf)

# 1. Reading spatial data (lotes) -----------------------------------------
lotes <- st_read("processed/lotes_wgs84.gpkg")
# 2. Filter by departaments -----------------------------------------------
lotes_ica <- lotes %>% filter(id_dpto == '11')
lotes_tacna <- lotes %>% filter(id_dpto == '23') 
lotes_lima <- lotes %>% filter(id_dpto %in% c('15','07'))

# 3. Export new geometries ------------------------------------------------
write_sf(lotes_ica, 'processed/lotes_wgs84_ica.gpkg')
write_sf(lotes_tacna,'processed/lotes_wgs84_tacna.gpkg')
write_sf(lotes_lima,'processed/lotes_wgs84_lima.gpkg')
