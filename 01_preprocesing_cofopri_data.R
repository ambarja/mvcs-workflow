library(sf)
library(janitor)
# 1. Lotes ----------------------------------------------------------------
lotes <- st_read("raw/cofopri/MVCS/MVCS/PSAD56/LOTES_PSAD56.shp")
lotes_wgs84 <- lotes %>% st_transform(crs = 4326) %>% clean_names() 
write_sf(lotes_wgs84,'processed/lotes_wgs84.gpkg')
rm(list = ls())

# 2. Manzanas -------------------------------------------------------------
manzana <- st_read("raw/cofopri/MVCS/MVCS/PSAD56/MANZANAS_PSAD56.shp")
manzana_wgs84 <- manzana %>% st_transform(crs = 4326) %>% clean_names() 
write_sf(manzana_wgs84,'processed/manzana_wgs84.gpkg')
rm(list = ls())

# 3. Pueblos --------------------------------------------------------------
pueblo <- st_read("raw/cofopri/MVCS/MVCS/PSAD56/puebloS_PSAD56.shp")
pueblo_wgs84 <- pueblo %>% st_transform(crs = 4326) %>% clean_names() 
write_sf(pueblo_wgs84,'processed/pueblo_wgs84.gpkg')
rm(list = ls())

# 4. Titulares ------------------------------------------------------------
# Obtener nombres de las hojas
archivo <- "raw/cofopri/PEDIDO_MVCS_TITULARES_27NOV_2024.xlsx"
hojas <- read_excel(path = archivo,sheet = 1)
hojas_clean <- hojas %>% clean_names()
write_csv(hojas_clean, "processed/titulares.csv")

