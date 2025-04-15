library(sf)
library(janitor)
library(readxl)
library(tidyverse)
sf_use_s2(use_s2 = FALSE)

# 1. Lotes ----------------------------------------------------------------
lotes <- st_read("raw/cofopri/MVCS/MVCS/PSAD56/LOTES_PSAD56.shp") %>% 
  distinct(across(everything()), .keep_all = TRUE)

lotes_wgs84 <- lotes %>% st_transform(crs = 4326) %>% clean_names() %>% st_make_valid()
write_sf(lotes_wgs84,'processed/lotes_wgs84.gpkg')
rm(list = ls())

# 2. Manzanas -------------------------------------------------------------
manzana <- st_read("raw/cofopri/MVCS/MVCS/PSAD56/MANZANAS_PSAD56.shp") %>% 
  distinct(across(everything()), .keep_all = TRUE)

manzana_wgs84 <- manzana %>% st_transform(crs = 4326) %>% clean_names() %>% st_make_valid() 
write_sf(manzana_wgs84,'processed/manzana_wgs84.gpkg')
rm(list = ls())

# 3. Pueblos --------------------------------------------------------------
pueblo <- st_read("raw/cofopri/MVCS/MVCS/PSAD56/puebloS_PSAD56.shp") %>% 
  distinct(across(everything()), .keep_all = TRUE)

pueblo_wgs84 <- pueblo %>% st_transform(crs = 4326) %>% clean_names() %>% st_make_valid()
write_sf(pueblo_wgs84,'processed/pueblo_wgs84.gpkg')
rm(list = ls())

# 4. Titulares ------------------------------------------------------------
# Obtener nombres de las hojas
archivo <- "raw/cofopri/PEDIDO_MVCS_TITULARES_27NOV_2024.xlsx"
sheets <- readxl::excel_sheets("raw/cofopri/PEDIDO_MVCS_TITULARES_27NOV_2024.xlsx")
fun_excel <- function(x){
  data <- read_excel(path = archivo,sheet = sheets[x]) %>% clean_names()
  return(data)
}
lista_excel <- lapply(1:length(sheets), FUN = fun_excel) %>% map_df(.f = as_tibble)
write_excel_csv2(lista_excel, "processed/titulares_v2.csv")