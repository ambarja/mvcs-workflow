library(tidyverse)
library(janitor)
library(sf)
library(purrr)
library(readxl)

# 1. Lectura de datos del SISFHOG -----------------------------------------
data_hog <- read_delim("raw/sisfogh/PGH_MAR_2025/PGH_HOGAR_PPSS.txt",delim = "|") %>%
  clean_names() %>%
  mutate(
    hogar_cse_niv_pobreza = str_remove_all(string = hogar_cse_niv_pobreza ,pattern = "-| UTC"),
    hogar_cse_niv_pobreza = if_else(
      condition = nchar(hogar_cse_niv_pobreza) == 6,
      true = paste0('00',hogar_cse_niv_pobreza),
      false = hogar_cse_niv_pobreza),
    hogar_cse_niv_pobreza = case_when(
      hogar_cse_niv_pobreza == "00220301" ~ "POBRE EXTREMO",
      hogar_cse_niv_pobreza == "00220302" ~ "POBRE",
      hogar_cse_niv_pobreza == "00220303" ~ "NO POBRE",
      .default = "NA"),
    vigente = case_when(
      vigente == "1" ~ "SI",
      .default = "NO"),
    hogar_area = case_when(
      hogar_area == "1" ~ "URBANO", 
      hogar_area == "1" ~ "RURAL",
      .default = "Sin Dato")
    ) %>% 
    filter(hogar_area != "RURAL")

data_per <- read_delim("raw/sisfogh/PGH_MAR_2025/PGH_PERSONA_PPSS.txt",delim = "|") %>%
  clean_names() 

data.sisfog <- data_hog %>% 
  left_join(data_per,by = "hogar_id")

# 2. Lectura de datos de COFOPRI ------------------------------------------
predios_sf <- st_read("processed/cofopri/BASE GRAFICA MVCS/PSAD56.gdb",layer = "Lotes")
predios_sf <- predios_sf %>% clean_names()

