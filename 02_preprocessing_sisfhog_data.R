rm(list = ls())
library(tidyverse)
library(janitor)

# 1. Preprocesing data ----------------------------------------------------
study_area <- c("tacna")
data_hog <- read_delim(
  "raw/sisfogh/PGH_MAR_2025/PGH_HOGAR_PPSS.txt",
  delim = "|",
  locale = locale(encoding = "latin1")) %>%
  clean_names() %>%
  filter(hogar_departamento_0 %in% str_to_upper(study_area)) %>% 
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
      .default = "Sin Dato")) %>% 
  filter(hogar_area != "RURAL")
  
data_per <- read_delim(
  "raw/sisfogh/PGH_MAR_2025/PGH_PERSONA_PPSS.txt",
  delim = "|",
  locale = locale(encoding = "latin1")) %>%
  clean_names() %>% 
  mutate(
    persona_tipo_doc = case_when(
      persona_tipo_doc == "00030203" ~ "PARTIDA DE NACIMIENTO MANUAL",
      persona_tipo_doc == "00030205" ~ "CARNET DE EXTRANJERIA",
      persona_tipo_doc == "00030206" ~ "CARNET DE PERMISO TEMPORAL DE PERMANENCIA",
      .default = "DNI"
    ),
    persona_sexo = case_when(
      persona_sexo == "1"~ "HOMBRE",
      persona_sexo == "2"~ "MUJER",
      .default = "Sin Dato"
    )
  )

data.sisfog <- data_hog %>% 
  left_join(data_per,by = "hogar_id")

write_excel_csv2(data.sisfog,paste0('processed/data_sisfog_',first(study_area),'.csv'))