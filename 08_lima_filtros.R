rm(list = ls())
library(tidyverse)
library(openxlsx)
library(janitor)

# 1. Reading data ---------------------------------------------------------
bpvvrs_before_pre <- read.xlsx("extra/Base del BPVVRS-Desembolsos_LIMA (06.01.2025).xlsx",sheet = 3) %>% 
  clean_names() %>% 
  filter(estado_de_obra %in% c('CULMINADA'))

bpvvrs_after_pre <- read.xlsx("extra/Base del BPVVRS-Desembolsos_LIMA (06.01.2025).xlsx",sheet = 5) %>% 
  clean_names() %>% 
  filter(estado_de_obra %in% c('CULMINADA','EJECUCIÓN','SIN INICIO'))

viviendas_asignadas_pre <- read.xlsx("extra/VIVIENDAS ASIGNADAS.xlsx") %>% 
  clean_names() %>% 
  rename(
    predio = codigo_de_predio,
    paterno = a_paterno,
    materno = a_materno)

bpvvrs_after_pre_last_version <- read.xlsx("extra/Base del BPVVRS-Desembolsos_LIMA (06.01.2025).xlsx",sheet = 5) %>% 
  clean_names() %>% 
  filter(of %in% '459-2025' | oficio_a_fmv %in% '392-2025')

# 2. Preprocessing data ---------------------------------------------------
variables <- c(
  "dni",
  "nombres",
  "paterno",
  "materno",
  "distrito",
  "predio"
  )

bpvvrs_before <- bpvvrs_before_pre %>% 
  select(all_of(variables))

bpvvrs_after <- bpvvrs_after_pre %>% 
  select(all_of(variables))

bpvvrs_last_version <- bpvvrs_after_pre_last_version %>% 
  select(all_of(variables))

viviendas_asignadas <- viviendas_asignadas_pre %>% 
  select(all_of(variables))

bbdd <- bind_rows(bpvvrs_before, bpvvrs_after, viviendas_asignadas, bpvvrs_last_version)
write_excel_csv2(bbdd,'output/lista_ya_ejecutada.csv')