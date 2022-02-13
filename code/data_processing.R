library(tidyverse)
library(magrittr)
library(ggplot2)
library(leaflet)
library(rgdal)
library(raster)
library(plotly)
library(lubridate)
library(shinydashboard)
library(readxl)

# crs

crs <- read_csv("data/crs.csv")

# Casos de dengue por município/ Brasil

df <- read.csv("data/data_dengue.csv")

df <- df %>%
  filter(UF == "RS") %>%
  dplyr::select(cod_ibge, case_1000_hab, number_cases, municipy_name, year) %>%
  rename(Ano = year, Município = municipy_name, "Casos 1000 hab" = case_1000_hab, Casos = number_cases)

df <- left_join(df, crs, by = c("Município" = "municipio"))

# Poligono para gradiente de casos por município

my_spdf <- readOGR(
  dsn = paste0(getwd(), "/data/shapefile"),
  layer = "Municipios_IBGE",
  verbose = FALSE
)

# transformando as colunas

my_spdf@data$cod <- as.character(my_spdf@data$Cod_ibge)
my_spdf@data$cod <- substr(my_spdf@data$cod, 1, 6)
my_spdf@data$cod <- as.numeric(my_spdf@data$cod)

# duplicando myspdf para usar na aba casos atuais e historicos

raster_2021 <- my_spdf

raster_historico <- my_spdf

# oraganizando os outros dados de ocorrência

occ <- read_csv("data/occ_aaegypti.csv") %>% dplyr::select(-X4)

# occ$...4 <- as.Date(occ$...4, format = "%d/%m/%Y")
# 
# occ %<>% mutate(year = case_when(
#   !is.na(`...4`) ~ year(`...4`),
#   TRUE ~ year
# )) %>% dplyr::select(-`...4`)

# Icone do mosquito para identificação de ocorrência

fly <- makeIcon(
  iconUrl = "https://cdn-icons-png.flaticon.com/512/4975/4975984.png",
  iconWidth = 25,
  iconHeight = 25
)

# Dados de 2021

casos <- list.files(path = "data/casos_2021/", pattern = "*.csv", include.dirs = T)

# setando o diretorío com os dados

dir <- paste0(getwd(), "/data/casos_2021")

setwd(dir)

# lendo todos os datasets

casos <- lapply(casos, read_csv)

# voltando para a pasta shiny

casos_autoctones_2021 <- casos[[1]] %>% mutate(
  Cod = substr(x = Municipio, start = 1, stop = 6),
  Municipio = substr(x = Municipio, start = 8, stop = 40)
)

municipio_pop <- read_excel("municipio_pop.xlsx")

casos_autoctones_2021_pop <- left_join(casos_autoctones_2021, municipio_pop, by = c("Municipio" = "Município"))

casos_autoctones_2021_pop <- left_join(casos_autoctones_2021_pop, crs, by = c("Municipio" = "municipio"))

casos_autoctones_2021_pop[, "casos_1000_hab"] <- (1000 * casos_autoctones_2021_pop[, "Frequência"]) / casos_autoctones_2021_pop[, "População estimada"]

casos_geral <- casos[[2]] %>% mutate(
  Cod = substr(x = Municipio, start = 1, stop = 6),
  Municipio = substr(x = Municipio, start = 8, stop = 40)
)

casos_por_escolaridade <- casos[[3]] %>%
  mutate(
    Municipio = substr(x = Municipio, start = 8, stop = 40),
    `Ign/Branco` = `Ign/Branco` + `Não se aplica`
  ) %>%
  dplyr::select(-`Não se aplica`) %>%
  pivot_longer(!Municipio, names_to = "categoria", values_to = "total")

casos_por_evolucao <- casos[[4]] %>%
  mutate(
    Municipio = substr(x = Municipio, start = 8, stop = 40)
  ) %>%
  pivot_longer(!Municipio, names_to = "categoria", values_to = "total")

casos_por_faixa_etaria <- casos[[5]] %>%
  mutate(
    Municipio = substr(x = Municipio, start = 8, stop = 40)
  ) %>%
  pivot_longer(!Municipio, names_to = "categoria", values_to = "total")

casos_por_gestante <- casos[[6]] %>%
  mutate(
    Municipio = substr(x = Municipio, start = 8, stop = 40),
    `Ign/Branco` = `Não` + `Não se Aplica` + `Idade gestacional Ignorada`
  ) %>%
  dplyr::select(-`Idade gestacional Ignorada`, -`Não se Aplica`, -`Não`) %>%
  pivot_longer(!Municipio, names_to = "categoria", values_to = "total")

casos_por_sexo <- casos[[7]] %>%
  mutate(
    Municipio = substr(x = Municipio, start = 8, stop = 40)
  ) %>%
  pivot_longer(!Municipio, names_to = "categoria", values_to = "total") %>%
  as_tibble()

casos_por_zona <- casos[[8]] %>%
  mutate(
    Municipio = substr(x = Municipio, start = 8, stop = 40)
  ) %>%
  pivot_longer(!Municipio, names_to = "categoria", values_to = "total")

setwd("../..")
