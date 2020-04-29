# setup -------------------------------------------------------------------
  if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, tidyverse, janitor, htmlwidgets, sjlabelled, expss, haven, chilemapas, highcharter, lubridate, geojsonsf, jsonlite, spdplyr,
               tm,stringi, shiny, shinythemes, DT)
url2 <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto1/Covid-19.csv" #Datos minciencia
url3 <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto6/bulk/data.csv" #Tasa de incidencia
covid <- read.csv(url2) %>% clean_names()
tasa_incidencia <- read_csv(url3) %>% clean_names()
last_date <- gsub("x","",names(covid)[length(covid)-1])
last_date <- gsub("_","-",last_date)

comunas <- codigos_territoriales %>%
  select(codigo_comuna,nombre_comuna) %>%
  distinct()

covid <- covid %>% mutate(n_casos = covid %>% select(names(.)[length(names(covid))-1]) %>% pull()) %>% 
  select(codigo_region,region,codigo_comuna,comuna,n_casos,tasa) %>% 
  mutate(n_casos = as_numeric(n_casos))

tasa_incidencia <- tasa_incidencia %>%
  filter(fecha == last_date) %>% 
  select(comuna_id,tasa,poblacion) %>% 
  rename(incidencia = tasa) %>% 
  mutate(comuna_id = comuna_id)

covid <- covid %>% left_join(tasa_incidencia, by = c("codigo_comuna"="comuna_id"))

mapa_comunas <- mapa_comunas %>% mutate(codigo_comuna = gsub("^0","",codigo_comuna),
                                        codigo_region = gsub("^0","",codigo_region))