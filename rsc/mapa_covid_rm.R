# setup -------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, tidyverse, janitor, htmlwidgets, sjlabelled, expss, haven, chilemapas, highcharter, lubridate, geojsonsf, jsonlite, spdplyr,
              tm,stringi)
url1 <- "https://raw.githubusercontent.com/jorgeperezrojas/covid19-data/master/csv/confirmados_comunas.csv" #Datos jorge perez
url2 <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto1/Covid-19.csv" #Datos minciencia
url3 <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto6/bulk/data.csv" #Tasa de incidencia
covid <- read.csv(url2) %>% clean_names()
tasa_incidencia <- read_csv(url3) %>% clean_names()
# covid --------------------------------------------------------------------

last_date <- gsub("x","",names(covid)[length(covid)-1])
last_date <- gsub("_","-",last_date)

# covid <- covid %>% mutate_all(funs(gsub("-",0,.))) 

covid <- covid %>% mutate(n_casos = covid %>% select(names(.)[length(names(covid))-1]) %>% pull()) %>% 
  filter(codigo_region==13) %>% 
  select(region,comuna,n_casos,tasa) %>% 
  mutate(n_casos = as_numeric(n_casos))


# comuna

comunas <- codigos_territoriales %>% 
  select(codigo_comuna,nombre_comuna) %>% 
  distinct() 



covid <- covid %>% mutate(
                          comuna = stri_trans_general(comuna,"Latin-ASCII"))


covid <- covid %>% left_join(comunas, by = c("comuna"="nombre_comuna"))


# tasa de incidencia ------------------------------------------------------

tasa_incidencia <- tasa_incidencia %>% filter(fecha == last_date,
                           region_id==13) %>% 
  select(comuna_id,tasa,poblacion) %>% 
  rename(incidencia = tasa) %>% 
  mutate(comuna_id = as_character(comuna_id))
  
covid <- covid %>% left_join(tasa_incidencia, by = c("codigo_comuna"="comuna_id"))
# mapa --------------------------------------------------------------------

mapa_rm <- covid %>% 
  select(comuna,codigo_comuna,n_casos,incidencia,poblacion) %>%
  filter(codigo_comuna %in% mapa_comunas$codigo_comuna)



mapa_region_met <- mapa_comunas %>%
  filter(codigo_region=="13") %>% 
  left_join(codigos_territoriales %>%
              select(codigo_comuna, nombre_comuna) %>%
              distinct()) %>%
  left_join(mapa_rm)

mapa_region_met <- mapa_region_met %>% mutate_all(funs(replace_na(.,0)))

mapa_region_met <- st_as_sf(mapa_region_met)
mapa_region_met_gjson <- sf_geojson(mapa_region_met) #transformo shapefile a geo json

mapa_region_met <- jsonlite::fromJSON(mapa_region_met_gjson, simplifyVector = FALSE)  #guardo objeto a json


#Mapa 
hc <- highchart(type = 'map') %>% 
  hc_add_series_map(mapa_region_met, mapa_rm, value = "n_casos", joinBy = c("codigo_comuna"),
                    showInLegend = TRUE, name = "comuna",
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.nombre_comuna}')) %>%  
  hc_plotOptions(lang = list(thousandSep = ".")) %>% 
  hc_tooltip(useHTML = TRUE, headerFormat = "{point.properties.nombre_comuna}",
             pointFormat = "<b><strong>{point.comuna}</b><br> </strong> <br>
             Cantidad de casos: <b> {point.n_casos} </b><br>
             Tasa de incidencia: <b> {point.properties.incidencia:.1f}</b><br>
             Población: <b> {point.properties.poblacion:,.0f}",
             style = list(fontSize = "15px" )) %>% 
  hc_mapNavigation(enabled = TRUE) %>% 
 
  hc_title(text = "Región Metropolitana - Covid-19") %>% 
  hc_subtitle(text = glue::glue("Casos por comuna al { last_date }" ), align = "center") %>% 
  hc_size(height = 750,width = 900) %>% 
  hc_credits(enabled = TRUE, text = paste("Fuente: INFORME EPIDEMIOLÓGICO. COVID-19, al",last_date),
                                                href = "https://www.minsal.cl/")

hc
# save --------------------------------------------------------------------


saveWidget(hc, file="index.html")
