# setup -------------------------------------------------------------------
pacman::p_load(readxl, tidyverse, janitor, htmlwidgets, sjlabelled, expss, haven, chilemapas, highcharter, lubridate, geojsonsf, jsonlite, spdplyr,
              tm,stringi)

covid <- read_csv("https://raw.githubusercontent.com/jorgeperezrojas/covid19-data/master/csv/confirmados_comunas.csv")
# covid RM ----------------------------------------------------------------
#

covid <- covid %>% mutate_all(funs(gsub("-",0,.))) 
covid <- covid %>% mutate(n_casos = covid %>% select(tail(names(.),1)) %>% pull()) %>% 
  filter(codigo_region==13) %>% 
  select(region,comuna,n_casos) %>% 
  mutate(n_casos = as_numeric(n_casos))

# comuna

comunas <- codigos_territoriales %>% 
  select(codigo_comuna,nombre_comuna) %>% 
  distinct() 



covid <- covid %>% mutate(
                          comuna = stri_trans_general(comuna,"Latin-ASCII"))


covid <- covid %>% left_join(comunas, by = c("comuna"="nombre_comuna"))


# mapa --------------------------------------------------------------------

mapa_rm <- covid %>% 
  select(comuna,codigo_comuna,n_casos) %>%
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
  # hc_colorAxis(stops = color_stops()) %>% 
  hc_tooltip(useHTML = TRUE, headerFormat = "{point.properties.nombre_comuna}",
             pointFormat = "<b>{point.comuna}</b><br> Cantidad de casos: <b> {point.n_casos} </b><br>",
             style = list(fontSize = "15px" )) %>% 
  hc_mapNavigation(enabled = TRUE) %>% 
  hc_title(text = "Región Metropolitana - Covid-19") %>% 
  hc_subtitle(text = glue::glue("Casos por comuna al { format(Sys.Date(), '%d- %m-%Y') }" ), align = "center") %>% 
  hc_size(height = 750,width = 900) %>% 
  hc_credits(enabled = TRUE, text = paste("Fuente: INFORME EPIDEMIOLÓGICO. COVID-19, al",Sys.Date()),
                                                href = "https://www.minsal.cl/wp-content/uploads/2020/04/Reporte_COVID_19_06_04_2020.pdf")

hc
# save --------------------------------------------------------------------

saveWidget(hc, file="index.html")
