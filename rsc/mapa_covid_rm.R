# setup -------------------------------------------------------------------
pacman::p_load(readxl, tidyverse, janitor, sjlabelled, expss, haven, chilemaps, highcharter, lubridate, geojsonsf, jsonlite, spdplyr,
              tm,stringi)
covid <- read_delim("data/covid.csv", ";", 
                    escape_double = FALSE, locale = locale(encoding = "ISO-8859-1", 
                                                           asciify = TRUE), trim_ws = TRUE)
# covid RM ----------------------------------------------------------------
#

# n de casos
covid <- covid %>% 
  rename(n_casos = n_07_04) %>% 
  mutate(n_casos = gsub("-","0",n_casos),
         n_casos = as.numeric(n_casos),
         incidencia = gsub("-","0",incidencia),
         incidencia = gsub("\\,",".",incidencia),
         incidencia = as.numeric(incidencia))
# comuna

comunas <- territorial_codes %>%
  select(commune_id,commune_name) %>%   # Importante tener etiqueta en ambas df (region_name)
  distinct() 
comunas <- comunas %>% mutate(commune_name = tolower(commune_name))

covid <- covid %>% mutate(comuna = tolower(comuna),
                          comuna = stri_trans_general(comuna,"Latin-ASCII"))

covid <- covid %>% left_join(comunas, by = c("comuna"="commune_name"))


# mapa --------------------------------------------------------------------

mapa_rm <- covid %>% 
  select(comuna,commune_id,n_casos,incidencia) %>%
  filter(commune_id %in% communes_map[[13]]$commune_id)

mapa_rm <- mapa_rm %>% mutate_all(funs(replace_na(.,0)))

mapa_region_met <- communes_map[[13]] %>%
  left_join(territorial_codes %>%
              select(commune_id, commune_name) %>%
              distinct()) %>%
  left_join(mapa_rm) 

mapa_region_met <- mapa_region_met %>% mutate_all(funs(replace_na(.,0)))

mapa_region_met_gjson <- sf_geojson(mapa_region_met) #transformo shapefile a geo json

mapa_region_met <- jsonlite::fromJSON(mapa_region_met_gjson, simplifyVector = FALSE)  #guardo objeto a json


#Mapa 
highchart(type = 'map') %>% 
  hc_add_series_map(mapa_region_met, mapa_rm, value = "n_casos", joinBy = c("commune_id"),
                    showInLegend = TRUE, name = "commune_name") %>%  
  # hc_colorAxis(stops = color_stops()) %>% 
  hc_tooltip(useHTML = TRUE, headerFormat = "{point.name}",
             pointFormat = "<b>{point.comuna}</b><br> Cantidad de casos: <b> {point.n_casos} </b><br>
             Incidencia: {point.incidencia}",
             style = list(fontSize = "15px" )) %>% 
  hc_mapNavigation(enabled = TRUE) %>% 
  hc_title(text = "Región Metropolitana - Covid-19") %>% 
  hc_subtitle(text = "Casos por comuna al 07-04-2020.",align = "center") %>% 
  hc_size(height = 750,width = 900) %>% 
  hc_credits(enabled = TRUE, text = "Fuente: INFORME EPIDEMIOLÓGICO. COVID-19. 06-04-2020.",
                                                href = "https://www.minsal.cl/wp-content/uploads/2020/04/Reporte_COVID_19_06_04_2020.pdf")
