# setup -------------------------------------------------------------------
source("../rsc/data_treatment.R", encoding = "UTF-8")
 

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    

    # Application title
    titlePanel("Casos de COVID19 en Chile"),
    
    theme = shinythemes::shinytheme('cerulean'),

    sidebarLayout(
        sidebarPanel(
            selectInput("region",
                        label = "Seleccione región",
                        choices = levels(covid$region))
        ),

        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(
            tabPanel("Mapa",highchartOutput("mapa", height = "650px")),
            tabPanel("Tabla", DT::DTOutput('table'))     
        )
    )
)
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    cod_reg <- reactive({
        
        if (input$region == "Nacional"){
            cod_reg <- "cl"
        } else {
        cod_reg <- covid %>% 
            filter(region == input$region) %>% 
            select(codigo_region) %>% 
            last() %>% as_character()
        }
    }) 
    
    
    output$mapa <- renderHighchart({ 
        
        cod_reg <- cod_reg()
        
        covid2map <- covid %>% filter(as_character(codigo_region) == as_character(cod_reg)) %>% 
            mutate(codigo_region = as_character(codigo_region)) %>% 
            select(codigo_region,region,comuna,codigo_comuna,n_casos,incidencia,poblacion) %>%
            filter(codigo_comuna %in% mapa_comunas$codigo_comuna)
        
        name_region <- if ( as.character(cod_reg) == "13" ) { 
            paste("Región",covid2map %>%
                      select(region) %>% 
                      last() %>% as.character(), sep = " ")
        } else  {
        paste("Región de ",covid2map %>%
            select(region) %>% 
            last() %>% as.character(), sep = " ")
                }

        mapa <- mapa_comunas %>%
            filter(codigo_region == cod_reg) %>% 
            left_join(codigos_territoriales %>%
                          select(codigo_comuna, nombre_comuna) %>%
                          distinct() %>% 
                          mutate(codigo_comuna = gsub("^0","",codigo_comuna)
                                 )) %>%
            left_join(covid2map %>%
                          mutate(codigo_comuna = as_character(codigo_comuna))) %>% 
            mutate(codigo_comuna = gsub("^0","",codigo_comuna)) %>% 
            sf::st_as_sf() %>% 
            geojsonsf::sf_geojson() 
        mapa <-   jsonlite::fromJSON(mapa,simplifyVector = FALSE) 
        
        hc <- highchart(type = 'map') %>% 
            hc_add_series_map(mapa, covid2map, value = "n_casos", joinBy = c("codigo_comuna"),
                              showInLegend = TRUE, name = "comuna",
                              dataLabels = list(enabled = TRUE,
                                                format = '{point.properties.nombre_comuna}')) %>%  
            hc_plotOptions(lang = list(thousandSep = ".")) %>% 
            hc_xAxis(minRange = 0.1) %>% 
            hc_tooltip(useHTML = TRUE, headerFormat = "{point.properties.nombre_comuna}",
                       pointFormat = "<b><strong>{point.comuna}</b><br> </strong> <br>
             Cantidad de casos: <b> {point.n_casos} </b><br>
             Tasa de incidencia: <b> {point.properties.incidencia:.1f}</b><br>
             Población: <b> {point.properties.poblacion:,.0f}",
                       style = list(fontSize = "15px" )) %>% 
            hc_mapNavigation(enabled = TRUE, enableMouseWheelZoom = TRUE) %>% 
            hc_title(text = paste0(name_region,"- Casos COVID19")) %>% 
            hc_subtitle(text = glue::glue("Casos por comuna al { last_date }" ), align = "center") %>% 
            hc_size(height = 650, width = 900) %>% 
            hc_credits(enabled = TRUE, text = paste("Fuente: INFORME EPIDEMIOLÓGICO. COVID-19, al",last_date),
                       href = "https://www.minsal.cl/")
        
        hc
        
        
    })
    
    output$table <- DT::renderDT({
        
        
        covid %>% filter(region == input$region)  %>% 
            select(comuna,poblacion,n_casos,incidencia) %>% 
            mutate(incidencia = round(as_numeric(incidencia))) %>% 
            arrange(-n_casos)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
