# CentroGeo

# Shiny en una sentada ----

# Semana de las Ciencias de Información Geoespacial----

# Taller por: ----
# Dr: Pablo López Ramírez - @plablo09 - pablo.lopez@centrogeo.edu.mx 
# Noé Osorio García - @NoeOsorioPK - al.nosorio@centrogeo.edu.mx


# Las bases usadas corresponden a las publicadas por el SESNSP con corte al 31 de agosto del 2022
# Disponibles aquí
# https://www.gob.mx/sesnsp/acciones-y-programas/datos-abiertos-de-incidencia-delictiva?state=published
# Carpetas y Víctimas.
# 11 Delitos prioritarios:

# 1	Extorsión - Víctimas
# 2	Feminicidio - Víctimas
# 3	Robo en transporte público individual - Carpetas
# 4	Homicidio doloso - Víctimas
# 5	Robo a negocio - Carpetas
# 6	Robo a transeúnte - Carpetas-Incluye robo a transeúnte en espacio abierto al público y en vía pública
# 7	Robo a casa habitación - Carpetas
# 8	Robo a transportista - Carpetas
# 9	Robo en transporte público colectivo - Carpetas
# 10	Robo de vehículo automotor - Carpetas - Se refiere a delitos de robo de coche de cuatro ruedas
# 11	Secuestro - Víctimas


# Encoding, Windows, linux o cambia si es MAC
# Sys.setlocale("LC_TIME", "es_ES")
Sys.setlocale("LC_ALL", "Spanish")

# Librerías ----
library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(ggrepel)

# Bases input ----
# Encoding de latin 1 para que reconozca los asentos 

# Base_ ts es el histórico por delito desdel 2015 al 2022
# con el total a nivel nacional de Carpetas O víctimas 
base_ts <- read_rds("01_input/ts_delitos_prioritarios.rds")
Encoding(base_ts$subtipo_de_delito) <- "latin1"

# BAse Mapa:
# Incluye los últimos dos meses de información del SESNSP
# con el total a nivel nacional de Carpetas O víctimas por entidad.
# y su geometría del Marco Geoestadístico.
base_mapa <- read_rds("01_input/mapa_simplificado.rds")
base_mapa <- sf::st_as_sf(base_mapa)
Encoding(base_mapa$subtipo_de_delito) <- "latin1"
Encoding(base_mapa$entidad) <- "latin1"


# Ui 
ui <- shinyUI(
  navbarPage(
    title = "Taller de Shiny, Semana CIG 2022, CentroGeo",
    fluid = TRUE,
    collapsible = TRUE,
    tabPanel("Visualizador",#General
             fluidRow(
               tagList(
                 div(class = "container",
                     tags$img(src = "CentroGeo-Logo_H.ai.png", height="10%", width="10%"),
                     h1("Visualizador de delitos",class = "title fit-h1"),
                     p(" La información presentada corresponde a los datos públicados por Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública.
                             Con información reportada por las Procuradurías Generales de Justicia o Fiscalías Generales de las 32 entidades federativas.
                              La información está actualizada al més de agosto 2022")))),
             column(12,
                    h4("Tipo de delito"),
                    selectInput("id_delito",
                                "Selecciona un delito",
                                c("Robo a casa habitación",
                                  "Robo a negocio",                   
                                  "Robo a transeúnte",                
                                  "Robo a transportista",
                                  "Robo en transporte público colectivo",
                                  "Robo en transporte público individual",
                                  "Robo de vehículo automotor",
                                  "Extorsión",                         
                                  "Feminicidio",                   
                                  "Homicidio doloso",
                                  "Secuestro"))),
             br(),
             column(7,
                    leafletOutput("mapa",
                                  width = 700, height = 600)),
             column(5,
                    plotOutput("serie",
                               height = 300)),
             column(5,
                    plotOutput("scatter",
                               height = 300))
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Reactividad  de los mapas
  b_mapa <- reactive({
    base_mapa %>% 
      filter(fecha==as.Date("2022-08-01"))
  })
  # Reactividad de la serie
  b_ts <- reactive({
    base_ts
  })
  # Reactividad del scatter
  b_scatter <- reactive({
    base_mapa
  })
  # Mapa leaflet
  output$mapa <- renderLeaflet({
    base <- b_mapa()[b_mapa()$subtipo_de_delito==input$id_delito,]
    pal <- colorNumeric(palette = "inferno",domain =b_mapa()[b_mapa()$subtipo_de_delito==input$id_delito,]$Total)    
    
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap) %>% 
      addPolygons(data=base$geometry,
                  fillColor = pal(base$Total),
                  color="black",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = .7,
                  label = base$NOMGEO,
                  group = "Entidades")%>% 
      addLegend(pal = pal,values = base$Total) %>% 
      addLayersControl(overlayGroups = "Entidades")
  })
  # Mapa serie de tiempo
  output$serie <- renderPlot({
    b_ts() %>% 
      filter(subtipo_de_delito==input$id_delito) %>%
      ggplot(aes(fecha,Total))+
      geom_line()+
      geom_point(data=. %>% 
                   filter(Total==max(Total)),
                 size=3,col="red"
      )+
      geom_point(data=. %>% 
                   filter(fecha==max(fecha)),
                 size=3
      )+
      theme_classic()
  })
  
  #Scatter PLot 
  
  output$scatter <- renderPlot({
    base <- b_scatter() %>% 
      filter(subtipo_de_delito==input$id_delito)
    
    ejes <- base %>% filter(Total==max(Total))
    
    base %>% 
      filter(subtipo_de_delito==input$id_delito) %>% 
      st_drop_geometry() %>%
      pivot_wider(id_cols = c('clave_ent','entidad'),
                  names_from = 'fecha',
                  values_from = 'Total') %>% 
      ggplot(aes(`2022-07-01`,`2022-08-01`))+
      geom_point()+
      scale_y_continuous(limits = c(0,ejes$Total))+
      scale_x_continuous(limits = c(0,ejes$Total))+
      geom_abline()+
      theme_classic()+
      ggrepel::geom_text_repel(aes(label=entidad))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)