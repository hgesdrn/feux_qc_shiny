library(shiny)
library(leaflet)
library(dplyr)
library(shinyWidgets)
library(ggplot2)
library(scales)
library(ggpubr)
library(sf)
library(readr)
library(tidyr)

# Périodes fixes
periodes <- c("1900-1909","1910-1919","1920-1929","1930-1939","1940-1949","1950-1959","1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2019", "2020-2029")

# Données agrégées
url_aggr <- "https://raw.githubusercontent.com/hgesdrn/feux_qc_shiny/main/data/feux_aggr.RDS"
aggr_data <- readRDS(gzcon(url(url_aggr)))

# Chargement de la couche de province sans le Saguenay
url_prov <- "https://github.com/hgesdrn/feux_qc_shiny/blob/main/data/prov_sf.rds?raw=true"
prov_sf <- readRDS(gzcon(url(url_prov)))

# UI
ui <- fluidPage(
  titlePanel("Feux de forêt au Québec de 1900 à 2023"),
  fluidRow(
    column(4,
           # Hauteur égale à celle de la carte (800px)
           div(style = "height:800px; overflow-y:auto;",
               sliderTextInput("periode", "Choisir une période :",
                               choices = periodes,
                               selected = periodes[1],
                               grid = TRUE,
                               animate = TRUE),
               br(),
               plotOutput("barplots", height = "800px")
           )
    ),
    column(8,
           leafletOutput("carte", height = "800px")
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Réactif : charge les polygones de la période sélectionnée
  feux_filtrés <- reactive({
    req(input$periode)
    url <- paste0("https://raw.githubusercontent.com/hgesdrn/feux_qc_shiny/main/data/periodes/feux_", input$periode, ".rds")
    readRDS(gzcon(url(url)))
  })
  
  # Carte de base
  output$carte <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = prov_sf,
                  color = NA,
                  fillColor = "gray30",
                  fillOpacity = 0.6,
                  weight = 0,
                  group = "Province") %>%
      fitBounds(lng1 = -79.5, lat1 = 44.5, lng2 = -56.5, lat2 = 63)
  })
  
  # Mise à jour des polygones de feux
  observe({
    leafletProxy("carte") %>%
      clearGroup("Feux") %>%
      addPolygons(
        data = feux_filtrés(),
        color = NA,
        fillColor = "#8B0000",
        fillOpacity = 0.75,
        weight = 0,
        group = "Feux"
      )
  })
  
  # Graphiques
  output$barplots <- renderPlot({
    req(input$periode)
    
    toutes_periodes <- periodes
    
    get_region_data <- function(region_name) {
      aggr_data %>%
        filter(Region == region_name) %>%
        right_join(data.frame(Periode = toutes_periodes), by = "Periode") %>%
        mutate(
          Superficie = replace_na(Superficie, 0),
          Selection = Periode == input$periode
        )
    }
    
    data_saguenay <- get_region_data("Saguenay")
    data_quebec <- get_region_data("Québec")
    
    plot_region <- function(data, show_x_labels) {
      p <- ggplot(data, aes(x = Periode, y = Superficie, fill = Selection)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "gray80"), guide = "none") +
        scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M", accuracy = 0.1)) +
        labs(y = "Superficie brûlée (M ha)") +
        theme_minimal() +
        theme(
          axis.text.y = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold")
        )
      
      if (show_x_labels) {
        p <- p +
          labs(x = "Période") +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
            axis.title.x = element_text(face = "bold")
          )
      } else {
        p <- p +
          theme(
            axis.text.x = element_blank(),
            axis.title.x = element_blank()
          )
      }
      
      return(p)
    }
    
    p1 <- plot_region(data_saguenay, show_x_labels = FALSE) + ggtitle("Région : Saguenay")
    p2 <- plot_region(data_quebec, show_x_labels = TRUE) + ggtitle("Région : Québec")
    
    ggarrange(p1, p2, ncol = 1, heights = c(1, 1))
  })
}

# Lancer l'application
shinyApp(ui, server)
