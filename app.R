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

# Définir manuellement les périodes (puisqu'on ne peut pas faire list.files() en ligne)
periodes <- c("1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2019", "2020-2029")

# Charger les périodes disponibles localement
# periode_files <- list.files("data/periodes", pattern = "feux_.*\\.rds", full.names = FALSE)
# periodes <- gsub("feux_|\\.rds", "", basename(periode_files)) %>% sort()


# Charger les données agrégées pour les graphiques
# aggr_data <- readRDS("data/feux_aggr.RDS")
url <- "https://raw.githubusercontent.com/hgesdrn/feux_qc_shiny/main/data/feux_aggr.RDS"
aggr_data <- readRDS(gzcon(url(url)))


# UI
ui <- fluidPage(
  titlePanel("Feux de forêt au Québec par période"),
  sidebarLayout(
    sidebarPanel(
      sliderTextInput("periode", "Choisir une période :",
                      choices = periodes,
                      selected = periodes[1],
                      grid = TRUE,
                      animate = TRUE),
      br(),
      plotOutput("barplots", height = "500px")
    ),
    mainPanel(
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
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      fitBounds(lng1 = -79.5, lat1 = 44.5, lng2 = -56.5, lat2 = 63)
  })
  
  # Met à jour les polygones affichés
  observe({
    leafletProxy("carte") %>%
      clearShapes() %>%
      addPolygons(
        data = feux_filtrés(),
        color = NA,
        fillColor = "#8B0000",
        fillOpacity = 0.75,
        weight = 0
      )
  })
  
  # Graphiques
  output$barplots <- renderPlot({
    req(input$periode)
    
    toutes_periodes <- periodes
    
    get_region_data <- function(region_name) {
      region_data <- aggr_data %>%
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
