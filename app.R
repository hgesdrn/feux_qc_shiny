
library(bslib)
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
url <- "https://raw.githubusercontent.com/hgesdrn/feux_qc_shiny/main/data/feux_aggr.RDS"
con <- gzcon(url(url))
on.exit(close(con))
aggr_data <- readRDS(con)


# Chargement de la couche de province sans le Saguenay
url_prov <- "https://github.com/hgesdrn/feux_qc_shiny/blob/main/data/prov_sf.rds?raw=true"
prov_sf <- readRDS(gzcon(url(url_prov)))

# UI
ui <- fluidPage(
  titlePanel("Feux de forêt au Québec de 1900 à 2023"),
  tags$style(HTML("
    .irs-grid-text, .irs-single, .irs-bar, .irs-min, .irs-max, .irs-from, .irs-to {
      font-size: 11px !important;
    }
    .irs-slider {
      height: 16px !important;
    }
  ")),
  fluidRow(
    column(6,
           # Panneau gauche dans un encadré stylé
           div(style = "height:700px; overflow-y:auto; padding: 20px; background-color: #f9f9f9;
                        border: 1px solid #ccc; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
               
               # Slider pleine largeur avec animation lente
               sliderTextInput("periode", "Choisir une période :",
                               choices = periodes,
                               selected = periodes[1],
                               grid = TRUE,
                               animate = animationOptions(interval = 1000, loop = FALSE),
                               width = "100%"),
               
               # Séparateur
               tags$hr(style = "border-top: 1px solid #aaa; margin-top: 20px; margin-bottom: 20px;"),
               
               # Graphiques
               plotOutput("barplots", height = "500px")
           )
    ),
    
    column(6,
           div(style = "height:700px; padding: 20px; background-color: #f9f9f9;
                    border: 1px solid #ccc; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
               # h4("Carte des feux de forêt"),
               leafletOutput("carte", height = "640px")
           )
    )
    
  )
)


# Serveur
server <- function(input, output, session) {
  
  # Réactif : charge les polygones de la période sélectionnée
  feux_filtrés <- reactive({
    req(input$periode)
    url <- paste0("https://raw.githubusercontent.com/hgesdrn/feux_qc_shiny/main/data/periodes/feux_", input$periode, ".rds")
    con <- gzcon(url(url))
    on.exit(close(con))
    readRDS(con)
  })
  
  # Carte de base
  output$carte <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = prov_sf,
                  color = NA,
                  fillColor = "gray70",
                  fillOpacity = 0.4,
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
          axis.text.y = element_text(face = "bold", size = 12),
          axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
          axis.text.x = element_text(angle = 20, hjust = 1, face = "bold", size = 12), # angle = 45, 
          axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 15)),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
        )
      
      if (show_x_labels) {
        p <- p +
          labs(x = "Période") +
          theme(
            axis.text.x = element_text(angle = 20,hjust = 1, face = "bold"), #angle = 45, 
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
    
    p1 <- plot_region(data_saguenay, show_x_labels = FALSE) + ggtitle("Région administrative du Saguenay")
    p2 <- plot_region(data_quebec, show_x_labels = TRUE) + ggtitle("Province du Québec")
    
    ggarrange(p1, p2, ncol = 1, heights = c(1, 1))
  })
}

# Lancer l'application
shinyApp(ui, server)
