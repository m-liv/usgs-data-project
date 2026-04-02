#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(leaflet)
library(readr)
library(lubridate)
library(htmltools)

# Load data
quakes <- read_csv("usgs_sampled2.csv", show_col_types = FALSE) %>%
  mutate(
    time = ymd_hms(time, quiet = TRUE),
    year = year(time),
    place = if_else(is.na(place) | place == "", "Unknown location", place)
  )

min_year <- min(quakes$year, na.rm = TRUE)
max_year <- max(quakes$year, na.rm = TRUE)

ui <- fluidPage(
  titlePanel("USGS Earthquakes Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "selected_year",
        label = "Choose a year:",
        min = min_year,
        max = max_year,
        value = min_year,
        step = 1,
        sep = ""
      ),
      p("Hover over a point to view earthquake details.")
    ),
    mainPanel(
      leafletOutput("quake_map", height = 650)
    )
  )
)

server <- function(input, output, session) {
  
  filtered_quakes <- reactive({
    quakes %>% filter(year == input$selected_year)
  })
  
  output$quake_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4)
  })
  
  observe({
    dat <- filtered_quakes()
    
    pal <- colorNumeric(
      palette = c("#2c7bb6", "#abd9e9", "#fdae61", "#d7191c"),
      domain = quakes$mag,
      na.color = "#808080"
    )
    
    leafletProxy("quake_map", data = dat) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~pmax(3, mag * 2),
        stroke = TRUE,
        weight = 1,
        color = "white",
        fillColor = ~pal(mag),
        fillOpacity = 0.75,
        popup = ~HTML(paste0(
          "<b>", place, "</b><br>",
          "<b>Time:</b> ", format(time, "%Y-%m-%d %H:%M:%S"), " UTC<br>",
          "<b>Magnitude:</b> ", round(mag, 2), "<br>",
          "<b>Depth:</b> ", round(depth, 2), " km<br>",
          "<b>Type:</b> ", type
        )),
        label = ~lapply(
          paste0(
            "Location: ", place, "\n",
            "Magnitude: ", round(mag, 2), "\n",
            "Depth: ", round(depth, 2), " km"
          ),
          HTML
        ),
        labelOptions = labelOptions(
          direction = "auto",
          textsize = "13px",
          style = list(
            "background-color" = "rgba(255,255,255,0.9)",
            "padding" = "6px 8px",
            "border-color" = "#999"
          )
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = quakes$mag,
        title = "Magnitude",
        opacity = 0.8
      )
  })
}

shinyApp(ui, server)
