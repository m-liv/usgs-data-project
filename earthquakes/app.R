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

# Load cities data
cities <- read_csv("worldcities.csv", show_col_types = FALSE) %>%
  filter(!is.na(lat), !is.na(lng), !is.na(city_ascii)) %>%
  mutate(
    lat = as.numeric(lat),
    lng = as.numeric(lng),
    label = paste0(city_ascii, ", ", country)
  ) %>%
  arrange(label)

# Use haversine formula to calculate distance in miles between coordinates
haversine_miles <- function(lat1, lng1, lat2, lng2) {
  R <- 3958.8  # Earth radius in miles
  phi1 <- lat1 * pi / 180
  phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlambda <- (lng2 - lng1) * pi / 180
  a <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlambda / 2)^2
  2 * R * asin(sqrt(a))
}

RADIUS_MILES <- 50

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
      hr(),
      selectizeInput(
        inputId = "selected_city",
        label = "Filter by city (50-mile radius):",
        choices = c("None (show all)" = "", setNames(cities$label, cities$label)),
        selected = "",
        options = list(
          placeholder = "Search for a city...",
          maxOptions = 50
        )
      ),
      conditionalPanel(
        condition = "input.selected_city != ''",
        actionButton("clear_city", "Clear city filter", class = "btn-sm btn-outline-secondary",
                     style = "margin-top: 4px;")
      ),
      hr(),
      p("Hover over a point to view earthquake details.")
    ),
    mainPanel(
      leafletOutput("quake_map", height = 650)
    )
  )
)

server <- function(input, output, session) {
  
  # Look up selected city
  selected_city_data <- reactive({
    req(input$selected_city != "")
    cities %>% filter(label == input$selected_city) %>% slice(1)
  })
  
  # Clear city when button is pressed
  observeEvent(input$clear_city, {
    updateSelectizeInput(session, "selected_city", selected = "")
  })
  
  filtered_quakes <- reactive({
    df <- quakes %>% filter(year == input$selected_year)
    
    if (input$selected_city != "") {
      city_row <- selected_city_data()
      df <- df %>%
        filter(
          haversine_miles(city_row$lat, city_row$lng, latitude, longitude) <= RADIUS_MILES
        )
    }
    
    df
  })
  
  output$quake_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4)
  })
  
  # Zoom the map into the newly selected city
  observeEvent(input$selected_city, {
    if (input$selected_city != "") {
      city_row <- selected_city_data()
      leafletProxy("quake_map") %>%
        flyTo(lng = city_row$lng, lat = city_row$lat, zoom = 8)
    }
  })
  
  observe({
    dat <- filtered_quakes()
    
    pal <- colorNumeric(
      palette = c("#2c7bb6", "#abd9e9", "#fdae61", "#d7191c"),
      domain = quakes$mag,
      na.color = "#808080"
    )
    
    proxy <- leafletProxy("quake_map", data = dat) %>%
      clearMarkers() %>%
      clearControls() %>%
      clearShapes()
    
    # 50-mile radius circle for selected city
    if (input$selected_city != "") {
      city_row <- selected_city_data()
      radius_meters <- RADIUS_MILES * 1609.34
      proxy <- proxy %>%
        addCircles(
          lng = city_row$lng,
          lat = city_row$lat,
          radius = radius_meters,
          color = "#444",
          weight = 2,
          fill = TRUE,
          fillColor = "#888",
          fillOpacity = 0.07,
          dashArray = "6 4",
          options = pathOptions(interactive = FALSE)
        ) %>%
        addCircleMarkers(
          lng = city_row$lng,
          lat = city_row$lat,
          radius = 6,
          color = "#222",
          fill = TRUE,
          fillColor = "#f0c040",
          fillOpacity = 1,
          weight = 2,
          label = city_row$label,
          options = markerOptions(zIndexOffset = 1000)
        )
    }
    
    proxy %>%
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
