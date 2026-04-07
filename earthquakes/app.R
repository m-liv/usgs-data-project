library(shiny)
library(dplyr)
library(leaflet)
library(readr)
library(lubridate)
library(htmltools)
library(ggplot2)
library(tidyr)

# Load data
quakes <- read_csv("usgs_sampled2.csv", show_col_types = FALSE) %>%
  mutate(
    time = ymd_hms(time, quiet = TRUE),
    year = year(time),
    place = if_else(is.na(place) | place == "", "Unknown location", place),
    continent = case_when(
      latitude >= -60 & latitude <= 90 & longitude >= -170 & longitude <= -30 ~ "North America",
      latitude >= -60 & latitude <= 15  & longitude >= -85  & longitude <= -30 ~ "South America",
      latitude >= 35  & latitude <= 70  & longitude >= -10  & longitude <= 40  ~ "Europe",
      latitude >= -35 & latitude <= 35  & longitude >= -20  & longitude <= 55  ~ "Africa",
      latitude >= 5   & latitude <= 80  & longitude >= 40   & longitude <= 180 ~ "Asia",
      latitude >= -50 & latitude <= 0   & longitude >= 110  & longitude <= 180 ~ "Oceania",
      TRUE ~ "Other / Ocean"
    ),
    mag_cat = case_when(
      mag < 3 ~ "Low (<3)",
      mag >= 3 & mag < 5 ~ "Medium (3-5)",
      mag >= 5 ~ "High (>=5)",
      TRUE ~ "Unknown"
    )
  ) %>%
  drop_na(mag, depth, latitude, longitude)

min_year <- min(quakes$year, na.rm = TRUE)
max_year <- max(quakes$year, na.rm = TRUE)

# Load cities data
cities <- read_csv("worldcities.csv", show_col_types = FALSE) %>%
  filter(!is.na(lat), !is.na(lng)) %>%
  mutate(
    city_name = coalesce(city_ascii, city),
    lat = as.numeric(lat),
    lng = as.numeric(lng),
    label = paste0(city_name, ", ", country)
  ) %>%
  distinct(label, .keep_all = TRUE) %>%
  arrange(label)

# Haversine formula for distance in miles
haversine_miles <- function(lat1, lng1, lat2, lng2) {
  R <- 3958.8
  phi1 <- lat1 * pi / 180
  phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlambda <- (lng2 - lng1) * pi / 180
  a <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlambda / 2)^2
  2 * R * asin(sqrt(a))
}

RADIUS_MILES <- 50

default_city <- "Charlottesville, United States"


ui <- fluidPage(
  titlePanel("USGS Earthquakes Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput(
        inputId = "selected_year",
        label = "Select year:",
        min = min_year,
        max = max_year,
        value = min_year,
        step = 1,
        sep = "",
        animate = animationOptions(interval = 800, loop = FALSE)
      ),
      selectInput(
        inputId = "continent",
        label = "Select continent:",
        choices = c("All", sort(unique(quakes$continent))),
        selected = "All"
      ),
      selectizeInput(
        inputId = "selected_city",
        label = "Select city:",
        choices = c("None (show all)" = "", setNames(cities$label, cities$label)),
        selected = "",
        options = list(
          placeholder = "Search for a city...",
          maxOptions = 50
        )
      ),
      conditionalPanel(
        condition = "input.selected_city != ''",
        actionButton(
          "clear_city",
          "Clear city filter",
          class = "btn-sm btn-outline-secondary",
          style = "margin-top: 4px;"
        )
      ),
      p("Hover over a point to view earthquake details.")
    ),
    mainPanel(
      width = 9,
      leafletOutput("quake_map", height = 400),
      br(),
      plotOutput("scatter", height = 300)
    )
  )
)

server <- function(input, output, session) {
  
  selected_city_data <- reactive({
    req(input$selected_city)
    req(input$selected_city != "")
    cities %>% filter(label == input$selected_city) %>% slice(1)
  })
  
  observeEvent(input$clear_city, {
    updateSelectizeInput(session, "selected_city", selected = "")
  })
  
  # Shared filter for year and continent
  year_continent_quakes <- reactive({
    df <- quakes %>%
      filter(year == input$selected_year)
    
    if (input$continent != "All") {
      df <- df %>% filter(continent == input$continent)
    }
    
    df
  })
  
  # Map filter: year, continent, and city radius
  filtered_quakes <- reactive({
    df <- year_continent_quakes()
    
    if (!is.null(input$selected_city) && input$selected_city != "") {
      city_row <- selected_city_data()
      df <- df %>%
        filter(
          haversine_miles(city_row$lat, city_row$lng, latitude, longitude) <= RADIUS_MILES
        )
    }
    
    df %>% mutate(id = row_number())
  })
  
  # Scatterplot filter: year and continent 
  scatter_quakes <- reactive({
    year_continent_quakes()
  })
  
  output$quake_map <- renderLeaflet({
    leaflet(options = leafletOptions(
      worldCopyJump = TRUE,
      minZoom = 2,
      maxZoom = 10
    )) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4)
  })
  
  observeEvent(input$selected_city, {
    if (!is.null(input$selected_city) && input$selected_city != "") {
      city_row <- selected_city_data()
      
      leafletProxy("quake_map") %>%
        flyTo(
          lng = city_row$lng,
          lat = city_row$lat,
          zoom = 8
        )
    }
  }, ignoreInit = FALSE)
  
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
    
    if (!is.null(input$selected_city) && input$selected_city != "") {
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
        layerId = ~id,
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
          "<b>Continent:</b> ", continent, "<br>",
          "<b>Type:</b> ", type
        )),
        label = ~lapply(
          paste0(
            "Location: ", place, "<br>",
            "Magnitude: ", round(mag, 2), "<br>",
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
  
  output$scatter <- renderPlot({
    data <- scatter_quakes()
    
    ggplot(data, aes(x = depth, y = mag, color = mag_cat)) +
      geom_point(alpha = 0.7, size = 2) +
      scale_color_manual(values = c(
        "Low (<3)" = "blue",
        "Medium (3-5)" = "green",
        "High (>=5)" = "red",
        "Unknown" = "gray"
      )) +
      labs(
        x = "Depth (km)",
        y = "Magnitude",
        color = "Magnitude Category",
        title = paste("Earthquake Depth vs Magnitude in", input$selected_year)
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)