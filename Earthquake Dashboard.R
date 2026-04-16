library(shiny)
library(dplyr)
library(leaflet)
library(readr)
library(lubridate)
library(htmltools)
library(ggplot2)
library(tidyr)
library(shinythemes)

# Load data
quakes <- read_csv("usgs_sampled2.csv", show_col_types = FALSE) %>%
  mutate(
    time = lubridate::parse_date_time(time, orders = c("Ymd HMS", "Y-m-d H:M:S")),
    year = year(time),
    place = if_else(is.na(place) | place == "", "Unknown location", place),
    continent = case_when(
      latitude >= -60 & latitude <= 15  & longitude >= -85  & longitude <= -30 ~ "South America",
      latitude >= -60 & latitude <= 90 & longitude >= -170 & longitude <= -30 ~ "North America",
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
min_mag <- floor(min(quakes$mag, na.rm = TRUE))
max_mag <- ceiling(max(quakes$mag, na.rm = TRUE))

continent_bounds <- list(
  "North America" = list(lng1 = -170, lat1 = 5,   lng2 = -30, lat2 = 75),
  "South America" = list(lng1 = -85,  lat1 = -60, lng2 = -30, lat2 = 15),
  "Europe"        = list(lng1 = -10,  lat1 = 35,  lng2 = 40,  lat2 = 70),
  "Africa"        = list(lng1 = -20,  lat1 = -35, lng2 = 55,  lat2 = 35),
  "Asia"          = list(lng1 = 40,   lat1 = 5,   lng2 = 180, lat2 = 80),
  "Oceania"       = list(lng1 = 110,  lat1 = -50, lng2 = 180, lat2 = 0), 
  "Other / Ocean" = list(lng1 = -30,  lat1 = -50, lng2 = 180, lat2 = 0)
  
)

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

continents <- sort(unique(quakes$continent))
continents <- c(setdiff(continents, "Other / Ocean"), "Other / Ocean")

ui <- fluidPage(
  theme = shinytheme("readable"), #adding theme, can remove or swtich to another theme
  titlePanel("USGS Earthquakes Explorer"),
  
  tags$style(HTML("
  table {
    font-size: 13px;
  }
")),
  
  tags$style(HTML("
  h1, h2, h3 {
    font-family: Verdana;
    font-weight: 600;
    font-size: 30px;
    text-align: center;
  }
")),
  
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
      
      sliderInput(
        inputId = "selected_mag",
        label = "Select maximum magnitude:",
        min = min_mag,
        max = max_mag,
        value = max_mag,
        step = 0.1
      ),
      
      selectInput(
        inputId = "continent",
        label = "Select continent:",
        choices = c("All", continents),
        selected = "All"
      ),
      
      conditionalPanel(
        condition = "input.continent != 'All'",
        actionButton(
          "clear_continent",
          "Clear continent filter",
          class = "btn-sm btn-outline-secondary",
          style = "margin-top: 4px;"
        )
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
      
      p("Hover over a point to view earthquake details."),
      
      h4("Summary Statistics"),
      verbatimTextOutput("summary_stats"),
      
      h4("Local Risk Insight"),
      verbatimTextOutput("city_stats")
    ),
    
    mainPanel(
      width = 9,
      
      leafletOutput("quake_map", height = 400),
      br(),
      
      tabsetPanel(
        
        tabPanel(
          "Scatter Plot",
          
          radioButtons(
            "scatter_mag_filter",
            "Filter by Magnitude Category:",
            choices = c(
              "All" = "All",
              "Low" = "Low (<3)",
              "Medium" = "Medium (3-5)",
              "High" = "High (>=5)"
            ),
            selected = "All",
            inline = TRUE
          ),
          
          plotOutput("scatter", height = 300)
        ),
        
        tabPanel(
          "Depth And Magnitude Histogram",
          
          radioButtons(
            "hist_var",
            "",
            choices = c("Depth" = "depth", "Magnitude" = "mag"),
            inline = TRUE
          ),
          
          plotOutput("hist_depth", height = 300)
        ),
        
        tabPanel(
          "Top Earthquakes",
          
          radioButtons(
            "show_table",
            "",
            choices = c("Show Info Table" = "yes", "Hide Info Table" = "no"),
            selected = "yes",
            inline = TRUE
          ),
          
          uiOutput("top_panel")
        )
      )
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
  
  observeEvent(input$clear_continent, {
    updateSelectInput(session, "continent", selected = "All")
  })
  
  #zoom to continent
  observeEvent(input$continent, {
        if (input$continent == "All") {
      leafletProxy("quake_map") %>%
        setView(lng = -98.5, lat = 39.8, zoom = 4)
      
    } else if (input$continent %in% names(continent_bounds)) {
      
      b <- continent_bounds[[input$continent]]
      leafletProxy("quake_map") %>%
        fitBounds(
          lng1 = b$lng1,
          lat1 = b$lat1,
          lng2 = b$lng2,
          lat2 = b$lat2
        )
    }
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
  
  # Map filter: year, continent, max magnitude, and city radius
  filtered_quakes <- reactive({
    df <- year_continent_quakes() %>%
      filter(mag <= input$selected_mag)
    
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
    if (!is.null(input$selected_city) && input$selected_city != "") {
      df <- filtered_quakes()
    } else {
      df <- year_continent_quakes()
    }
    
    if (input$scatter_mag_filter != "All") {
      df <- df %>% filter(mag_cat == input$scatter_mag_filter)
    }
    
    df
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
  }, ignoreInit = TRUE)
  
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
        popup = ~lapply(
          paste0(
            "<b>", place, "</b><br>",
            "<b>Time:</b> ", format(time, "%Y-%m-%d %H:%M:%S"), " UTC<br>",
            "<b>Magnitude:</b> ", round(mag, 2), "<br>",
            "<b>Depth:</b> ", round(depth, 2), " km<br>",
            "<b>Continent:</b> ", continent, "<br>"
          ),
          HTML
        ),
        label = ~lapply(
          paste0(
            "<b>", place, "</b><br>",
            "<b>Time:</b> ", format(time, "%Y-%m-%d %H:%M:%S"), " UTC<br>",
            "<b>Magnitude:</b> ", round(mag, 2), "<br>",
            "<b>Depth:</b> ", round(depth, 2), " km<br>",
            "<b>Continent:</b> ", continent, "<br>"
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
  
  top_quakes <- reactive({
    
    # Case 1: city selected → use radius-filtered data
    if (!is.null(input$selected_city) && input$selected_city != "") {
      df <- filtered_quakes()
    } else {
      # Case 2: no city → use continent/year filter only
      df <- year_continent_quakes()
    }
    
    df %>%
      arrange(desc(mag)) %>%
      slice_head(n = 5)
  })
  
  output$top_quakes_table <- renderTable({
    top_quakes() %>%
      select(place, mag, depth, continent) %>%
      rename(
        Place = place,
        Magnitude = mag,
        Depth = depth,
        Continent = continent
      )
  })
  
# side by side table 
  output$top_panel <- renderUI({
    
    if (input$show_table == "yes") {
      
      fluidRow(
        column(
          6,
          plotOutput("top_quakes_plot", height = 300)
        ),
        column(
          6,
          div(
            style = "overflow-y:auto; height:300px;",
            tableOutput("top_quakes_table")
          )
        )
      )
      
    } else {
      
      fluidRow(
        column(
          12,
          plotOutput("top_quakes_plot", height = 300)
        )
      )
      
    }
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
        title = if (input$continent == "All") {
          paste("Earthquake Depth vs Magnitude in", input$selected_year)
        } else {
          paste("Earthquake Depth vs Magnitude in", input$continent, "-", input$selected_year)
        }
      ) +
      theme_minimal() + theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold",
          size = 15
        ),
        axis.title.x = element_text(face = "bold", size = 11),
        axis.title.y = element_text(face = "bold", size = 11), 
        axis.text.x = element_text(color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        legend.title = element_text(size = 12, face = "bold", color = "black"),
        legend.text = element_text(size = 11, color = "black")
      )
  })
  
  output$summary_stats <- renderText({
    data <- filtered_quakes()
    
    paste0(
      "Earthquakes shown: ", nrow(data), "\n",
      "Average magnitude: ", round(mean(data$mag, na.rm = TRUE), 2), "\n",
      "Max magnitude: ", round(max(data$mag, na.rm = TRUE), 2)
    )
  })
  output$city_stats <- renderText({
    data <- filtered_quakes()
    
    if (is.null(input$selected_city) || input$selected_city == "") {
      return("Select a city to view local earthquake risk.")
    }
    
    total <- nrow(data)
    
    paste0(
      "Earthquakes within 50 miles: ", total, "\n",
      ifelse(total > 0,
             paste0("Strongest nearby magnitude: ", round(max(data$mag), 2)),
             "No recent nearby earthquakes")
    )
  })
  
  #adjusting bar graph of magnitudes 
  output$top_quakes_plot <- renderPlot({
    df <- top_quakes()
    
    ggplot(df, aes(x = reorder(place, mag), y = mag)) +
      geom_col(fill = "darkred") +
      coord_flip() +
      labs(
        title = ifelse(
          input$selected_city != "",
          paste("Top 5 Strongest Earthquakes within", RADIUS_MILES, "miles"),
          "Top 5 Strongest Earthquakes"
        ),
        x = "Location",
        y = "Magnitude"
      ) +
      theme_minimal() + 
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold",
          size = 15
        ),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10), 
        axis.text.x = element_text(color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11)
      )
  })
  

  
  #depth histogram
  
  output$hist_depth <- renderPlot({
    data <- scatter_quakes()
    
    if(input$hist_var == "depth"){
      ggplot(data, aes(x = depth)) +
        geom_histogram(bins = 8, fill = "steelblue", color = "white") +
        labs(
          x = "Depth (km)",
          y = "Count",
          title = paste("Distribution of Earthquake Depths in", input$selected_year)
        ) +
        theme_minimal() + theme(
          plot.title = element_text(
            hjust = 0.5,
            face = "bold",
            size = 15
          ),
          axis.title.x = element_text(face = "bold", size = 11),
          axis.title.y = element_text(face = "bold", size = 11), 
          axis.text.x = element_text(color = "black", size = 11),
          axis.text.y = element_text(color = "black", size = 11)
        )
    }
    else {
      ggplot(data, aes(x = mag)) +
        geom_histogram(bins = 8, fill = "darkmagenta", color = "white") +
        labs(
          x = "Magnitude",
          y = "Count",
          title = paste("Distribution of Earthquake Magnitudes in", input$selected_year)
        ) + 
        theme_minimal() + 
        theme(
          plot.title = element_text(
            hjust = 0.5,
            face = "bold",
            size = 15
          ),
          axis.title.x = element_text(face = "bold", size = 10),
          axis.title.y = element_text(face = "bold", size = 10), 
          axis.text.x = element_text(color = "black", size = 11),
          axis.text.y = element_text(color = "black", size = 11)
        )
    }
  })
}

shinyApp(ui, server)
