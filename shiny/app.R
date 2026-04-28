library(shiny)
library(dplyr)
library(leaflet)
library(readr)
library(lubridate)
library(htmltools)
library(ggplot2)
library(tidyr)
library(bslib)
library(bsicons)

get_continent <- function(lat, lon) {
  case_when(
    lat >= -60 & lat <= 15  & lon >= -85  & lon <= -30 ~ "South America",
    lat >= -60 & lat <= 90  & lon >= -170 & lon <= -30 ~ "North America",
    lat >= 35  & lat <= 70  & lon >= -10  & lon <= 40  ~ "Europe",
    lat >= -35 & lat <= 35  & lon >= -20  & lon <= 55  ~ "Africa",
    lat >= 5   & lat <= 80  & lon >= 40   & lon <= 180 ~ "Asia",
    lat >= -50 & lat <= 0   & lon >= 110  & lon <= 180 ~ "Oceania",
    TRUE ~ "Other / Ocean"
  )
}

# Load data
quakes <- read_csv("data/usgs_sampled2.csv", show_col_types = FALSE) %>%
  mutate(
    time = lubridate::parse_date_time(time, orders = c("Ymd HMS", "Y-m-d H:M:S")),
    year = year(time),
    place = if_else(is.na(place) | place == "", "Unknown location", place),
    continent = get_continent(latitude, longitude),
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

# Load cities data
cities <- read_csv("data/simplemaps_worldcities_basicv1.901/worldcities.csv", show_col_types = FALSE) %>%
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

RADIUS_MILES <- 100

continents <- sort(unique(quakes$continent))
continents <- c(setdiff(continents, "Other / Ocean"), "Other / Ocean")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .well {
        padding-top: 18px;
        padding-bottom: 18px;
        background-color: #f8f9fa;
        border-radius: 14px;
        border: 1px solid #e3e6ea;
      }

      .form-group {
        margin-bottom: 18px;
      }

      .control-label {
        font-weight: 700;
      }

      .sidebar-section {
        margin-bottom: 22px;
      }

      .sidebar-help {
        color: #555;
        font-size: 13px;
        margin-top: 4px;
        margin-bottom: 0;
      }

      .stats-card {
        background: white;
        border: 1px solid #e5e7eb;
        border-radius: 14px;
        padding: 14px 16px;
        margin-bottom: 16px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.06);
      }

      .stats-card-title {
        font-weight: 700;
        font-size: 16px;
        margin-bottom: 10px;
        display: flex;
        align-items: center;
        gap: 8px;
      }

      .tab-content {
        padding-top: 14px;
      }

      .shiny-output-error-validation {
        color: #666;
      }

      pre {
        background-color: transparent;
        border: none;
        padding: 0;
        margin: 0;
        font-size: 13px;
        white-space: pre-wrap;
      }
    "))
  ),
  titlePanel("USGS Earthquakes Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      div(
        class = "sidebar-section",
        sliderInput(
          inputId = "selected_year",
          label = "Select year:",
          min = min_year,
          max = max_year,
          value = min_year,
          step = 1,
          sep = "",
          animate = animationOptions(interval = 800, loop = FALSE)
        )
      ),
      
      div(
        class = "sidebar-section",
        sliderInput(
          inputId = "selected_mag",
          label = "Filter by magnitude:",
          min = min_mag,
          max = max_mag,
          value = c(min_mag, max_mag),
          step = 0.1
        )
      ),
      
      div(
        class = "sidebar-section",
        selectInput(
          inputId = "continent",
          label = "Select continent:",
          choices = c("All", continents),
          selected = "All"
        )
      ),
      
      div(
        class = "sidebar-section",
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
        )
      ),
      
      div(
        class = "sidebar-section",
        p("Hover over a point to view earthquake details.", class = "sidebar-help")
      ),
      
      div(
        class = "stats-card",
        div(class = "stats-card-title", "Summary Statistics"),
        verbatimTextOutput("summary_stats")
      ),
      
      div(
        class = "stats-card",
        div(
          class = "stats-card-title",
          "Local Risk Insight",
          tooltip(
            bs_icon("info-circle"),
            "Shows the number of earthquakes within 50 miles of the selected city and the strongest nearby magnitude.",
            placement = "right"
          )
        ),
        verbatimTextOutput("city_stats")
      )
    ),
    mainPanel(
      width = 9,
      leafletOutput("quake_map", height = 400),
      br(),
      tabsetPanel(
        tabPanel(
          "Depth vs. Magnitude",
          plotOutput("scatter", height = 300)
        ),
        tabPanel(
          "Depth and Magnitude Distributions",
          radioButtons(
            "hist_var",
            "",
            choices = c("Depth" = "depth", "Magnitude" = "mag")
          ),
          plotOutput("hist_depth", height = 300)
        ),
        tabPanel(
          "Top Earthquakes by Magnitude",
          br(),
          conditionalPanel(
            condition = "input.selected_city != ''",
            uiOutput("top_quakes_message")
          ),
          plotOutput("top_quakes_plot", height = 300),
          br(),
          tableOutput("top_quakes_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  plot_title <- function(base) {
    if (input$continent == "All") {
      paste0(base, " (", input$selected_year, ")")
    } else {
      paste0(base, " in ", input$continent, " (", input$selected_year, ")")
    }
  }
  
  plot_style <- theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 15, margin = margin(b = 10)),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      legend.position = "right"
    )
  
  colors <- list(
    primary = "#2c7bb6",   # blue
    secondary = "#abd9e9", # light blue
    accent = "#fdae61",    # orange
    highlight = "#d7191c", # red
    neutral = "#6c757d"    # gray
  )
  
  mag_colors <- c(
    "Low (<3)" = colors$primary,
    "Medium (3-5)" = colors$accent,
    "High (>=5)" = colors$highlight,
    "Unknown" = colors$neutral
  )
  
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
  
  # Map filter: year, continent, max magnitude, and city radius
  filtered_quakes <- reactive({
    df <- year_continent_quakes() %>%
      filter(
        mag >= input$selected_mag[1],
        mag <= input$selected_mag[2]
      )
    
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
    if (!is.null(input$selected_city) && input$selected_city != "") {
      df <- filtered_quakes()
    } else {
      df <- year_continent_quakes()
    }
    
    df %>%
      arrange(desc(mag)) %>%
      slice_head(n = 5)
  })
  
  output$top_quakes_table <- renderTable({
    df <- top_quakes()
    
    if (!is.null(input$selected_city) && input$selected_city != "" && nrow(df) == 0) {
      return(NULL)
    }
    
    df %>%
      select(
        place,
        mag,
        depth,
        time,
        continent
      )
  })
  
  output$scatter <- renderPlot({
    data <- scatter_quakes()
    
    ggplot(data, aes(x = depth, y = mag, color = mag_cat)) +
      geom_point(alpha = 0.75, size = 2) +
      scale_color_manual(values = mag_colors) +
      labs(
        x = "Depth (km)",
        y = "Magnitude",
        color = "Magnitude Category",
        title = plot_title("Earthquake Depth vs. Magnitude")
      ) +
      plot_style
  })
  
  output$summary_stats <- renderText({
    data <- filtered_quakes()
    
    if (!is.null(input$selected_city) && input$selected_city != "" && nrow(data) == 0) {
      return(paste0(
        "Earthquakes shown: 0\n",
        "Average magnitude: N/A\n",
        "Max magnitude: N/A"
      ))
    }
    
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
      ifelse(
        total > 0,
        paste0("Strongest nearby magnitude: ", round(max(data$mag), 2)),
        "No recent nearby earthquakes"
      )
    )
  })
  
  output$top_quakes_plot <- renderPlot({
    df <- top_quakes()
    
    if (!is.null(input$selected_city) && input$selected_city != "" && nrow(df) == 0) {
      return(NULL)
    }
    
    ggplot(df, aes(x = reorder(place, mag), y = mag)) +
      geom_col(fill = colors$highlight, alpha = 0.9) +
      coord_flip() +
      labs(
        title = if (input$selected_city != "") {
          paste0(
            "Strongest Earthquakes within ", RADIUS_MILES, " miles of ",
            input$selected_city, " (", input$selected_year, ")"
          )
        } else {
          paste0(
            "Strongest Earthquakes",
            if (input$continent == "All") "" else paste(" in", input$continent),
            " (", input$selected_year, ")"
          )
        },
        x = "Location",
        y = "Magnitude"
      ) +
      plot_style
  })
  
  output$top_quakes_message <- renderUI({
    df <- top_quakes()
    
    if (!is.null(input$selected_city) && input$selected_city != "" && nrow(df) == 0) {
      div(
        style = "font-weight: 600; margin-bottom: 12px;",
        "No earthquakes match the criteria."
      )
    }
  })
  
  output$hist_depth <- renderPlot({
    data <- scatter_quakes()
    
    if (input$hist_var == "depth") {
      ggplot(data, aes(x = depth)) +
        geom_histogram(bins = 10, fill = colors$primary, color = "white", alpha = 0.9) +
        labs(
          x = "Depth (km)",
          y = "Count",
          title = plot_title("Distribution of Earthquake Depths")
        ) +
        plot_style
    } else {
      ggplot(data, aes(x = mag)) +
        geom_histogram(bins = 10, fill = colors$accent, color = "white") +
        labs(
          x = "Magnitude",
          y = "Count",
          title = plot_title("Distribution of Earthquake Magnitudes")
        ) +
        plot_style
    }
  })
}

shinyApp(ui, server)