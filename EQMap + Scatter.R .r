library(dplyr)
library(readr)
library(lubridate)
library(shiny)
library(leaflet)
library(ggplot2)


df <- read_csv("~/Desktop/ds 2003/usgs_sampled2.csv")
print(head(df))
df <- df %>%
  mutate(
    time = ymd_hms(time),
    year = year(time)
  ) %>%
  mutate(
    continent = case_when(
      latitude >= -60 & latitude <= 90 & longitude >= -170 & longitude <= -30 ~ "North America",
      latitude >= -60 & latitude <= 15  & longitude >= -85  & longitude <= -30 ~ "South America",
      latitude >= 35  & latitude <= 70  & longitude >= -10  & longitude <= 40  ~ "Europe",
      latitude >= -35 & latitude <= 35  & longitude >= -20  & longitude <= 55  ~ "Africa",
      latitude >= 5   & latitude <= 80  & longitude >= 40   & longitude <= 180 ~ "Asia",
      latitude >= -50 & latitude <= 0   & longitude >= 110  & longitude <= 180 ~ "Oceania",
      TRUE ~ "Other / Ocean"
    )
  )

ui <- fluidPage(
  titlePanel("Earthquake Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("continent", "Select Continent",
                  choices = c("All", sort(unique(df$continent))), selected = "All"),
      sliderInput("year", "Select Year",
                  min = min(df$year), max = max(df$year),
                  value = min(df$year), step = 1,
                  animate = animationOptions(interval = 800, loop = FALSE))
    ),
    
    #formatting, might not be applicable after merging code
    mainPanel(
      width = 9,
      leafletOutput("map", height = "350px", width = "75%"),
      plotOutput("scatter", height = "350px")
    )
  )
)
server <- function(input, output, session) {
  
  df <- df %>%
    mutate(
      mag_cat = case_when(
        mag < 3 ~ "Low (<3)",
        mag >= 3 & mag < 5 ~ "Medium (3-5)",
        mag >= 5 ~ "High (>=5)",
        TRUE ~ "Unknown"
      )
    )

 #include all data points rather than just continent
   filteredData <- reactive({
    data <- df
    if (input$continent != "All") {
      data <- data %>% filter(continent == input$continent)
    }
    
    data <- data %>% filter(year == input$year)
    data %>% mutate(id = row_number())
    
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      worldCopyJump = TRUE,  
      minZoom = 2,
      maxZoom = 10
    )) %>%
      addProviderTiles("CartoDB.DarkMatter") %>%  #black map
      setView(lng = -98, lat = 39, zoom = 4) # set at USA
  })
  
  
  # scatter plot
  output$scatter <- renderPlot({
    data <- filteredData() 
    
    ggplot(data, aes(x = depth, y = mag, color = mag_cat)) +
      geom_point(alpha = 0.7, size = 2) +
      
      scale_color_manual(values = c(
        "Low (<3)" = "blue",
        "Medium (3-5)" = "green",
        "High (>=5)" = "red"
      )) +
      
      labs(
        x = "Depth (km)",
        y = "Magnitude",
        color = "Magnitude Category",
        title = "Earthquake Depth vs Magnitude"
      ) +
      theme_minimal()
  })
  
  observe({
    data <- filteredData()
    pal <- colorNumeric("YlOrRd", domain = data$mag)
    leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~id,
        radius = ~mag * 2,
        fillColor = ~pal(mag),
        fillOpacity = 0.7,
        color = "white",
        weight = 0.5
      ) 
  
  })

}
shinyApp(ui, server)