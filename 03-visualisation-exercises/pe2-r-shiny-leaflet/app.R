# FIT5147 PE2 Shiny application
# load library
library(ggplot2)
library(dplyr)
library(lubridate)
library(leaflet)
library(shiny)

# 1.2.1 VIS1
# Read dataset 
data <- read.csv("ALA_PE2S12026.csv") 

# Convert data type and define seasons
data$observationDate <- as.Date(data$observationDate)
data$month <- month(data$observationDate)

data$season <- case_when(  
  data$month %in% c(12, 1, 2) ~ "Summer",
  data$month %in% c(3, 4, 5) ~ "Autumn",
  data$month %in% c(6, 7, 8) ~ "Winter",
  data$month %in% c(9, 10, 11) ~ "Spring"
)

# Calculate proportion of each season
season_summary <- data %>%
  count(season) %>%
  mutate(
    proportion = n / sum(n))

season_summary$season <- factor(
  season_summary$season,
  levels = c('Summer','Autumn','Winter','Spring'))

# Plot bar chart
vis1 <- ggplot(season_summary, aes(x =season, y =proportion, fill = season)) +
  geom_col() +
  scale_fill_manual(values = c(
    "Summer" = "gold",
    "Autumn" = "orange",
    "Winter" = "skyblue",
    "Spring" = "seagreen"
  ))+
  labs(title = "Proportion of Bird Observations by Season in Au",
       x = "Season",
       y = "Proportion of observations") +
  theme_minimal()

# 1.2.2 MAP
# Prepare map data
map_data <- data %>%
  group_by(decimalLatitude, decimalLongitude, scientificName,
           vernacularName, stateProvince, season) %>%
  summarise(observation_count = n(), .groups = "drop")

# Create species list
species_choices <- unique(map_data$vernacularName)

# Define colour palette for bird species
pal <- colorFactor(
  palette = c("gold", "orange", "skyblue", "seagreen"),
  domain = map_data$vernacularName
  )

# Create tooltip text for map markers
map_data$tooltip <- paste(
  "Scientific name:", map_data$scientificName,
  "<br>Common name:", map_data$vernacularName,
  "<br>Observations:", map_data$observation_count,
  "<br>State/Territory:", map_data$stateProvince
  )

#1.2.3
ui <- fixedPage(
  
  # Part 1: title
  div(
    h1("Seasonal and Spatial Patterns of Australian Parrot Observations",
       style = "font-size: 23px; font-weight: bold;")
    ),
  
  # Part 2: VIS 1 and description
  fluidRow(
    column(
      width = 6,
      h3("VIS 1",
         style = "font-size: 20px"),
      plotOutput("vis1Plot", height = "300px")
      ),
    column(
      width = 6,
      h3("VIS 1 Description",
         style = "font-size: 20px"),
      p("This static bar chart shows the proportion of bird observations recorded in each Australian season. Using proportion rather than raw counts allows a fair comparison across summer, autumn, winter and spring. The chart shows that observations are not evenly distributed throughout the year, with spring having the highest proportion and summer the lowest. This may reflect seasonal differences in bird activity, breeding behaviour, visibility, or movement.")
      )
  ),
    br(),
  
  # Part 3: MAP
  fluidRow(
    column(
      width = 3,
      wellPanel(
        h3("Map Filters",
           style = "font-size: 20px"),
        checkboxGroupInput(
          inputId = "species_filter",
          label = "Select bird species:",
          choices = species_choices,
          selected = species_choices
          ),
        radioButtons(
          inputId = "season_filter",
          label = "Select season:",
          choices = c("Full year", "Summer", "Autumn", "Winter", "Spring"),
          selected = "Full year"
          )
        )
      ),
    column(
      width = 9,
      style = "margin-top: -20px;",
      h3("MAP",
         style = "font-size: 20px"),
      leafletOutput("birdMap", height = "430px")
      )
    ),
  br(),
  
  # Part 4: MAP description
  fluidRow(
    column(
      width = 12,
      wellPanel(
        h3("MAP Description",
           style = "font-size: 20px"),
        p("This interactive map shows the spatial distribution of parrot observations across Australia. Circle colour represents species and circle size represents the number of observations at each location. Use the checklist to filter bird species and the radio buttons to compare the full year with individual seasons. Hover over each circle to view the scientific name, common name, observation count, and state or territory. The map highlights strong clustering along eastern and south-eastern Australia, and the seasonal filter helps reveal changes in observation locations that suggest seasonal movement or migration patterns for some species.")
        )
      )
    ),
  br(),
  
  # Part 5: Original data source
  fluidRow(
    column(
      width = 12,
      wellPanel(
        h4("Original Data Source",
           style = "font-size: 20px"),
        p("Data name: Atlas of Living Australia (ALA) species observation data"),
        p("URL: http://www.ala.org.au and https://spatial.ala.org.au/"),
        p("Licensor: Atlas of Living Australia"),
        p("Original data version used for this visualisation: 24 March 2026")
        )
      )
    ),
  )


server <- function(input, output, session) {
  
  # Render VIS 1
  output$vis1Plot <- renderPlot({
    vis1
  })
  
  # Filter map data by selected species and season
  filtered_map_data <- reactive({
    
    species_filtered <- map_data %>%
      filter(vernacularName %in% input$species_filter)
    
    if (input$season_filter == "Full year") {
      species_filtered
    } else {
      species_filtered %>%
        filter(season == input$season_filter)
    }
  })
  
  # Render interactive map
  output$birdMap <- renderLeaflet({
    
    current_data <- filtered_map_data()
    
    leaflet(current_data) %>%
      addTiles() %>%
      setView(lng = 133.9453, lat = -30.4486, zoom = 3) %>%
      addCircleMarkers(
        lng = ~decimalLongitude,
        lat = ~decimalLatitude,
        color = ~pal(vernacularName),
        radius = ~sqrt(observation_count),
        fillOpacity = 0.6,
        stroke = FALSE,
        label = ~paste0(
          "Scientific name: ", scientificName,
          "<br>Common name: ", vernacularName,
          "<br>Observations: ", observation_count,
          "<br>State/Territory: ", stateProvince
        ),
        labelOptions = labelOptions(direction = "auto")
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = species_choices,
        title = "Species",
        opacity = 1
      )
  })
}

shinyApp(ui = ui, server = server)

