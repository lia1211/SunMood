# ============================================================
# Mental Health Around the World
# DVP Part 2 - Interactive Narrative Visualisation
# Yawen Li - 35429399 - Applied Session 07
# Tutors: Michael Niemann & Ayden Zhou
# ============================================================

library(shiny)
library(tidyverse)
library(countrycode)
library(leaflet)
library(plotly)
library(shinyjs)

# ---- Read raw data ----
happiness_raw <- read.csv("happiness-cantril-ladder.csv", stringsAsFactors = FALSE)
suicide_raw   <- read.csv("death-rate-from-suicides-ghe.csv", stringsAsFactors = FALSE)
solar_2021    <- read.csv("solar_data_2021.csv", stringsAsFactors = FALSE)
centroids     <- read.csv("world_centroids.csv", stringsAsFactors = FALSE)

# ---- Rename columns ----
happiness <- happiness_raw %>%
  rename(country = Entity, code = Code, year = Year,
         satisfaction_score = Self.reported.life.satisfaction)

suicide <- suicide_raw %>%
  rename(country = Entity, code = Code, year = Year,
         suicide_rate = Death.rate.from.self.harm.among.both.sexes)

# ---- Remove rows without country code (regional aggregates) ----
happiness <- happiness %>% filter(!is.na(code), code != "")
suicide   <- suicide   %>% filter(!is.na(code), code != "")

# ---- Keep only common countries and years, then merge ----
common_countries <- intersect(suicide$country, happiness$country)
common_years      <- intersect(suicide$year, happiness$year)

q1_data <- suicide %>%
  filter(country %in% common_countries, year %in% common_years) %>%
  inner_join(
    happiness %>% filter(country %in% common_countries, year %in% common_years),
    by = c("country", "code", "year")
  )

# ---- Add continent from country code ----
q1_data <- q1_data %>%
  mutate(continent = countrycode(country, "country.name", "continent")) %>%
  filter(!is.na(continent))

# ---- Ensure numeric ----
q1_data <- q1_data %>%
  mutate(satisfaction_score = as.numeric(satisfaction_score),
         suicide_rate       = as.numeric(suicide_rate))

# ---- Build Q2 data (2021 + solar radiation) ----
solar_2021 <- solar_2021 %>% select(country, year, annual_solar_radiation)

q2_data <- q1_data %>%
  filter(year == 2021) %>%
  left_join(solar_2021, by = c("country", "year"))

# ---- Add coordinates for the map (join by ISO code) ----
coords <- centroids %>% select(iso_a3, lat, lon)

map_2021 <- q1_data %>%
  filter(year == 2021) %>%
  left_join(coords, by = c("code" = "iso_a3"))

# ---- Continent colour palette (consistent with DEP) ----
continent_colors <- c(
  "Africa"   = "#C97C5D",
  "Americas" = "#6E8FA8",
  "Asia"     = "#56C596",
  "Europe"   = "#F59E0B",
  "Oceania"  = "#B48EAD"
)

# UI
ui <- fluidPage(
  useShinyjs(),
  
tags$head(tags$style(HTML("
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: Georgia, serif;
      background-color: #FFFFFF;
      color: #1A1A2E;
      overflow-x: hidden;
    }

    /* Header */
    #site-header {
      background-color: #FFFFFF;
      border-bottom: 1px solid #E2E8F0;
      padding: 16px 40px;
      position: sticky; top: 0; z-index: 999;
      display: flex; align-items: center; justify-content: space-between;
    }
    #site-header .title-text {
      font-size: 1rem; font-weight: bold;
      letter-spacing: 0.08em; text-transform: uppercase;
      color: #1A1A2E;
    }
    #progress-dots { display: flex; gap: 10px; }
    .dot { width: 10px; height: 10px; border-radius: 50%;
           background: #E2E8F0; transition: background 0.3s; cursor: pointer; }
    .dot.active { background: #3A8FB7; }

    /* Hero */
    #hero {
      min-height: 88vh; display: flex; flex-direction: column;
      justify-content: center; padding: 80px 10%;
      background: #FFFFFF;
    }
    #hero .eyebrow { font-size: 0.8rem; letter-spacing: 0.2em;
      text-transform: uppercase; color: #3A8FB7; margin-bottom: 16px; }
    #hero h1 { font-size: clamp(2rem, 5vw, 3.6rem); font-weight: normal;
      line-height: 1.15; max-width: 720px; margin-bottom: 24px; color: #1A1A2E; }
    #hero .scroll-hint { display: flex; align-items: center; gap: 10px;
      color: #3A8FB7; font-size: 0.85rem; letter-spacing: 0.1em; }
    .arrow-down { width: 0; height: 0;
      border-left: 5px solid transparent; border-right: 5px solid transparent;
      border-top: 8px solid #3A8FB7; animation: bounce 1.5s infinite; }
    @keyframes bounce { 0%,100%{transform:translateY(0);} 50%{transform:translateY(6px);} }

    .questions-list { margin-bottom: 40px; }
    .q-item { font-size: 1rem; color: #4A5568;
      line-height: 1.9; letter-spacing: 0.01em; }
      .hero-inner { display: flex; align-items: center;
      gap: 40px; width: 100%; }
    .hero-left { flex: 0 0 55%; }
    .hero-right { flex: 1; display: flex;
      align-items: center; justify-content: center; }

    .hero-stats { display: flex; gap: 20px; margin-bottom: 32px; }
    .stat-card { display: flex; flex-direction: column;
      align-items: center; background: #EBF4FA;
      border-radius: 8px; padding: 14px 22px;
      border: 1px solid #C8E0EE; }
    .stat-num { font-size: 1.8rem; font-weight: bold;
      color: #3A8FB7; line-height: 1; }
    .stat-label { font-size: 0.75rem; color: #4A5568;
      text-transform: uppercase; letter-spacing: 0.08em;
      margin-top: 4px; }
    .globe-container { display: flex; justify-content: center;
      align-items: center; width: 100%; }
    .globe-svg { max-width: 280px; }
    .pulse-dot { animation: pulse 2s ease-in-out infinite; }
    .pulse-dot:nth-child(1) { animation-delay: 0s; }
    .pulse-dot:nth-child(2) { animation-delay: 0.3s; }
    .pulse-dot:nth-child(3) { animation-delay: 0.6s; }
    .pulse-dot:nth-child(4) { animation-delay: 0.9s; }
    .pulse-dot:nth-child(5) { animation-delay: 1.2s; }
    .pulse-dot:nth-child(6) { animation-delay: 1.5s; }
    .pulse-dot:nth-child(7) { animation-delay: 1.8s; }
    .pulse-dot:nth-child(8) { animation-delay: 0.4s; }
    @keyframes pulse {
      0%, 100% { opacity: 0.3; }
      50% { opacity: 1; }
    }
    .globe-svg ellipse {
      transform-origin: 150px 150px;
      animation: spin 20s linear infinite;
    }
    @keyframes spin {
      from { transform: rotate(0deg); }
      to { transform: rotate(360deg); }
    }
      

    /* Sections */
    .section-block { padding: 80px 0; border-top: 1px solid #E2E8F0; }
    .section-block:nth-child(even) { background: #F5F7FA; }
    .sticky-layout { display: flex; align-items: flex-start;
      max-width: 1200px; margin: 0 auto; padding: 0 40px; gap: 50px; }
    .sticky-text { flex: 0 0 320px; position: sticky; top: 90px; }
    .sticky-text .section-label { font-size: 0.7rem; letter-spacing: 0.2em;
      text-transform: uppercase; color: #3A8FB7; margin-bottom: 12px; }
    .sticky-text h2 { font-size: 1.6rem; font-weight: normal;
      margin-bottom: 18px; line-height: 1.3; color: #1A1A2E; }
    .sticky-text p { font-size: 0.95rem; color: #4A5568;
      line-height: 1.7; margin-bottom: 14px; }
    .finding-box { background: #EBF4FA; border-left: 3px solid #3A8FB7;
      padding: 14px 16px; margin-top: 20px; border-radius: 0 4px 4px 0;
      font-size: 0.88rem; color: #2D3748; line-height: 1.6; }

    .charts-col { flex: 1; min-width: 0; display: flex;
      flex-direction: column; gap: 24px; }
    .chart-card { background: #FFFFFF; border-radius: 8px;
      padding: 18px; border: 1px solid #E2E8F0; }
    .chart-card .chart-title { font-size: 0.78rem; letter-spacing: 0.12em;
      text-transform: uppercase; color: #3A8FB7; margin-bottom: 12px; }

    .ctrl-row { display: flex; flex-wrap: wrap; gap: 14px;
      margin-bottom: 14px; align-items: center; }
    .ctrl-row label { font-size: 0.78rem; color: #3A8FB7;
      text-transform: uppercase; letter-spacing: 0.06em; }
    .shiny-input-container { margin: 0 !important; }
    .irs--shiny .irs-bar, .irs--shiny .irs-handle,
    .irs--shiny .irs-single { background: #3A8FB7 !important; }
    .irs--shiny .irs-line { background: #E2E8F0 !important; }
    .irs--shiny .irs-min, .irs--shiny .irs-max,
    .irs--shiny .irs-from, .irs--shiny .irs-to { color: #4A5568 !important; }
    .leaflet-container { background: #FFFFFF !important; border-radius: 6px; }

    #footer { background: #F5F7FA; border-top: 1px solid #E2E8F0;
      padding: 40px 10%; font-size: 0.82rem; color: #4A5568; line-height: 1.8; }
    #footer a { color: #3A8FB7; text-decoration: none; }
    
    #intro { padding: 60px 0; background: #FFFFFF; }
    .intro-block { max-width: 680px; margin: 0 auto; padding: 0 40px; }
    .intro-drop { float: left; font-size: 5rem; line-height: 0.8;
      margin-right: 8px; margin-top: 12px; color: #3A8FB7; font-family: Georgia, serif; }
    .intro-text { font-size: 1.1rem; color: #2D3748; line-height: 1.85;
      margin-bottom: 20px; font-family: Georgia, serif; }
    .transition-block { padding: 50px 0; background: #F5F7FA;
      border-top: 1px solid #E2E8F0; }

    @media (max-width: 800px) {
      .sticky-layout { flex-direction: column; padding: 0 20px; }
      .sticky-text { position: static; flex: none; width: 100%; }
    }
  "))),
  
  # Header
  tags$div(id = "site-header",
           tags$div(class = "title-text", "Mental Health Around the World"),
           tags$div(id = "progress-dots",
                    tags$div(class = "dot active", id = "dot0",
                             onclick = "window.scrollTo({top:0,behavior:'smooth'})"),
                    tags$div(class = "dot", id = "dot1",
                             onclick = "document.getElementById('section1').scrollIntoView({behavior:'smooth'})"),
                    tags$div(class = "dot", id = "dot2",
                             onclick = "document.getElementById('section2').scrollIntoView({behavior:'smooth'})"),
                    tags$div(class = "dot", id = "dot3",
                             onclick = "document.getElementById('section3').scrollIntoView({behavior:'smooth'})")
           )
  ),
  

# Hero
tags$div(id = "hero",
         tags$div(class = "hero-inner",
                  tags$div(class = "hero-left",
                           tags$p(class = "eyebrow", "A data story \u00b7 Yawen Li \u00b7 FIT5147"),
                           tags$h1("What shapes mental health around the world?"),
                           tags$div(class = "questions-list",
                                    tags$p(class = "q-item", "Q1 \u2014 How do mental health outcomes vary globally?"),
                                    tags$p(class = "q-item", "Q2 \u2014 How does sunlight associate with mental health?"),
                                    tags$p(class = "q-item", "Q3 \u2014 Do life satisfaction and suicide rate tell the same story?")
                           ),
                           tags$div(class = "hero-stats",
                                    tags$div(class = "stat-card", tags$span(class = "stat-num", "141"), tags$span(class = "stat-label", "Countries")),
                                    tags$div(class = "stat-card", tags$span(class = "stat-num", "10"), tags$span(class = "stat-label", "Years of data")),
                                    tags$div(class = "stat-card", tags$span(class = "stat-num", "3"), tags$span(class = "stat-label", "Indicators"))
                           ),
                           tags$div(class = "scroll-hint",
                                    tags$div(class = "arrow-down"), tags$span("SCROLL TO EXPLORE"))
                  ),
                  tags$div(class = "hero-right",
                           HTML('
    <div class="globe-container">
      <svg viewBox="0 0 300 300" xmlns="http://www.w3.org/2000/svg" class="globe-svg">
        <!-- Background circle -->
        <circle cx="150" cy="150" r="120" fill="#EBF4FA" stroke="#3A8FB7" stroke-width="1.5" opacity="0.8"/>
        
        <!-- Meridians -->
        <ellipse cx="150" cy="150" rx="40" ry="120" fill="none" stroke="#3A8FB7" stroke-width="0.8" opacity="0.4"/>
        <ellipse cx="150" cy="150" rx="80" ry="120" fill="none" stroke="#3A8FB7" stroke-width="0.8" opacity="0.4"/>
        <ellipse cx="150" cy="150" rx="110" ry="120" fill="none" stroke="#3A8FB7" stroke-width="0.8" opacity="0.4"/>
        <line x1="150" y1="30" x2="150" y2="270" stroke="#3A8FB7" stroke-width="0.8" opacity="0.4"/>
        
        <!-- Parallels -->
        <ellipse cx="150" cy="110" rx="100" ry="25" fill="none" stroke="#3A8FB7" stroke-width="0.8" opacity="0.4"/>
        <ellipse cx="150" cy="150" rx="120" ry="30" fill="none" stroke="#3A8FB7" stroke-width="0.8" opacity="0.4"/>
        <ellipse cx="150" cy="190" rx="100" ry="25" fill="none" stroke="#3A8FB7" stroke-width="0.8" opacity="0.4"/>
        
        <!-- Data points -->
        <circle cx="110" cy="130" r="4" fill="#3A8FB7" opacity="0.7" class="pulse-dot"/>
        <circle cx="170" cy="120" r="3" fill="#3A8FB7" opacity="0.6" class="pulse-dot"/>
        <circle cx="200" cy="155" r="5" fill="#3A8FB7" opacity="0.8" class="pulse-dot"/>
        <circle cx="130" cy="170" r="3" fill="#3A8FB7" opacity="0.6" class="pulse-dot"/>
        <circle cx="185" cy="175" r="4" fill="#3A8FB7" opacity="0.7" class="pulse-dot"/>
        <circle cx="150" cy="140" r="3" fill="#3A8FB7" opacity="0.5" class="pulse-dot"/>
        <circle cx="220" cy="135" r="3" fill="#3A8FB7" opacity="0.6" class="pulse-dot"/>
        <circle cx="95"  cy="155" r="4" fill="#3A8FB7" opacity="0.7" class="pulse-dot"/>
        
        <!-- Outer ring decoration -->
        <circle cx="150" cy="150" r="120" fill="none" stroke="#3A8FB7" stroke-width="1" opacity="0.3"/>
        <circle cx="150" cy="150" r="135" fill="none" stroke="#3A8FB7" stroke-width="0.5" opacity="0.15" stroke-dasharray="4,6"/>
      </svg>
    </div>
  ')
                  )
         )
),
# Intro text block
tags$div(id = "intro",
         tags$div(class = "intro-block",
                  tags$p(class = "intro-drop", "W"),
                  tags$p(class = "intro-text",
                         "hen I moved to Melbourne, I noticed something unexpected — my mood shifted 
       with the seasons. Less sunlight meant less energy. It was a personal 
       observation, but it raised a bigger question: could the same be true 
       at a global scale?"
                  ),
                  tags$p(class = "intro-text",
                         "Mental health is one of the most complex and least understood dimensions 
       of human wellbeing. It is shaped by culture, economics, healthcare access, 
       and yes — perhaps even the amount of sunlight a country receives each year."
                  ),
                  tags$p(class = "intro-text",
                         "This data story explores two indicators — life satisfaction and suicide rate — 
       across 141 countries over a decade. The findings are sometimes surprising, 
       sometimes counterintuitive, and always worth examining. Scroll down to explore 
       the data — starting with how mental health varies across the globe."
                  )
         )
),
  # Section 1
  tags$div(id = "section1", class = "section-block",
           tags$div(class = "sticky-layout",
                    tags$div(class = "sticky-text",
                             tags$p(class = "section-label", "Section 01"),
                             tags$h2("How does mental health vary around the world?"),
                             tags$p("Choose an indicator and drag the year slider to see how it is distributed across countries. Hover over a country for its value."),
                             tags$div(class = "finding-box",
                                      "Life satisfaction is highest in Oceania and Europe and lowest across much of Africa. Suicide rate follows a different geography \u2014 the two are not simple mirrors of each other."),
                             tags$br(),
                             tags$div(class = "ctrl-row", tags$label("Indicator"),
                                      selectInput("s1_indicator", NULL,
                                                  c("Life satisfaction" = "satisfaction_score",
                                                    "Suicide rate" = "suicide_rate"), width = "200px")),
                             tags$div(class = "ctrl-row", tags$label("Year"),
                                      sliderInput("s1_year", NULL, min = 2011, max = 2021,
                                                  value = 2021, step = 1, sep = "", width = "200px"))
                    ),
                    tags$div(class = "charts-col",
                             tags$div(class = "chart-card",
                                      tags$p(class = "chart-title", "World Map"),
                                      leafletOutput("map_s1", height = "420px")),
                             tags$div(class = "chart-card",
                                      tags$p(class = "chart-title", "Trends by Continent, 2011\u20132021"),
                                      plotlyOutput("line_s1", height = "300px"))
                    )
           )
  ),
  
# Transition block between Section 1 and Section 2
tags$div(class = "transition-block",
         tags$div(class = "intro-block",
                  tags$p(class = "intro-drop", "T"),
                  tags$p(class = "intro-text",
                         "he pattern is clear — but what drives it? One factor researchers have 
       long examined is sunlight. Studies show that sunlight exposure influences 
       serotonin levels, circadian rhythms, and mood regulation. Seasonal Affective 
       Disorder, a condition linked to reduced daylight in winter months, affects 
       millions worldwide — and is more prevalent at higher latitudes where seasonal 
       light changes are most extreme."
                  ),
                  tags$p(class = "intro-text",
                         "But does annual sunlight exposure — measured across an entire country — 
       actually predict mental health outcomes at the global scale? 
       The answer turns out to be more complicated than expected."
                  )
         )
),
  # Section 2
  tags$div(id = "section2", class = "section-block",
           tags$div(class = "sticky-layout",
                    tags$div(class = "sticky-text",
                             tags$p(class = "section-label", "Section 02"),
                             tags$h2("Does more sunlight mean better mental health?"),
                             tags$p("Each point is a country in 2021. Colour intensity shows the indicator value — darker means higher. Hover over a point for details."),
                             tags$div(class = "finding-box",
                                      "Surprisingly, higher solar radiation is associated with lower life satisfaction overall \u2014 likely because sunlight is tangled up with broader economic and social differences."),
                             tags$br(),
                             tags$div(class = "ctrl-row", tags$label("Y-axis indicator"),
                                      selectInput("s2_indicator", NULL,
                                                  c("Life satisfaction" = "satisfaction_score",
                                                    "Suicide rate" = "suicide_rate"), width = "200px"))
                    ),
                    tags$div(class = "charts-col",
                             tags$div(class = "chart-card",
                                      tags$p(class = "chart-title", "Solar Radiation vs Mental Health (2021)"),
                                      plotlyOutput("scatter_s2", height = "500px"))
                    )
           )
  ),

# Transition block between Section 2 and Section 3
tags$div(class = "transition-block",
         tags$div(class = "intro-block",
                  tags$p(class = "intro-drop", "S"),
                  tags$p(class = "intro-text",
                         "o sunlight alone does not explain the full picture. But perhaps the 
         two indicators we have been using — life satisfaction and suicide rate 
         — are simply two sides of the same coin. If they measure the same thing, 
         we only need one of them."
                  ),
                  tags$p(class = "intro-text",
                         "But do they? A country with high life satisfaction should, in theory, 
         also have a low suicide rate. The data tells a more complicated story."
                  )
         )
),
  # Section 3
  tags$div(id = "section3", class = "section-block",
           tags$div(class = "sticky-layout",
                    tags$div(class = "sticky-text",
                             tags$p(class = "section-label", "Section 03"),
                             tags$h2("Are life satisfaction and suicide rate the same thing?"),
                             tags$p("If high life satisfaction simply meant low suicide rates, the two would be interchangeable. The charts on the right test that."),
                             tags$div(class = "finding-box",
                                      "They are not. Europe and Oceania show high life satisfaction AND relatively high suicide rates. The two indicators capture different dimensions of mental health.")
                    ),
                    tags$div(class = "charts-col",
                             tags$div(class = "chart-card",
                                      tags$p(class = "chart-title", "Country Level (2021)"),
                                      plotlyOutput("scatter_s3", height = "360px")),
                             tags$div(class = "chart-card",
                                      tags$p(class = "chart-title", "Regional Mean Comparison"),
                                      plotlyOutput("bubble_s3", height = "300px"))
                    )
           )
  ),
  
  # Footer
  tags$div(id = "footer",
           tags$p("Data sources:"),
           tags$p(
             tags$a(href = "https://ourworldindata.org/grapher/happiness-cantril-ladder",
                    "Our World in Data \u2014 Life Satisfaction"), " \u00b7 ",
             tags$a(href = "https://ourworldindata.org/grapher/death-rate-from-suicides-ghe",
                    "Our World in Data \u2014 Suicide Rate"), " \u00b7 ",
             tags$a(href = "https://power.larc.nasa.gov/data-access-viewer/",
                    "NASA POWER \u2014 Solar Radiation")
           ),
           tags$p("Yawen Li \u00b7 35429399 \u00b7 FIT5147 \u00b7 Monash University 2026")
  ),
  
  tags$script(HTML("
    window.addEventListener('scroll', function() {
      var ids = ['section1','section2','section3'];
      var dots = ['dot0','dot1','dot2','dot3'];
      dots.forEach(function(d){ document.getElementById(d).classList.remove('active'); });
      var y = window.scrollY, active = 0;
      ids.forEach(function(id, i) {
        var el = document.getElementById(id);
        if (el && y >= el.offsetTop - 200) active = i + 1;
      });
      document.getElementById(dots[active]).classList.add('active');
    });
  "))
)

# SERVER
server <- function(input, output, session) {
  
  # ---- SECTION 1: Map ----
  output$map_s1 <- renderLeaflet({
    var <- input$s1_indicator
    
    df <- q1_data %>%
      filter(year == input$s1_year) %>%
      left_join(coords, by = c("code" = "iso_a3")) %>%
      filter(!is.na(lat), !is.na(lon), !is.na(.data[[var]]))
    
    label_txt <- if (var == "satisfaction_score")
      "Life satisfaction score" else "Suicide rate (per 100k)"
    
    # Use continent colours instead of value-based palette
    cont_pal <- colorFactor(
      palette = c("#C97C5D", "#6E8FA8", "#56C596", "#F59E0B", "#B48EAD"),
      levels  = c("Africa", "Americas", "Asia", "Europe", "Oceania")
    )
    
    leaflet(df) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat, radius = 6,
        color = "white", weight = 1, opacity = 0.8,
        fillColor = ~cont_pal(continent),
        fillOpacity = 0.85,
        popup = ~paste0("<b>", country, "</b><br>",
                        label_txt, ": ", round(get(var), 2),
                        "<br>Continent: ", continent)
      ) %>%
      addLegend(position = "bottomright",
                pal = cont_pal, values = df$continent,
                title = "Continent", opacity = 0.9)
  })
  

  # ---- SECTION 1: Line chart ----
  output$line_s1 <- renderPlotly({
    var <- input$s1_indicator
    y_lab <- if (var == "satisfaction_score")
      "Mean life satisfaction" else "Mean suicide rate"
    
    trend <- q1_data %>%
      group_by(continent, year) %>%
      summarise(mean_val = mean(.data[[var]], na.rm = TRUE), .groups = "drop")
    
    plot_ly(trend, x = ~year, y = ~mean_val,
            color = ~continent, colors = continent_colors,
            type = "scatter", mode = "lines+markers",
            text = ~paste0(continent, "<br>", year, ": ", round(mean_val, 2)),
            hoverinfo = "text",
            line = list(width = 2), marker = list(size = 5)) %>%
      layout(paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF",
             font = list(color = "#4A5568", size = 11),
             xaxis = list(title = "Year", gridcolor = "#1E2F42",
                          tickvals = seq(2011, 2021, 2)),
             yaxis = list(title = y_lab, gridcolor = "#1E2F42"),
             legend = list(orientation = "h", y = -0.3, x = 0.5, xanchor = "center"),
             margin = list(l = 50, r = 20, t = 10, b = 80))
  }
  )
  
  # ---- SECTION 2: Scatter ----
  output$scatter_s2 <- renderPlotly({
    var <- input$s2_indicator
    y_lab <- if (var == "satisfaction_score")
      "Life satisfaction score" else "Suicide rate (per 100k)"
    
    df <- q2_data %>%
      filter(!is.na(annual_solar_radiation), !is.na(.data[[var]]))
    
    colors <- if (var == "satisfaction_score")
      c("#C8E6F5", "#2171B5") else c("#FEE5D9", "#A50F15")
    
    plot_ly(df, 
            x = ~annual_solar_radiation, 
            y = df[[var]],
            type = "scatter", 
            mode = "markers",
            marker = list(
              size = 9,
              opacity = 0.85,
              color = df[[var]],
              colorscale = list(c(0, colors[1]), c(1, colors[2])),
              showscale = TRUE,
              colorbar = list(
                title = y_lab,
                thickness = 10,
                len = 0.5,
                x = 1.02
              )
            ),
            text = ~paste0("<b>", country, "</b><br>",
                           "Solar: ", round(annual_solar_radiation, 2),
                           "<br>", y_lab, ": ", round(df[[var]], 2),
                           "<br>Continent: ", continent),
            hoverinfo = "text") %>%
      add_lines(x = ~annual_solar_radiation,
                y = fitted(loess(df[[var]] ~ df$annual_solar_radiation)),
                inherit = FALSE,
                line = list(color = "#2D3748", width = 2),
                showlegend = FALSE,
                hoverinfo = "none") %>%
      layout(paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF",
             font = list(color = "#4A5568", size = 11),
             xaxis = list(title = "Annual solar radiation (kWh/m\u00b2/day)",
                          gridcolor = "#E2E8F0",
                          range = c(1, 7.5)),
             yaxis = list(title = y_lab, gridcolor = "#E2E8F0"),
             margin = list(l = 100, r = 80, t = 10, b = 80))
  })
  
  # ---- SECTION 3: Bubble (regional means) ----
  # ---- SECTION 3: Scatter (life satisfaction vs suicide) ----
  output$scatter_s3 <- renderPlotly({
    df <- q1_data %>%
      filter(year == 2021, !is.na(satisfaction_score), !is.na(suicide_rate))
    
    plot_ly(df, x = ~satisfaction_score, y = ~suicide_rate,
            color = ~continent, colors = continent_colors,
            type = "scatter", mode = "markers",
            text = ~paste0("<b>", country, "</b><br>",
                           "Life satisfaction: ", round(satisfaction_score, 2),
                           "<br>Suicide rate: ", round(suicide_rate, 2)),
            hoverinfo = "text",
            marker = list(size = 8, opacity = 0.8)) %>%
      layout(paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF",
             font = list(color = "#4A5568", size = 11),
             xaxis = list(title = "Life satisfaction score", gridcolor = "#E2E8F0"),
             yaxis = list(title = "Suicide rate (per 100k)", gridcolor = "#E2E8F0"),
             legend = list(orientation = "h", y = -0.2),
             margin = list(l = 55, r = 20, t = 10, b = 60))
  })
  output$bubble_s3 <- renderPlotly({
    df <- q1_data %>%
      filter(year == 2021) %>%
      group_by(continent) %>%
      summarise(mean_sat = mean(satisfaction_score, na.rm = TRUE),
                mean_sui = mean(suicide_rate, na.rm = TRUE),
                n = n(), .groups = "drop")
    
    plot_ly(df, x = ~mean_sat, y = ~mean_sui,
            color = ~continent, colors = continent_colors,
            type = "scatter", mode = "markers+text",
            text = ~continent, textposition = "top center",
            hovertext = ~paste0("<b>", continent, "</b><br>",
                                "Mean satisfaction: ", round(mean_sat, 2),
                                "<br>Mean suicide: ", round(mean_sui, 2),
                                "<br>Countries: ", n),
            hoverinfo = "text",
            marker = list(size = ~n, sizemode = "area",
                          sizeref = 0.05, opacity = 0.75)) %>%
      layout(paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF",
             font = list(color = "#4A5568", size = 11),
             gridcolor = "#E2E8F0",
             xaxis = list(title = "Regional mean life satisfaction",
                          gridcolor = "#1E2F42"),
             yaxis = list(title = "Regional mean suicide rate",
                          gridcolor = "#1E2F42"),
             showlegend = FALSE,
             margin = list(l = 60, r = 20, t = 10, b = 50))
  })
}

# RUN
shinyApp(ui = ui, server = server)
