library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(tigris)
library(readr)
library(tidycensus)
library(purrr)
library(stringr)
library(DT)
library(rsconnect)

setwd("/Users/jordanbaechle/Desktop/SDOH")

# Set your Census API key
# census_api_key("96fa6465c555026fce996f660c362865d24f2a0d", install = TRUE, overwrite = TRUE)
census_api_key(Sys.getenv("CENSUS_API_KEY"))
readRenviron("~/.Renviron")

# Load ADI, SVI, and Weather Risk Data
adi_data <- read_csv("data/TON_ADI.csv")
svi_data <- read_csv("data/TON_SVI_data.csv")
weather_risk_data <- read_csv("data/TON_weather_risk.csv")

# Prepare ADI data with 12-digit block numbers
adi_data <- adi_data %>%
  mutate(FIPS = sprintf("%012.0f", as.numeric(FIPS)))

# Prepare SVI and Weather Risk data with 11-digit census tract numbers
svi_data <- svi_data %>%
  mutate(FIPS = sprintf("%011.0f", as.numeric(FIPS)))

weather_risk_data <- weather_risk_data %>%
  mutate(FIPS = sprintf("%011.0f", as.numeric(FIPS)))

# Ensure FIPS is a string and pad with leading zeros if necessary
adi_data <- adi_data %>%
  mutate(FIPS = str_pad(FIPS, width = 12, side = "left", pad = "0"),
         ADI_NATRANK = as.numeric(ADI_NATRANK),
         ADI_STATERNK = as.numeric(ADI_STATERNK))

svi_data <- svi_data %>%
  mutate(FIPS = str_pad(FIPS, width = 11, side = "left", pad = "0"))

weather_risk_data <- weather_risk_data %>%
  mutate(FIPS = str_pad(FIPS, width = 11, side = "left", pad = "0"))

# Filter out rows with missing numeric Weather Risk data
numeric_columns <- sapply(weather_risk_data, is.numeric)
weather_data <- weather_risk_data %>%
  filter(complete.cases(weather_risk_data[, numeric_columns]))

# Define the states you want to retrieve census data for
states <- c("TX", "NM", "OK")  # Texas, New Mexico, Oklahoma

# Retrieve Census Block Group data for ADI mapping
blocks <- map_dfr(states, function(state) {
  get_decennial(
    geography = "block group", 
    state = state, 
    variables = "P1_001N",  # Total population variable from 2020 Census
    geometry = TRUE,
    year = 2020
  )
}) %>%
  st_transform(4326)  # Transform to WGS 84

# Retrieve Census Tract data for SVI and Weather Risk mapping
tracts <- map_dfr(states, function(state) {
  get_decennial(
    geography = "tract", 
    state = state, 
    variables = "P1_001N",  # Total population variable from 2020 Census
    geometry = TRUE,
    year = 2020
  )
}) %>%
  st_transform(4326)  # Transform to WGS 84

# Load U.S. state boundaries and filter for TX, NM, and OK
us_states <- states(cb = TRUE, class = "sf") %>%
  st_transform(4326) %>%
  filter(STUSPS %in% c("TX", "NM", "OK"))

# Merge the ADI data with the census block geometries
block_data <- blocks %>%
  left_join(adi_data, by = c("GEOID" = "FIPS"))

# Merge the SVI and Weather Risk data with the census tract geometries
tract_data <- tracts %>%
  left_join(svi_data, by = c("GEOID" = "FIPS"))

weather_data <- tracts %>%
  left_join(weather_risk_data, by = c("GEOID" = "FIPS"))

# Filter out rows with missing ADI data after merging
block_data <- block_data %>%
  filter(!is.na(ADI_NATRANK) & !is.na(ADI_STATERNK))

# Filter out rows with missing SVI data after merging
tract_data <- tract_data %>%
  filter(!is.na(RPL_THEMES))

# User Interface
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .githublogo {
        position: fixed;
        top: 10px;
        right: 10px;
        z-index: 1100;
      }
      .navbar {
        position: fixed;
        top: 0;
        width: 100%;
        z-index: 100;
      }
      body { 
        padding-bottom: 150px;
        padding-top: 60px;
      } 
      .footer {
        position: fixed;
        bottom: 0;
        width: 100%;
        background-color: rgba(255, 255, 255, 0.85); 
        text-align: center;
        padding: 10px;
      }
      @media (max-width: 768px) {
        .githublogo {
          right: 10px;
        }
        .navbar, .footer {
          width: 100%;
        }
        .navbar-header {
          height: 40px;
          margin-left: 0%;
        }
        .navbar-toggle {
          float: left;
          margin-left: 0;
        }
        .navbar-collapse {
          clear: both;
        }
        body {
          padding-top: 40px;
          padding-bottom: 0px;
        }
        .footer {
          display: none;
        }
        .no-space-above {
          margin-top: 0px;
        }
      }
    "))
  ),
  
  navbarPage(
    title = "",
    id = "mainNavbar",
    collapsible = TRUE, 
    windowTitle = "TTUHSC",
    tags$head(tags$style(HTML('.navbar .navbar-brand {padding-top: 0; padding-bottom: 0;}'))),
    
    # Place the GitHub logo link inside the navbar
    tags$a(href="https://github.com/jordanbaechle/promise", target="_blank",
           tags$img(src="githublogo.png", class="githublogo", height="35px"), # Adjust height as needed
           class="pull-right" # Bootstrap class to align to the right
    ),
    
    tabPanel("About",
             fluidRow(
               column(10, offset = 1,
                      # Center the image and adjust its size
                      br(),
                      br(),
                      br(),
                      br(),
                      div(style = "text-align: center;",
                          tags$img(src = "wallpaper1.png", width = "60%", height = "auto")
                      ),
                      # Add the centered GIF
                      div(style = "text-align: center;",
                          tags$img(src = "scroll.gif", width = "50%", height = "auto")
                      ),
                      br(),
                      br(),
                      h3("About this Application"),
                      p("This application visualizes the Area Deprivation Index (ADI), Social Vulnerability Index (SVI), and Weather Risk across the states of Texas, New Mexico, and Oklahoma. 
                      You can select different variables to map and also search for specific FIPS codes to get more detailed information."),
                      br(),
                      br(),
               )
             ),
             
             fluidRow(
               column(5, offset = 1,
                      br(),
                      br(),
                      # Image on the left
                      tags$img(src = "wallpaper2.png", width = "100%", height = "auto")
               ),
               column(5,
                      # Text on the right
                      h3("More About ADI, SVI, and Weather Risk"),
                      p("Geographical social determinants of health, such as the Area Deprivation Index (ADI) and the Social Vulnerability Index (SVI), significantly influence surgical outcomes and access to surgical care. The SVI assesses a community's vulnerability based on factors like socioeconomic status, household composition, minority status, and housing type, which can further exacerbate disparities in surgical care. In regions with high SVI, patients often encounter barriers such as limited access to surgical facilities, reduced healthcare resources, and longer delays in receiving care. Additionally, weather, climate, and disaster risks pose substantial challenges, particularly in vulnerable areas where infrastructure may be inadequate to withstand extreme events. Natural disasters can disrupt surgical services, delay procedures, and impede patient access to care, leading to worsened health outcomes. Addressing these disparities requires a multifaceted approach that includes strengthening healthcare infrastructure, improving emergency preparedness, and tailoring interventions to address the compounded risks associated with social vulnerability and environmental factors.")
               )
             ),
             
             fluidRow(
               column(4, offset = 1,
                      # Text on the left
                      h3("Additional Information"),
                      p("This application was developed as part of a project to better understand and visualize the interplay between social, economic, and environmental factors in Texas, New Mexico, and Oklahoma. The data used in this application is sourced from reputable public datasets."),
                      br(),
                      p("Using integrated census data to quantify risk is a powerful tool for improving health and surgical outcomes, particularly in vulnerable populations. By combining data from sources such as the Census Bureau with health records and socioeconomic indicators like the Area Deprivation Index (ADI) and Social Vulnerability Index (SVI), healthcare providers and policymakers can identify communities at higher risk for poor health outcomes due to factors like poverty, limited access to healthcare, and environmental hazards. This data-driven approach allows for the identification of geographic and demographic disparities in surgical care, enabling targeted interventions to address the specific needs of at-risk populations. For example, areas with high ADI or SVI scores may benefit from increased healthcare resources, community outreach programs, or the placement of specialized surgical services to mitigate the barriers these communities face. Additionally, integrating census data with health outcomes allows for more accurate predictions of surgical risks, leading to more personalized care plans and better resource allocation, ultimately enhancing both individual and community health outcomes.")
                      ),
               column(6,
                      # Image on the right
                      tags$img(src = "wallpaper3.png", width = "100%", height = "auto")
               )
             )
    ),
    
    tabPanel("ADI Mapping",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", "Select Area of Deprivation Index Variable to Display:", 
                             choices = c("ADI National Rank" = "ADI_NATRANK", 
                                         "ADI State Rank" = "ADI_STATERNK")),
                 sliderInput("opacity", "Select Map Opacity:", min = 0.1, max = 1, value = 1.0),
                 textInput("fips_input", "Enter FIPS code(s):", placeholder = "e.g., 350010001071"),
                 actionButton("submit_fips", "Submit")
               ),
               
               mainPanel(
                 leafletOutput("blockMap", height = "600px"),
                 DTOutput("fips_table")  # Table to display FIPS information
               )
             )
    ),
    
    tabPanel("SVI Mapping",
             sidebarLayout(
               sidebarPanel(
                 selectInput("svi_variable", "Select Social Vulnerability Index Variable to Display:", 
                             choices = c(
                               "Socioeconomic Status" = "RPL_THEME1", 
                               "Household Characteristics" = "RPL_THEME2", 
                               "Racial & Ethnic Minority Status" = "RPL_THEME3", 
                               "Housing Type & Transportation" = "RPL_THEME4", 
                               "Social Vulnerability Score" = "RPL_THEMES"
                             )),
                 sliderInput("svi_opacity", "Select Map Opacity:", min = 0.1, max = 1, value = 1.0),
                 textInput("svi_fips_input", "Enter FIPS code(s):", placeholder = "e.g., 350010001071"),
                 actionButton("submit_svi_fips", "Submit")
               ),
               
               mainPanel(
                 leafletOutput("tractMap", height = "600px"),
                 DTOutput("svi_fips_table")  # Table to display SVI FIPS information
               )
             )
    ),
    tabPanel("NRI Mapping",
             sidebarLayout(
               sidebarPanel(
                 selectInput("weather_variable", "Select National Risk Index Variable to Display:", 
                             choices = names(weather_data)[6:ncol(weather_data)]),
                 sliderInput("weather_opacity", "Select Map Opacity:", min = 0.1, max = 1, value = 1.0),
                 textInput("weather_fips_input", "Enter FIPS code(s):", placeholder = "e.g., 350010001071"),
                 actionButton("submit_weather_fips", "Submit")
               ),
               
               mainPanel(
                 leafletOutput("weatherMap", height = "600px"),
                 DTOutput("weather_fips_table")  # Table to display Weather Risk FIPS information
               )
             )
    ),
    
    tabPanel("References",
             fluidRow(
               column(10, offset = 1,
                      h3("References"),
                      p("This application uses the following data sources:"),
                      tags$ul(
                        tags$li("Area Deprivation Index (ADI): ..."),
                        tags$li("Social Vulnerability Index (SVI): ..."),
                        tags$li("National Risk Index (NRI): ..."),
                        tags$li("...")
                      ),
                      p("For more information, please visit the relevant links or contact the project team."),
                      h3("Contributors"),
                      p("..."),
                      h3("Acknowledgments"),
                      p("..."),
                      h3("Publications"),
                      p("...")
               )
             )
    )
  ),
  
  # Footer
  tags$div(class = "footer",
           tags$img(src = "logo1.png", height = "95px", style = "right: 10px; margin-right: 10px;")
  )
)


# Server Logic
server <- function(input, output, session) {
  
  # Render ADI Map
  output$blockMap <- renderLeaflet({
    selected_variable <- input$variable
    
    # Create a color palette based on the selected variable
    pal <- colorNumeric("RdYlBu", domain = block_data[[selected_variable]], na.color = "gray", reverse = TRUE)
    
    # Render leaflet map with ADI data
    leaflet(data = block_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  # Base map
      addPolygons(
        fillColor = ~pal(get(selected_variable)),
        fillOpacity = input$opacity,
        color = "white",
        weight = 1,
        smoothFactor = 0.5,
        popup = ~paste0(
          "Block GEOID: ", GEOID, "<br>",
          "Name: ", NAME, "<br>",
          "ADI National Rank: ", ADI_NATRANK, "<br>",
          "ADI State Rank: ", ADI_STATERNK)
      ) %>%
      addPolygons(        
        data = us_states,  # Adding state borders
        fill = FALSE,      # Do not fill states
        color = "black",   # Black borders for states
        weight = 0.75 
      ) %>%
      setView(lng = -99.9018, lat = 34.0003, zoom = 5) %>%  # Center the map on the three states
      addLegend(
        "bottomright",
        pal = pal,
        values = block_data[[selected_variable]],
        title = selected_variable,
        opacity = 1
      )
  })
  
  observeEvent(input$submit_fips, {
    fips_list <- strsplit(input$fips_input, ",\\s*")[[1]]
    fips_data <- block_data %>%
      filter(GEOID %in% fips_list)
    
    output$fips_table <- renderDT({
      datatable(fips_data %>% select(GEOID, NAME, ADI_NATRANK, ADI_STATERNK, distance_to_reference))
    })
  })
  
  # Render SVI Map
  output$tractMap <- renderLeaflet({
    selected_variable <- input$svi_variable
    
    # Create a color palette based on the selected SVI variable
    pal <- colorNumeric("RdYlBu", domain = tract_data[[selected_variable]], na.color = "gray", reverse = TRUE)
    
    # Render leaflet map with SVI data
    leaflet(data = tract_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  # Base map
      addPolygons(
        fillColor = ~pal(get(selected_variable)),
        fillOpacity = input$svi_opacity,
        color = "white",
        weight = 1,
        smoothFactor = 0.5,
        popup = ~paste0(
          "Census Tract GEOID: ", GEOID, "<br>",
          selected_variable, ": ", get(selected_variable))
      ) %>%
      addPolygons(
        data = us_states,  # Adding state borders
        fill = FALSE,      # Do not fill states
        color = "black",   # Black borders for states
        weight = 0.75 
      ) %>%
      setView(lng = -99.9018, lat = 34.0003, zoom = 5) %>%  # Center the map on the three states
      addLegend(
        "bottomright",
        pal = pal,
        values = tract_data[[selected_variable]],
        title = selected_variable,
        opacity = 1
      )
  })
  
  observeEvent(input$submit_svi_fips, {
    svi_fips_list <- strsplit(input$svi_fips_input, ",\\s*")[[1]]
    svi_fips_data <- tract_data %>%
      filter(GEOID %in% svi_fips_list)
    
    output$svi_fips_table <- renderDT({
      datatable(svi_fips_data %>% select(GEOID, NAME, RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4, RPL_THEMES))
    })
  })
  
  # Render Weather Risk Map
  output$weatherMap <- renderLeaflet({
    selected_variable <- input$weather_variable
    
    # Create a color palette based on the selected weather risk variable
    pal <- colorNumeric("RdYlBu", domain = weather_data[[selected_variable]], na.color = "gray", reverse = TRUE)
    
    # Render leaflet map with weather risk data
    leaflet(data = weather_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  # Base map
      addPolygons(
        fillColor = ~pal(get(selected_variable)),
        fillOpacity = input$weather_opacity,
        color = "white",
        weight = 1,
        smoothFactor = 0.5,
        popup = ~paste0(
          "Census Tract GEOID: ", GEOID, "<br>",
          selected_variable, ": ", get(selected_variable))
      ) %>%
      addPolygons(
        data = us_states,  # Adding state borders
        fill = FALSE,      # Do not fill states
        color = "black",   # Black borders for states
        weight = 0.75 
      ) %>%
      setView(lng = -99.9018, lat = 34.0003, zoom = 5) %>%  # Center the map on the three states
      addLegend(
        "bottomright",
        pal = pal,
        values = weather_data[[selected_variable]],
        title = selected_variable,
        opacity = 1
      )
  })
  
  observeEvent(input$submit_weather_fips, {
    weather_fips_list <- strsplit(input$weather_fips_input, ",\\s*")[[1]]
    weather_fips_data <- weather_data %>%
      filter(GEOID %in% weather_fips_list)
    
    output$weather_fips_table <- renderDT({
      datatable(weather_fips_data %>% select(GEOID, NAME, matches("^Weather")))
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)





             

             