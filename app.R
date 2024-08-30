library(leaflet)
library(sf)
library(tigris)
library(dplyr)
library(DT)
library(rsconnect)
library(readr)
library(stringr)
library(plotly)
library(ggplot2)
library(flexdashboard)
library(leaflet.extras)

# Preload Data (outside server to avoid repeated loading)
zip_boundaries <- readRDS("data/zip_boundaries.rds")

# Load and process ton_master_zip once
ton_master_zip <- read_csv("data/TON_master_zip.csv") %>%
  mutate(ZIP = as.character(ZIP))

# Join zip_boundaries and ton_master_zip
zip_boundaries <- zip_boundaries %>%
  left_join(ton_master_zip, by = c("ZCTA5CE10" = "ZIP"))

# User Interface
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .githublogo { position: fixed; top: 10px; right: 10px; z-index: 1100; }
      .navbar { position: fixed; top: 0; width: 100%; z-index: 100; }
      body { padding-bottom: 150px; padding-top: 60px; } 
      .footer { position: fixed; bottom: 0; width: 100%; background-color: rgba(255, 255, 255, 0.85); text-align: center; padding: 10px; }
      @media (max-width: 768px) {
        .githublogo { right: 10px; }
        .navbar, .footer { width: 100%; }
        .navbar-header { height: 40px; margin-left: 0%; }
        .navbar-toggle { float: left; margin-left: 0; }
        .navbar-collapse { clear: both; }
        body { padding-top: 40px; padding-bottom: 0px; }
        .footer { display: none; }
      }
    "))
  ),
  
  navbarPage(
    title = "",
    id = "mainNavbar",
    collapsible = TRUE, 
    windowTitle = "TTUHSC",
    tags$head(tags$style(HTML('.navbar .navbar-brand {padding-top: 0; padding-bottom: 0;}'))),
    
    # GitHub logo in navbar
    tags$a(href="https://github.com/jordanbaechle/SDOH", target="_blank",
           tags$img(src="githublogo.png", class="githublogo", height="35px"),
           class="pull-right"
    ),
    
    tabPanel("About",
             fluidRow(
               column(10, offset = 1,
                      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                      div(style = "text-align: center;",
                          tags$img(src = "wallpaper1.png", width = "45%", height = "auto")
                      ),
                      div(style = "text-align: center;",
                          tags$img(src = "scroll.gif", width = "50%", height = "auto")
                      ),
                      br(),
                      br(),
                      h3("About this Application"),
                      p("This application visualizes the Area Deprivation Index (ADI), Social Vulnerability Index (SVI), and Weather Risk across the states of Texas, New Mexico, and Oklahoma. 
                      You can select different variables to map and also search for specific FIPS codes to get more detailed information."),
                      br(),
                      br()
               )
             ),
             
             fluidRow(
               column(5, offset = 1,
                      br(),
                      br(),
                      tags$img(src = "wallpaper2.png", width = "100%", height = "auto")
               ),
               column(5,
                      h3("More About ADI, SVI, and Weather Risk"),
                      p("Geographical social determinants of health, such as the Area Deprivation Index (ADI) and the Social Vulnerability Index (SVI), significantly influence surgical outcomes and access to surgical care. The SVI assesses a community's vulnerability based on factors like socioeconomic status, household composition, minority status, and housing type, which can further exacerbate disparities in surgical care. In regions with high SVI, patients often encounter barriers such as limited access to surgical facilities, reduced healthcare resources, and longer delays in receiving care. Additionally, weather, climate, and disaster risks pose substantial challenges, particularly in vulnerable areas where infrastructure may be inadequate to withstand extreme events. Natural disasters can disrupt surgical services, delay procedures, and impede patient access to care, leading to worsened health outcomes. Addressing these disparities requires a multifaceted approach that includes strengthening healthcare infrastructure, improving emergency preparedness, and tailoring interventions to address the compounded risks associated with social vulnerability and environmental factors.")
               )
             ),
             
             fluidRow(
               column(4, offset = 1,
                      h3("Additional Information"),
                      p("This application was developed as part of a project to better understand and visualize the interplay between social, economic, and environmental factors in Texas, New Mexico, and Oklahoma. The data used in this application is sourced from reputable public datasets."),
                      br(),
                      p("Using integrated census data to quantify risk is a powerful tool for improving health and surgical outcomes, particularly in vulnerable populations. By combining data from sources such as the Census Bureau with health records and socioeconomic indicators like the Area Deprivation Index (ADI) and Social Vulnerability Index (SVI), healthcare providers and policymakers can identify communities at higher risk for poor health outcomes due to factors like poverty, limited access to healthcare, and environmental hazards. This data-driven approach allows for the identification of geographic and demographic disparities in surgical care, enabling targeted interventions to address the specific needs of at-risk populations. For example, areas with high ADI or SVI scores may benefit from increased healthcare resources, community outreach programs, or the placement of specialized surgical services to mitigate the barriers these communities face. Additionally, integrating census data with health outcomes allows for more accurate predictions of surgical risks, leading to more personalized care plans and better resource allocation, ultimately enhancing both individual and community health outcomes.")
               ),
               column(6,
                      tags$img(src = "wallpaper3.png", width = "100%", height = "auto")
               )
             ),
             
             fluidRow(
               column(5, offset = 1,
                      br(),
                      br(),
                      tags$img(src = "wallpaper4.png", width = "100%", height = "auto")
               ),
               column(5,
                      h3("Geocoding / Crosswalking"),
                      p("Geographical social determinants of health, such as the Area Deprivation Index (ADI) and the Social Vulnerability Index (SVI), significantly influence surgical outcomes and access to surgical care. The SVI assesses a community's vulnerability based on factors like socioeconomic status, household composition, minority status, and housing type, which can further exacerbate disparities in surgical care. In regions with high SVI, patients often encounter barriers such as limited access to surgical facilities, reduced healthcare resources, and longer delays in receiving care. Additionally, weather, climate, and disaster risks pose substantial challenges, particularly in vulnerable areas where infrastructure may be inadequate to withstand extreme events. Natural disasters can disrupt surgical services, delay procedures, and impede patient access to care, leading to worsened health outcomes. Addressing these disparities requires a multifaceted approach that includes strengthening healthcare infrastructure, improving emergency preparedness, and tailoring interventions to address the compounded risks associated with social vulnerability and environmental factors.")
               )
             )
             
    ),
    
    tabPanel("Clinician Tool",
             fluidPage(
               fluidRow(
                 br(), br(), br(), br(), br(), br(),
                 column(6, offset = 3, align = "center",
                        textInput("zip_input_2", "Enter 5-digit ZIP code:", placeholder = "e.g., 79430", width = "250px"),
                        br(),
                        actionButton("submit_zip_2", "Submit", class = "btn-primary")
                 )
               ),
               br(), br(), br(), br(),
               fluidRow(
                 column(12, offset = 1, align = "center",
                        fluidRow(
                          column(2, gaugeOutput("gauge1")),
                          column(2, gaugeOutput("gauge2")),
                          column(2, gaugeOutput("gauge3")),
                          column(2, gaugeOutput("gauge4")),
                          column(2, gaugeOutput("gauge5"))
                        ),
                        fluidRow(
                          column(2, gaugeOutput("gauge6")),
                          column(2, gaugeOutput("gauge7")),
                          column(2, gaugeOutput("gauge8")),
                          column(2, gaugeOutput("gauge9")),
                          column(2, gaugeOutput("gauge10"))
                        )
                 )
               )
             )
    ),
    
    tabPanel("Research Tool",
             sidebarLayout(
               sidebarPanel(
                 textInput("zip_input", "Enter ZIP code(s):", placeholder = "e.g., 79430, 73001, 73002"),
                 actionButton("submit_zip", "Submit"),
                 br(),
                 selectInput("x_axis", "Select X-Axis Variable:", choices = NULL),
                 selectInput("y_axis", "Select Y-Axis Variable:", choices = NULL),
                 selectInput("point_size", "Select Point Size Variable:", choices = c("Fixed Size", NULL)),
                 selectInput("point_color", "Select Point Color Variable:", choices = c("Fixed Color", NULL)),
                 sliderInput("size_range", "Adjust Point Size Range:", min = 0.5, max = 5, value = c(1, 3)),
                 actionButton("submit_plot", "Generate Plot")
               ),
               mainPanel(
                 DTOutput("zip_table"), 
                 plotlyOutput("scatter_plot")
               )
             )
    ),
    
    tabPanel("Mapping Tool",
             sidebarLayout(
               sidebarPanel(
                 textInput("zip_input", "Enter ZIP code(s):", placeholder = "e.g., 79430, 73001, 73002"),
                 actionButton("submit_zip", "Submit", class = "btn-primary"),
                 br(),
                 selectInput("mapping_variable", "Select Variable to Map:", 
                             choices = c(
                               'ADI National Rank',
                               'ADI State Rank',
                               'Social Vulnerability Index',
                               'Socioeconomic Status',
                               'Household Characteristics',
                               'Racial & Ethnic Minority Status',
                               'Housing Type & Transportation',
                               'National Risk Index (FEMA)', 
                               'Social Vulnerability Score',
                               'Community Resilience Score',
                               'Miles to UMC'
                             )),
                 radioButtons("plot_type", "Select Plot Type:",
                              choices = c("Heat Map" = "heatmap", "Scatter Plot" = "scatter"))
               ),
               mainPanel(
                 leafletOutput("mappingMap", height = "600px")
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
                        tags$li("Distance Index to University Medical Center: ..."),
                        tags$li(" ...")
                      ),
                      p("For more information, please visit the relevant links."),
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
  
  # Reactive expression to filter data for selected ZIP codes
  filtered_data_reactive <- reactive({
    req(input$zip_input)
    zip_list <- strsplit(input$zip_input, ",\\s*")[[1]]
    ton_master_zip %>%
      filter(ZIP %in% zip_list) %>%
      mutate_if(is.numeric, round, digits = 1)
  })
  
  # Update variable choices for scatter plot based on filtered data
  observeEvent(filtered_data_reactive(), {
    data <- filtered_data_reactive()
    updateSelectInput(session, "x_axis", choices = names(data))
    updateSelectInput(session, "y_axis", choices = names(data))
    updateSelectInput(session, "point_size", choices = c("Fixed Size", names(data)))
    updateSelectInput(session, "point_color", choices = c("Fixed Color", names(data)))
  })
  
  # Render the table in the "Research Tool" tab
  observeEvent(input$submit_zip, {
    output$zip_table <- renderDT({
      datatable(filtered_data_reactive())
    })
  })
  
  # Render the interactive scatter plot based on user inputs
  observeEvent(input$submit_plot, {
    req(input$x_axis, input$y_axis)
    plot_data <- filtered_data_reactive()
    
    plot_size <- if (input$point_size == "Fixed Size") {
      rep(5, nrow(plot_data))
    } else {
      scales::rescale(plot_data[[input$point_size]], to = input$size_range)
    }
    
    plot_color <- if (input$point_color == "Fixed Color") {
      rep("blue", nrow(plot_data))
    } else {
      plot_data[[input$point_color]]
    }
    
    output$scatter_plot <- renderPlotly({
      plot_ly(data = plot_data, 
              x = ~get(input$x_axis), 
              y = ~get(input$y_axis), 
              size = plot_size, 
              color = plot_color,
              text = ~paste("ZIP:", ZIP),
              type = 'scatter',
              mode = 'markers',
              colors = "RdYlBu",
              marker = list(sizemode = 'diameter', 
                            sizeref = 2 * max(plot_size) / (input$size_range[2] ^ 2),
                            sizemin = input$size_range[1],
                            sizemax = input$size_range[2],
                            line = list(color = 'black', width = 1.5))) %>%
        layout(title = paste("Scatter plot of", input$x_axis, "vs", input$y_axis),
               xaxis = list(title = input$x_axis, zeroline = TRUE, zerolinecolor = 'black', zerolinewidth = 0.5,
                            range = c(0, max(plot_data[[input$x_axis]], na.rm = TRUE) * 1.1)),
               yaxis = list(title = input$y_axis, zeroline = TRUE, zerolinecolor = 'black', zerolinewidth = 0.5,
                            range = c(0, max(plot_data[[input$y_axis]], na.rm = TRUE) * 1.1)))
    })
  })
  
  # Reactive expression to fetch the relevant data for the entered ZIP code in Clinician Tool
  zip_data_reactive <- reactive({
    req(input$zip_input_2)
    ton_master_zip %>%
      filter(ZIP == input$zip_input_2)
  })
  
  output$gauge1 <- renderGauge({
    data <- zip_data_reactive()
    gauge(data$`ADI National Rank`, 
          min = 0, max = 100, 
          label = "\nADI National Rank", 
          gaugeSectors(success = c(75, 100), warning = c(25, 75), danger = c(0, 25)))
  })
  
  output$gauge2 <- renderGauge({
    data <- zip_data_reactive()
    gauge(data$`ADI State Rank`, 
          min = 0, max = 10, 
          label = "\nADI State Rank", 
          gaugeSectors(success = c(7.5, 10), warning = c(2.5, 7.5), danger = c(0, 2.5)))
  })
  
  output$gauge3 <- renderGauge({
    data <- zip_data_reactive()
    gauge(data$`Social Vulnerability Index`, 
          min = 0, max = 1, 
          label = "\nSocial Vulnerability Index", 
          gaugeSectors(success = c(0.75, 1), warning = c(0.25, 0.75), danger = c(0, 0.25)))
  })
  
  output$gauge4 <- renderGauge({
    data <- zip_data_reactive()
    gauge(data$`Socioeconomic Status`, 
          min = 0, max = 1, 
          label = "\nSocioeconomic Status", 
          gaugeSectors(success = c(0.75, 1), warning = c(0.25, 0.75), danger = c(0, 0.25)))
  })
  
  output$gauge5 <- renderGauge({
    data <- zip_data_reactive()
    gauge(data$`Household Characteristics`, 
          min = 0, max = 1, 
          label = "\nHousehold Characteristics", 
          gaugeSectors(success = c(0.75, 1), warning = c(0.25, 0.75), danger = c(0, 0.25)))
  })
  
  output$gauge6 <- renderGauge({
    data <- zip_data_reactive()
    gauge(data$`Racial & Ethnic Minority Status`, 
          min = 0, max = 1, 
          label = "\nRacial & Ethnic Minority Status", 
          gaugeSectors(success = c(0.75, 1), warning = c(0.25, 0.75), danger = c(0, 0.25)))
  })
  
  output$gauge7 <- renderGauge({
    data <- zip_data_reactive()
    gauge(data$`Housing Type & Transportation`, 
          min = 0, max = 1, 
          label = "\nHousing Type & Transportation", 
          gaugeSectors(success = c(0.75, 1), warning = c(0.25, 0.75), danger = c(0, 0.25)))
  })
  
  output$gauge8 <- renderGauge({
    data <- zip_data_reactive()
    gauge(data$`National Risk Index (FEMA)`, 
          min = 0, max = 100, 
          label = "\nNational Risk Index (FEMA)", 
          gaugeSectors(success = c(75, 100), warning = c(25, 75), danger = c(0, 25)))
  })
  
  output$gauge9 <- renderGauge({
    data <- zip_data_reactive()
    gauge(data$`Social Vulnerability Score`, 
          min = 0, max = 100, 
          label = "\nSocial Vulnerability Score", 
          gaugeSectors(success = c(75, 100), warning = c(25, 75), danger = c(0, 25)))
  })
  
  output$gauge10 <- renderGauge({
    data <- zip_data_reactive()
    gauge(data$`Community Resilience Score`, 
          min = 0, max = 100, 
          label = "\nCommunity Resilience Score", 
          gaugeSectors(success = c(75, 100), warning = c(25, 75), danger = c(0, 25)))
  })
  
  
  # Reactive expression to get the selected data for mapping
  map_data_reactive <- reactive({
    req(input$mapping_variable)
    zip_list <- strsplit(input$zip_input, ",\\s*")[[1]]
    ton_master_zip %>%
      filter(ZIP %in% zip_list) %>%
      select(ZIP, Latitude, Longitude, all_of(input$mapping_variable)) %>%
      filter(!is.na(.data[[input$mapping_variable]]))
  })
  
  # Render the map
  output$mappingMap <- renderLeaflet({
    data <- map_data_reactive()
    
    pal <- colorNumeric("RdYlBu", domain = data[[input$mapping_variable]], na.color = "gray", reverse = TRUE)
    
    map <- leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron)
    
    if (input$plot_type == "scatter") {
      map <- map %>%
        addCircles(
          lng = ~Longitude, lat = ~Latitude, 
          fillColor = ~pal(data[[input$mapping_variable]]), 
          color = "black", 
          opacity = 1, 
          fillOpacity = 1, 
          radius = 5000,
          stroke = TRUE, 
          weight = 1,
          popup = ~paste("ZIP Code: ", ZIP, "<br>",
                         input$mapping_variable, ": ", data[[input$mapping_variable]])
        )
    } else if (input$plot_type == "heatmap") {
      if (!input$mapping_variable %in% names(zip_boundaries)) {
        showNotification("Selected variable is not available in ZIP boundaries data.", type = "error")
        return()
      }
      
      map <- map %>%
        addPolygons(
          data = zip_boundaries, 
          fillColor = ~pal(zip_boundaries[[input$mapping_variable]]), 
          color = "white", 
          weight = 1,
          fillOpacity =1,
          opacity = 1, 
          popup = ~paste("ZIP Code: ", zip_boundaries$ZCTA5CE10, "<br>",
                         input$mapping_variable, ": ", zip_boundaries[[input$mapping_variable]])
        )
    }
    
    map <- map %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = data[[input$mapping_variable]],
        title = input$mapping_variable,
        opacity = 1
      )
    
    map
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

