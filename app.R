library(shiny)
library(shinydashboard)
library(bslib)
library(DT)
library(leaflet)
library(sf)
library(tigris)
library(maps)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(zipcodeR)
library(tidycensus)

# Source global utilities
source("global.R")

# Define UI
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C3E50",
    secondary = "#34495E"
  ),
  
  # Header
  headerPanel(
    title = div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      div(
        style = "font-size: 24px; font-weight: bold;",
        "US ZIP Code Density Map"
      ),
      div(
        fileInput(
          "file1",
          "Upload CSV",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          ),
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        )
      )
    )
  ),
  
  # Main content
  fluidRow(
    # Map panel (75% width)
    column(
      width = 9,
      div(
        style = "height: calc(100vh - 120px);",
        leafletOutput("map", height = "100%")
      )
    ),
    
    # Analytics panel (25% width)
    column(
      width = 3,
      div(
        style = "height: calc(100vh - 120px); overflow-y: auto;",
        # Summary statistics
        div(
          class = "panel panel-default",
          div(
            class = "panel-heading",
            h4("Summary Statistics")
          ),
          div(
            class = "panel-body",
            verbatimTextOutput("summary_stats")
          )
        ),
        
        # Top 10 areas
        div(
          class = "panel panel-default",
          div(
            class = "panel-heading",
            h4("Top 10 Areas by Density")
          ),
          div(
            class = "panel-body",
            DTOutput("top_areas")
          )
        ),
        
        # Export options
        div(
          class = "panel panel-default",
          div(
            class = "panel-heading",
            h4("Export Options")
          ),
          div(
            class = "panel-body",
            downloadButton("download_data", "Download Processed Data"),
            downloadButton("download_map", "Export Map Image")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values for storing data
  values <- reactiveValues(
    zip_data = NULL,
    processed_data = NULL,
    map_data = NULL,
    city_boundaries = NULL
  )
  
  # Initialize city boundaries
  observe({
    values$city_boundaries <- get_city_boundaries()
  })
  
  # File upload handling
  observeEvent(input$file1, {
    req(input$file1)
    
    # Check file size
    if (input$file1$size > MAX_FILE_SIZE) {
      showNotification(error_messages$file_too_large, type = "error")
      return()
    }
    
    # Read and validate CSV
    tryCatch({
      # Read the CSV file - first column is zip, second is state
      df <- read.csv(input$file1$datapath, header = FALSE, col.names = c("zip_code", "state"), stringsAsFactors = FALSE)
      
      # Process ZIP codes (now with state)
      processed_df <- df %>%
        mutate(
          zip_code = str_pad(zip_code, 5, pad = "0"),
          is_valid = validate_zip_code(zip_code)
        ) %>%
        filter(is_valid) %>%
        group_by(zip_code, state) %>%
        summarise(count = n(), .groups = "drop") %>%
        arrange(desc(count))
      
      if (nrow(processed_df) == 0) {
        showNotification(error_messages$invalid_zip, type = "error")
        return()
      }
      
      # Update reactive values
      values$zip_data <- df
      values$processed_data <- processed_df
      
      # State abbreviation to FIPS code lookup
      state_abbr_to_fips <- c(
        AL = "01", AK = "02", AZ = "04", AR = "05", CA = "06", CO = "08", CT = "09", DE = "10", FL = "12", GA = "13",
        HI = "15", ID = "16", IL = "17", IN = "18", IA = "19", KS = "20", KY = "21", LA = "22", ME = "23", MD = "24",
        MA = "25", MI = "26", MN = "27", MS = "28", MO = "29", MT = "30", NE = "31", NV = "32", NH = "33", NJ = "34",
        NM = "35", NY = "36", NC = "37", ND = "38", OH = "39", OK = "40", OR = "41", PA = "42", RI = "44", SC = "45",
        SD = "46", TN = "47", TX = "48", UT = "49", VT = "50", VA = "51", WA = "53", WV = "54", WI = "55", WY = "56"
      )

      extract_city_state <- function(post_office_city) {
        parts <- strsplit(post_office_city, ", ")
        city <- sapply(parts, `[`, 1)
        state <- sapply(parts, `[`, 2)
        data.frame(city_name = city, state_abbr = state, stringsAsFactors = FALSE)
      }

      # Get ZIP to city mapping and build robust city/state mapping
      zip_mapping <- get_zip_to_city_mapping()
      city_state_df <- extract_city_state(zip_mapping$post_office_city)
      zip_mapping$city_name <- city_state_df$city_name
      zip_mapping$state_abbr <- city_state_df$state_abbr
      zip_mapping$STATEFP <- state_abbr_to_fips[zip_mapping$state_abbr]
      
      # Join on both zip_code and state_abbr to get city info
      values$map_data <- processed_df %>%
        left_join(zip_mapping, by = c("zip_code" = "zipcode", "state" = "state_abbr")) %>%
        group_by(city_name, STATEFP) %>%
        summarise(
          zip_count = n(),
          total_count = sum(count, na.rm = TRUE),
          avg_count = mean(count, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(total_count))
      
      showNotification(success_messages$data_processed, type = "message")
      
    }, error = function(e) {
      showNotification(
        paste(error_messages$processing_error, ":", e$message),
        type = "error"
      )
    })
  })
  
  # Map output
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
      addLayersControl(
        position = "bottomright",
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Update map when data changes
  observeEvent(values$map_data, {
    req(values$map_data, values$city_boundaries)
    
    # Join city boundaries with map data (using NAME and state abbreviation)
    filtered_boundaries <- values$city_boundaries %>%
      left_join(values$map_data, by = c("NAME" = "city_name", "STATEFP" = "STATEFP")) %>%
      filter(!is.na(total_count))
    
    pal <- colorNumeric(
      palette = map_style$density_colors,
      domain = filtered_boundaries$total_count
    )
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(
        data = filtered_boundaries,
        fillColor = ~pal(total_count),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2,
          color = map_style$highlight_color,
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste(
          NAME, STATEFP,
          "\nTotal occurrences:", total_count,
          "\nUnique ZIP codes:", zip_count,
          "\nAverage per ZIP:", round(avg_count, 1)
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = filtered_boundaries$total_count,
        title = "Total Occurrences",
        opacity = 0.7
      )
  })
  
  # Summary statistics output
  output$summary_stats <- renderPrint({
    if (is.null(values$processed_data)) {
      cat("Upload a CSV file to see summary statistics")
    } else {
      cat("Summary Statistics\n")
      cat("-----------------\n")
      cat("Total ZIP codes:", nrow(values$processed_data), "\n")
      cat("Unique cities:", nrow(values$map_data), "\n")
      cat("States covered:", length(unique(values$map_data$state)), "\n")
      cat("Average ZIP codes per city:", 
          round(mean(values$map_data$zip_count), 2), "\n")
    }
  })
  
  # Top 10 areas table
  output$top_areas <- renderDT({
    if (is.null(values$map_data)) {
      return(NULL)
    }
    
    top_areas <- values$map_data %>%
      arrange(desc(total_count)) %>%
      head(10) %>%
      select(
        City = city_name,
        State = STATEFP,
        `ZIP Count` = zip_count,
        `Total Count` = total_count
      )
    
    datatable(
      top_areas,
      options = list(
        pageLength = 10,
        searching = TRUE,
        ordering = TRUE
      )
    )
  })
  
  # Download handlers
  output$download_data <- downloadHandler(
    filename = function() {
      paste("processed_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$map_data, file, row.names = FALSE)
    }
  )
  
  output$download_map <- downloadHandler(
    filename = function() {
      paste("map_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Save map as PNG
      mapshot(
        leafletProxy("map"),
        file = file,
        vwidth = 1200,
        vheight = 800
      )
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)