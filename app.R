library(shiny)
library(RSQLite)
library(zip)
library(DBI)
library(dplyr)
library(ggplot2)
library(shinycssloaders)
library(shinyjs)

# Define constants
zip_url <- "https://www.adeq.state.ar.us/downloads/WebDatabases/TechnicalServices/WQARWebLIMS/WQARWebLIMS_web.zip"
fallback_data_path <- "Data/cleaned_agfc_lakes.csv"
temp_dir <- tempdir()
#temp_dir <- "Data/Test"

# Define UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  div(
    id = "loading-overlay",
    style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; 
           background-color: rgba(255, 255, 255, 0.8); z-index: 1000; 
           display: none; align-items: center; justify-content: center;",
    h3(id = "loading-message", "Loading data, please wait...", style = "color: #555;")
  ),
  titlePanel("Water Quality Data Viewer"),
  mainPanel(
    withSpinner(plotOutput("plot"))  # Spinner while plot is rendering
  )
)


# Define server logic
server <- function(input, output, session) {
  
  # Reactive function to fetch and clean data
  fetch_and_clean_data <- reactive({
   
    # Show loading overlay
    runjs('document.getElementById("loading-overlay").style.display = "flex";')
    runjs('document.getElementById("loading-message").textContent = "Downloading data, please wait...";')
    
    # Attempt to download and unzip the file
    zip_path <- file.path(temp_dir, "data.zip")
    download_success <- FALSE
    unzip_success <- FALSE
    clean_data <- NULL
    
    
     tryCatch({
      # Download the zip file
      download.file(zip_url, zip_path, mode = "wb")
      download_success <- TRUE
      
      # Update the loading message to indicate unzipping
      runjs('document.getElementById("loading-message").textContent = "Extracting data...";')
      
      # Unzip the file
      unzip(zip_path, exdir = temp_dir)
      access_file <- list.files(temp_dir, pattern = "\\.mdb$", full.names = TRUE)[1]
      
      # Check if the database file exists
      if (length(access_file) > 0) {
        unzip_success <- TRUE
        
        # Update the loading message to indicate data cleaning
        runjs('document.getElementById("loading-message").textContent = "Cleaning data...";')
      
      # Connect to the Access database
      con <- dbConnect(odbc::odbc(), .connection_string = paste(
        "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
        "Dbq=", access_file, ";",
        "Uid=Admin;Pwd=;", sep = ""
      ))
      
      # Read only relevant data and do minimal cleaning:
      query <- "SELECT SamplingPoint, DateSampled, FinalResult
                FROM WebLIMSResults
                WHERE SamplingPoint = 'LOUA015A'
                AND WebParameter = 'Depth, Secchi disk depth (m)'"
      
      raw_data <- dbGetQuery(con, query)
    
        clean_data <- raw_data %>%
        mutate(DateSampled = as.Date(DateSampled),
               FinalResult = as.numeric(FinalResult))
      
      dbDisconnect(con)
      
      }
      
    }, error = function(e) {
      # Fallback to pre-cleaned data
      message("Error fetching data. Using fallback data.")

      
    })
    # If either download or unzip failed, load fallback data
    if (!download_success || !unzip_success || is.null(clean_data)) {
      # Update the loading message to indicate fallback
      runjs('document.getElementById("loading-message").textContent = "Using fallback data...";')
      
       cleaned_agfc_lakes <- read.csv(fallback_data_path)
      
      cleaned_agfc_lakes <- cleaned_agfc_lakes %>% 
        filter(SamplingPoint == "LOUA024A") %>% 
        filter(WebParameter == "Depth, Secchi disk depth (m)")
      
      cleaned_agfc_lakes
    }
    
    # Hide loading overlay
    runjs('document.getElementById("loading-overlay").style.display = "none";')
    
    clean_data
  })
  
  # Use the cleaned data for plotting or analysis
  output$plot <- renderPlot({
    data <- fetch_and_clean_data()
    ggplot(data, aes(x = DateSampled, y = FinalResult)) +
      geom_point() +
      ggtitle(data$SamplingPoint[1])
  })
  
}


# Run the app
shinyApp(ui = ui, server = server)
