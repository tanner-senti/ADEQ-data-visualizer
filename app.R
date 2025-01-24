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
fallback_data_path <- "Data/cleaned_agfc_lakes.csv" # Update this with better database
temp_dir <- tempdir()

# Define UI
ui <- fluidPage(
  useShinyjs(), #Shinyjs for data loading panels
  div(
    id = "loading-overlay",
    style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; 
           background-color: rgba(255, 255, 255, 0.8); z-index: 1000; 
           display: none; align-items: center; justify-content: center;",
    h3(id = "loading-message", "Loading data, please wait...", style = "color: #555;")
  ),
  titlePanel("Water Quality Data Viewer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("site", "Select Site", choices = NULL),  # Dropdown for site selection
      selectInput("parameter", "Select Parameter", choices = NULL)  # Dropdown for parameter selection
    ),
  mainPanel(
    div(
      style = "width: 60%; margin-lef: 0;",
    withSpinner(plotOutput("plot", height = "400px"))  # Spinner while plot is rendering
  )
  )
)
)


# Define server logic
server <- function(input, output, session) {
  
  # Step 1: Fetch and unzip data
  fetch_and_unzip_data <- reactive({
    runjs('document.getElementById("loading-overlay").style.display = "flex";')
    runjs('document.getElementById("loading-message").textContent = "Downloading data, please wait...";')
    
    zip_path <- file.path(temp_dir, "data.zip")
    download_success <- FALSE
    unzip_success <- FALSE
    
    tryCatch({
      # Download the zip file
      download.file(zip_url, zip_path, mode = "wb")
      download_success <- TRUE
      
      # Extract zip
      runjs('document.getElementById("loading-message").textContent = "Extracting data...";')
      unzip(zip_path, exdir = temp_dir)
      access_file <- list.files(temp_dir, pattern = "\\.mdb$", full.names = TRUE)[1]
      
      if (length(access_file) > 0) {
        unzip_success <- TRUE
      }
    }, error = function(e) {
      message("Error fetching or unzipping data.")
    })
    
    if (!download_success || !unzip_success) {
      runjs('document.getElementById("loading-message").textContent = "Using fallback data...";')
      cleaned_agfc_lakes <- read.csv(fallback_data_path)
      return(cleaned_agfc_lakes)
    }
    
    runjs('document.getElementById("loading-overlay").style.display = "none";')  # Hide overlay after success
    access_file
  })
  
  # Step 2: Pull a list of sites from SamplingPoint
  get_sites_from_db <- reactive({
    access_file <- fetch_and_unzip_data()
    if (is.character(access_file)) {
      con <- dbConnect(odbc::odbc(), .connection_string = paste(
        "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
        "Dbq=", access_file, ";",
        "Uid=Admin;Pwd=;", sep = ""
      ))
      
      query <- "SELECT DISTINCT SamplingPoint FROM WebLIMSResults"
      site_names <- dbGetQuery(con, query)
      dbDisconnect(con)
      
      return(site_names$SamplingPoint)
    }
    return(NULL)
  })
  
  # Step 3: Populate dropdown with site options
  observe({
    site_names <- get_sites_from_db()
    updateSelectInput(session, "site", choices = site_names)
  })
  
  # Step 4: Dynamically update parameter options based on site selection
  observeEvent(input$site, {
    req(input$site)  # Ensure the site is selected
    
    # Fetch parameters for the selected site
    access_file <- fetch_and_unzip_data()
    if (is.character(access_file)) {
      con <- dbConnect(odbc::odbc(), .connection_string = paste(
        "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
        "Dbq=", access_file, ";",
        "Uid=Admin;Pwd=;", sep = ""
      ))
      
      query <- paste("SELECT DISTINCT WebParameter FROM WebLIMSResults WHERE SamplingPoint = '", input$site, "'", sep = "")
      parameters <- dbGetQuery(con, query)
      dbDisconnect(con)
      
      updateSelectInput(session, "parameter", choices = parameters$WebParameter)
    }
  })
  
  # Step 5: Query data for the selected site and parameter, then plot
  output$plot <- renderPlot({
    req(input$site, input$parameter)  # Ensure both site and parameter are selected
    
    runjs('document.getElementById("loading-message").textContent = "Querying data...";')
    
    access_file <- fetch_and_unzip_data()
    clean_data <- NULL
    
    if (is.character(access_file)) {
      con <- dbConnect(odbc::odbc(), .connection_string = paste(
        "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
        "Dbq=", access_file, ";",
        "Uid=Admin;Pwd=;", sep = ""
      ))
      
      query <- paste("SELECT SamplingPoint, DateSampled, FinalResult, Qualifiers
                      FROM WebLIMSResults
                      WHERE SamplingPoint = '", input$site, "' AND WebParameter = '", input$parameter, "'", sep = "")
      raw_data <- dbGetQuery(con, query)
      
      # Data cleaning here:
      clean_data <- raw_data %>%
        mutate(DateSampled = as.Date(DateSampled))
      # Handle detection limits:
      # Data below DL (<) become half the value with "BDL"
      # Data greater than DL (>) retain value with qualifier "ADL"
      clean_data <- clean_data %>%
        mutate(
          DL = case_when(
            grepl("^>", FinalResult) ~ ">DL",
            grepl("^<", FinalResult) ~ "<DL",
            TRUE ~ "Measured value"
          ),
          FinalResult = case_when(
            grepl("^>", FinalResult) ~ as.numeric(sub(">", "", FinalResult)),
            grepl("^<", FinalResult) ~ as.numeric(sub("<", "", FinalResult)) /
              2,
            TRUE ~ as.numeric(FinalResult)
          ),
          Qualifiers = case_when(
            Qualifiers == "" ~ "None",
            TRUE ~ Qualifiers
          )
        )
      
      clean_data <- clean_data %>% 
        mutate(across(c(Qualifiers, DL), as.factor))
      
      
      dbDisconnect(con)
    }
    
    runjs('document.getElementById("loading-overlay").style.display = "none";')  # Hide overlay after plot data is ready
    
    # Plotting:
    ggplot(clean_data, aes(x = DateSampled, y = FinalResult)) +
      geom_point(aes(color = Qualifiers, shape = DL),
                 alpha = 0.7,
                 size = 2.5) +
      theme_classic(base_size = 14) +
      ggtitle(paste(input$site, "-", input$parameter)) +
      labs(
        #title = paste(filtered_data$Samplingpoint[1], param_group),
        x = "Date",
        y = "Result",
        color = "Qualifiers",
        shape = "Values"
      )
  })
}


# Run the app
shinyApp(ui = ui, server = server)
