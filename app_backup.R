library(shiny)
library(duckdb)
library(zip)
library(DBI)
library(dplyr)
library(ggiraph)
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(DT)

# This app will work locally on a windows machine or hosted on a microsoft server
# with windows. 

# If hosted on shinyapps (linux), will default to backup duckdb database (cannot pull and
 # work with access databases)

# This is an inefficient version of the app that works locally, outside ADEQ servers.
# IF hosted on a linux server, the app should still work but will use the data backup.

# Define constants
zip_url <- "https://www.adeq.state.ar.us/downloads/WebDatabases/TechnicalServices/WQARWebLIMS/WQARWebLIMS_web.zip"
fallback_data_path <- "Data/WebLIMS_backup.duckdb" # Update this with better database
temp_dir <- tempdir()


# Define UI
ui <- fluidPage(
  useShinyjs(), # Shinyjs for data loading panels
  div(
    id = "loading-overlay",
    style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; 
           background-color: rgba(255, 255, 255, 0.8); z-index: 1000; 
           display: none; align-items: center; justify-content: center;",
    h3(id = "loading-message", "Loading data, please wait...", style = "color: #555;")
  ),
  
  # Centering the content
  div(
    style = "display: flex; flex-direction: column; align-items: center; justify-content: center; width: 80%; margin: 0 auto;",
    
    titlePanel("Water Quality Data Viewer"),
    HTML('<h4 style="text-align: center;">This is an unofficial web app developed to view Water Quality Monitoring Data',
         ' from the Arkansas Department of Energy and Environment.',
         ' <a href="https://www.adeq.state.ar.us/techsvs/env_multi_lab/water_quality_station.aspx#Display" 
         target="_blank">Click here for more information and data availability.</a></h4>'),
    
    # Message about the database being used
    htmlOutput("db_message"),
    br(),
    
    # Input panel with site and parameter selection horizontally
    div(
      style = "display: flex; justify-content: center; gap: 10px;",
      selectizeInput("site", "Search by SiteID:", choices = NULL, multiple = FALSE, 
                     options = list(placeholder = "Search...", maxOptions = 1000), 
                     width = "200px"),
      selectizeInput("description", "Search by description:", choices = NULL, multiple = FALSE, 
                     options = list(placeholder = "Search...", maxOptions = 1000)),
      selectizeInput("parameter", "Search or select a parameter:", choices = NULL, multiple = FALSE, 
                     options = list(placeholder = "Search...", maxOptions = 1000), 
                     width = "200px")
    ),
    
    # Plot panel
    div(
      style = "width: 80%; margin-left: 0;",
      withSpinner(girafeOutput("plot", height = "600px"))  # Spinner while plot is rendering
    ),
    
    # Table under the single plot:
    div(
      style = "width: 85%; margin-top: 20px; max-width: 700px",
      DT::DTOutput("data_table")  # This will render the table
    ),
    
    h5("Developed by Tanner Senti (2025)")
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Step 1: Fetch and unzip data - also check for linux system
  fetch_and_unzip_data <- reactive({
    runjs('document.getElementById("loading-overlay").style.display = "flex";')
    runjs('document.getElementById("loading-message").textContent = "Downloading data, please wait...";')
    
    zip_path <- file.path(temp_dir, "data.zip")
    download_success <- FALSE
    unzip_success <- FALSE
    database_file <- NULL
    db_message <- NULL
    
    is_linux <- Sys.info()[["sysname"]] == "Linux"
    
    if (!is_linux) {
      tryCatch({
        # Download the zip file
        download.file(zip_url, zip_path, mode = "wb")
        download_success <- TRUE
        
        # Extract zip
        runjs('document.getElementById("loading-message").textContent = "Extracting data...";')
        unzip(zip_path, exdir = temp_dir)
        database_file <- list.files(temp_dir, pattern = "\\.mdb$", full.names = TRUE)[1]
        
        if (length(database_file) > 0) {
          unzip_success <- TRUE
        }
      }, error = function(e) {
        message("Error fetching or unzipping data.")
      })
    }
    
    if (is_linux || !download_success || !unzip_success) {
      runjs('document.getElementById("loading-message").textContent = "Using fallback data...";')
      # Use SQLite if no Access database is available
      database_file <- fallback_data_path
      # Fetch the date range from SQLite
      conn_sqlite <- dbConnect(duckdb(), database_file)
      date_range <- dbGetQuery(conn_sqlite, "SELECT MIN(DateSampled) AS min_date, MAX(DateSampled) AS max_date FROM WebLIMSResults")
      # FIX the leading/trailing spaces for SQLITE here:
      # Run update queries to trim spaces
      dbExecute(conn_sqlite, "UPDATE WebLIMSResults SET SamplingPoint = TRIM(SamplingPoint);")
      dbExecute(conn_sqlite, "UPDATE WebLIMSStations SET StationID = TRIM(StationID);")
      dbDisconnect(conn_sqlite)
      
      db_message <- paste("Using backup database - data available between ", date_range$min_date, " and ", date_range$max_date)
    } else {
      # Fetch the TempUpdated date from Access database
      conn_access <- dbConnect(odbc::odbc(), .connection_string = paste(
        "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
        "Dbq=", database_file, ";",
        "Uid=Admin;Pwd=;", sep = ""
      ))
      temp_updated_date <- dbGetQuery(conn_access, "SELECT Updated FROM TempUpdated")
      date_range <- dbGetQuery(conn_access, "SELECT MIN(DateSampled) AS min_date, MAX(DateSampled) AS max_date FROM WebLIMSResults")
      
      # Removing leading/trailing spaces here!!!
      # Run update queries to trim spaces
      dbExecute(conn_access, "UPDATE WebLIMSResults SET SamplingPoint = Trim(SamplingPoint);")
      dbExecute(conn_access, "UPDATE WebLIMSStations SET StationID = Trim(StationID);")
      dbDisconnect(conn_access)
      
      db_message <- paste("Using most recent version of the database uploaded on ", format(temp_updated_date$Updated, "%m-%d-%Y"),
                          "<br>", "Data available between ", format(date_range$min_date, "%m-%d-%Y"), " and ", format(date_range$max_date, "%m-%d-%Y"))
    }
    
    runjs('document.getElementById("loading-overlay").style.display = "none";')  # Hide overlay after success
    output$db_message <- renderUI({
      HTML(db_message)
    })
    
    database_file
  })
  
  # Step 2: Pull a list of sites from SamplingPoint
  get_sites_from_db <- reactive({
    database_file <- fetch_and_unzip_data()
    con <- NULL
    
    if (endsWith(database_file, ".mdb")) {
      # Connect to Access database
      con <- dbConnect(odbc::odbc(), .connection_string = paste(
        "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
        "Dbq=", database_file, ";",
        "Uid=Admin;Pwd=;", sep = ""
      ))
    } else {
      # Connect to SQLite database (fallback)
      con <- dbConnect(duckdb(), database_file)
    }
    
    if (!is.null(con)) {
      query <- "SELECT DISTINCT SamplingPoint, WebLIMSStations.Description
                FROM WebLIMSResults
                LEFT JOIN WebLIMSStations ON WebLIMSResults.SamplingPoint = WebLIMSStations.StationID"
      site_names <- dbGetQuery(con, query)
      dbDisconnect(con)
      
      return(site_names)
    }
    return(NULL)
  })
  
  # Step 3: Populate dropdown with site options
  observe({
    site_names <- get_sites_from_db()
    updateSelectizeInput(session, "site", choices = site_names$SamplingPoint, server = TRUE)
    updateSelectizeInput(session, "description", choices = site_names$Description, server = TRUE)
  })
  
  # Observe a description selection, update the site input:
  observeEvent(input$description, {
    req(input$description)
    site_id <- get_sites_from_db() %>% 
      filter(Description == input$description) %>% 
      pull(SamplingPoint)
    
    if (length(site_id) > 0) {
      updateSelectizeInput(session, "site", selected = site_id)
    }
  })
  
  # Fix the observeEvent for site selection to correctly update description
  observeEvent(input$site, {
    req(input$site)
    
    description <- get_sites_from_db() %>%
      filter(SamplingPoint == input$site) %>%
      pull(Description)
    
    if (length(description) > 0) {
      updateSelectizeInput(session, "description", selected = description)
    }
  })
  
  # Step 4: Dynamically update parameter options based on site selection
  observeEvent(input$site, {
    req(input$site)  # Ensure the site is selected
    
    # Fetch parameters for the selected site
    database_file <- fetch_and_unzip_data()
    con <- NULL
    
    if (endsWith(database_file, ".mdb")) {
      # Connect to Access database
      con <- dbConnect(odbc::odbc(), .connection_string = paste(
        "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
        "Dbq=", database_file, ";",
        "Uid=Admin;Pwd=;", sep = ""
      ))
    } else {
      # Connect to SQLite database (fallback)
      con <- dbConnect(duckdb(), database_file)
    }
    
    if (!is.null(con)) {
      query <- paste("SELECT DISTINCT WebParameter FROM WebLIMSResults WHERE SamplingPoint = '", input$site, "'", sep = "")
      parameters <- dbGetQuery(con, query)
      dbDisconnect(con)
      
      updateSelectizeInput(session, "parameter", choices = parameters$WebParameter, server = TRUE)
    }
  })
  
  # Step 5: Query data for the selected site and parameter
  get_data_for_plot_and_table <- reactive({
    req(input$site, input$parameter)  # Ensure both site and parameter are selected
    
    # Use the fetch_and_unzip_data function to get the file path
    database_file <- fetch_and_unzip_data()
    clean_data <- NULL
    con <- NULL
    
    if (endsWith(database_file, ".mdb")) {
      # Connect to Access database
      con <- dbConnect(odbc::odbc(), .connection_string = paste(
        "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
        "Dbq=", database_file, ";",
        "Uid=Admin;Pwd=;", sep = ""
      ))
    } else {
      # Connect to SQLite database (fallback)
      con <- dbConnect(duckdb(), database_file)
    }
    
    if (!is.null(con)) {
      # query <- paste("SELECT SamplingPoint, DateSampled, FinalResult, Qualifiers, RelativeDepthComments, WebParameter
      #             FROM WebLIMSResults
      #             WHERE SamplingPoint = '", input$site, "' AND WebParameter = '", input$parameter, "'", sep = "")
      query <- paste0(
       "SELECT SamplingPoint, DateSampled, FinalResult, Qualifiers, ",
       "RelativeDepthComments, WebParameter, WebLIMSStations.Description ",
       "FROM WebLIMSResults ",
       "LEFT JOIN WebLIMSStations ON WebLIMSResults.SamplingPoint = WebLIMSStations.StationID ",
       "WHERE SamplingPoint = '", input$site, "' ",
       "AND WebParameter = '", input$parameter, "'"
       )
      
      raw_data <- dbGetQuery(con, query)
      
      # Data cleaning here:
      clean_data <- raw_data %>%
        mutate(DateSampled = as.Date(DateSampled)) %>%
        mutate(
          FinalResult = as.character(FinalResult),
          # Ensure it's character before manipulation
          DL = case_when(
            grepl("^>", FinalResult) ~ ">DL",
            grepl("^<", FinalResult) ~ "<DL",
            TRUE ~ "Measured value"
          ),
          FinalResult = case_when(
            grepl("^>", FinalResult) ~ suppressWarnings(as.numeric(sub(">", "", FinalResult))),
            grepl("^<", FinalResult) ~ suppressWarnings(as.numeric(sub("<", "", FinalResult))) / 2,
            TRUE ~ suppressWarnings(as.numeric(trimws(FinalResult)))
          ),
          Qualifiers = case_when(Qualifiers == "" ~ "None",
                                 TRUE ~ Qualifiers),
          RelativeDepthComments = trimws(RelativeDepthComments),  # Remove leading & trailing spaces
          RelativeDepthComments = toupper(RelativeDepthComments),
          RelativeDepthComments = na_if(RelativeDepthComments, "")
        ) %>%
        mutate(across(c(Qualifiers, RelativeDepthComments, DL), as.factor))
      
      dbDisconnect(con)
    }
    
    return(clean_data)
  })
  
  # Step 6: Use the reactive data for both plot and table
  
  # Render plot using the reactive data
  output$plot <- renderGirafe({
    clean_data <- get_data_for_plot_and_table()  # Get cleaned data
  
    # Check if RelativeDepthComments has any non-NA values
    use_size <- any(!is.na(clean_data$RelativeDepthComments))
    
    # Base plot (no size or Relative Depth):
    p <- ggplot(clean_data, aes(x = DateSampled, y = FinalResult, 
                                tooltip = paste("Date:", format(DateSampled, "%m-%d-%Y"), 
                                                "<br>Result:", FinalResult,
                                                #"<br>Value:", DL, # weird display issue 
                                                "<br>Qualifiers:", Qualifiers,
                                                "<br>Relative Depth:", RelativeDepthComments))) +
      geom_point_interactive(aes(color = Qualifiers, shape = DL),
                             alpha = 0.7,
                             size = 2.5) +
      scale_shape_manual(values = c("Measured value" = 16, "<DL" = 17, ">DL" = 17)) + # 16 = circle, 17 = triangle
      theme_classic(base_size = 14) +
      ggtitle(paste(input$site, "-", input$parameter)) +
      labs(
        x = "Date",
        y = "Result",
        color = "Qualifiers",
        shape = "Values"
      ) +
      theme( axis.text.x = element_text(angle = 45, hjust =1))
    
    # Add size mapping only if RelativeDepthComments is not all NA
    if (use_size) {
      p <- p + 
        geom_point_interactive(aes(size = RelativeDepthComments, color = Qualifiers, shape = DL), alpha = 0.7) +
        # scale_shape_manual(values = c("Measured value" = 16, "<DL" = 17, ">DL" = 17)) +
        scale_size_manual(values = c("EPILIMNION" = 2.5, "HYPOLIMNION" = 5.5,
                                     "THERMOCLINE" = 4, "MID-DEPTH" = 4),
                          name = "Relative Depth",
                          drop = TRUE)
    }
    
    girafe(ggobj = p)
  })
  
  # Render the data table using the same reactive data
  output$data_table <- DT::renderDT({
    clean_data <- get_data_for_plot_and_table()  # Get cleaned data
    
    DT::datatable(clean_data[, c("SamplingPoint", "WebParameter","DateSampled", "FinalResult", "DL", "Qualifiers", "RelativeDepthComments")],
                  colnames = c("Site", "Parameter", "Date", "Result", "Detection Limit", "Qualifiers", "Relative Depth"),
                  extensions = 'Buttons',  # Enable buttons extension
                  options = list(
                    pageLength = 15,
                    autoWidth = TRUE,
                    dom = "Bfrtip",  # This controls the placement of buttons
                    buttons = list(list(extend = "csv", text = "Download CSV")),
                    searching = FALSE,  # Disable the search function
                    columnDefs = list(
                      list(
                        targets = 2,  # The "Date" column is at index 2 (third column)
                        width = '260px'  # Adjust the width as needed (in pixels or percentages)
                      )
                    )
                  ),
                  rownames = FALSE)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)