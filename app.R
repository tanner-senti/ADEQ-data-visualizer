library(zip)
library(DBI)
library(dplyr)
library(ggiraph)
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(DT)
library(stringr)
library(duckdb)

# Load the server information:
readRenviron("~/.Renviron")


# This app will work locally on a windows machine connected to the ADEQ network, 
# or hosted on a server by ADEQ IT.

# This version of the app will connect directly to the SQL Server database and
# pull the data. IF this connection fails, will default to smaller backup database.

# Define constants
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
    style = "display: flex; flex-direction: column; align-items: center; justify-content: center;",
    
    titlePanel("Water Quality Data Viewer"),
    
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
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Step 1: Function to Fetch data from server and save tables to local database
  # Function only runs once initially, when Observe() calls it below
  fetch_data <- reactive({
    runjs('document.getElementById("loading-overlay").style.display = "flex";')
    runjs('document.getElementById("loading-message").textContent = "Fetching data from server, please wait...";')
    
    server_grab <- FALSE
    database_file <- NULL
    db_message <- NULL
    
    tryCatch({
      server_con <- dbConnect(
        odbc::odbc(),
        Driver   = "SQL Server",
        Server = Sys.getenv("SQL_SERVER"),
        Database = Sys.getenv("SQL_DATABASE"),
        Trusted_Connection = "Yes")
      
      WebLIMSResults <- dbReadTable(server_con, "WebLIMSResults")
      WebLIMSStations  <- dbReadTable(server_con, "WebLIMSStations")
      server_grab <- TRUE
      
      # Close the SQL connection
      dbDisconnect(server_con)
      
      # Save tables to temp_dir sqlite database - use this connection for rest of app!
      runjs('document.getElementById("loading-message").textContent = "Initializing database...";')
      
      # Create sqlite database for data
      full_con <- dbConnect(duckdb::duckdb(), paste0(temp_dir, "/weblims_full.duckdb"))
      
      # Convert Date to character for SQLite compatibility (must convert back to date whenever read in):
      WebLIMSResults$DateSampled <- as.character(WebLIMSResults$DateSampled)
      
      dbWriteTable(full_con, "WebLIMSResults", WebLIMSResults, overwrite = TRUE)
      dbWriteTable(full_con, "WebLIMSStations", WebLIMSStations, overwrite = TRUE)
      
      dbDisconnect(full_con)
      
      database_file <- paste0(temp_dir, "/weblims_full.duckdb")
      
    }, error = function(e) {
      message("Error fetching or initializing data.:", e$message)
    })
    
    
    if (!server_grab) {
      runjs('document.getElementById("loading-message").textContent = "Using backup database...";')
      # Use SQLite if no Access database is available
      # ENSURE backup database column names and types are identical to SQL Server!!!
      database_file <- fallback_data_path
      
      conn_sqlite <- dbConnect(duckdb::duckdb(), database_file)
      
      # Get the data range for backup data:
      date_range <- dbGetQuery(conn_sqlite, "SELECT MIN(DateSampled) AS min_date, MAX(DateSampled) AS max_date FROM WebLIMSResults")
      
      # FIX the leading/trailing spaces for SQLITE here:
      # Run update queries to trim spaces
      dbExecute(conn_sqlite, "UPDATE WebLIMSResults SET SamplingPoint = TRIM(SamplingPoint);")
      dbExecute(conn_sqlite, "UPDATE WebLIMSStations SET StationID = TRIM(StationID);")
      dbDisconnect(conn_sqlite)
      
      db_message <- paste("Using backup database - data available between ", date_range$min_date, " and ", date_range$max_date)
      
    } else {
      # Fetch the date range from SQLite
      full_con <- dbConnect(duckdb::duckdb(), database_file)
      
      # Rename StationID to SamplingPoint to reduce errors:
      dbExecute(full_con, "ALTER TABLE WebLIMSResults RENAME COLUMN StationID to SamplingPoint")
      
      date_range <- dbGetQuery(full_con, "SELECT MIN(DateSampled) AS min_date, MAX(DateSampled) AS max_date FROM WebLIMSResults")
      # FIX the leading/trailing spaces for SQLITE here:
      # Run update queries to trim spaces
      dbExecute(full_con, "UPDATE WebLIMSResults SET SamplingPoint = TRIM(SamplingPoint);")
      dbExecute(full_con, "UPDATE WebLIMSStations SET StationID = TRIM(StationID);")
      dbDisconnect(full_con)
      
      db_message <- paste("Using most recent version of the database:",
                          "<br>", "Data available between ", format(as.Date(date_range$min_date), "%m-%d-%Y"), " and ", format(as.Date(date_range$max_date), "%m-%d-%Y"))
    }
    
    runjs('document.getElementById("loading-overlay").style.display = "none";')  # Hide overlay after success
    
    output$db_message <- renderUI({
      HTML(db_message)
    })
    # Output is path to database file (either full SQLite or backup SQLite)
    database_file
  })
  
  # Step 2: Pull a list of sites from StationID
  get_sites_from_data <- reactive({
    database_file <- fetch_data()
    con <- NULL
    
    # Connect to SQLite database
    con <- dbConnect(duckdb::duckdb(), database_file)
    
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
    site_info <- get_sites_from_data()
    req(site_info)  # Ensure data exists before updating UI
    suppressWarnings(updateSelectizeInput(session, "site", choices = site_info$SamplingPoint, server = FALSE)) # Set to FALSE during debugging
    suppressWarnings(updateSelectizeInput(session, "description", choices = site_info$Description, server = FALSE))
  })
  
  # Step 4: Observer a description seletion, update the site input:
  observeEvent(input$description, {
    req(input$description)
    site_id <- get_sites_from_data() %>% 
      filter(Description == input$description) %>% 
      pull(SamplingPoint)
    
    if (length(site_id) > 0) {
      updateSelectizeInput(session, "site", selected = site_id)
    }
  })
  
  # Step 5: Fix the observeEvent for site selection to correctly update description
  observeEvent(input$site, {
    req(input$site)
    
    description <- get_sites_from_data() %>%
      filter(SamplingPoint == input$site) %>%
      pull(Description)
    
    if (length(description) > 0) {
      updateSelectizeInput(session, "description", selected = description)
    }
  })
  
  # Step 6: Dynamically update parameter options based on site selection
  observeEvent(input$site, {
    req(input$site)  # Ensure the site is selected
    
    # Fetch parameters for the selected site
    database_file <- fetch_data()
    con <- NULL
    
    # Connect to SQLite database (fallback)
    con <- dbConnect(duckdb::duckdb(), database_file)
    
    if (!is.null(con)) {
      query <- paste("SELECT DISTINCT WebParameter FROM WebLIMSResults WHERE SamplingPoint = '", input$site, "'", sep = "")
      parameters <- dbGetQuery(con, query)
      dbDisconnect(con)
      
      updateSelectizeInput(session, "parameter", choices = parameters$WebParameter, server = TRUE)
    }
  })
  
  # Step 7: Query data for the selected site and parameter
  get_data_for_plot_and_table <- reactive({
    req(input$site, input$parameter)  # Ensure both site and parameter are selected
    
    # Use the fetch_and_unzip_data function to get the file path
    database_file <- fetch_data()
    clean_data <- NULL
    con <- NULL
    
    # Connect to SQLite database (fallback)
    con <- dbConnect(duckdb::duckdb(), database_file)
    
    if (!is.null(con)) {
      # query <- paste("SELECT SamplingPoint, DateSampled, FinalResult, Qualifiers, RelativeDepthComments, WebParameter
      #             FROM WebLIMSResults
      #             WHERE SamplingPoint = '", input$site, "' AND WebParameter = '", input$parameter, "'", sep = "")
      query <- paste0(
        "SELECT SamplingPoint, DateSampled, WebResult, Qualifier, ",
        "RelativeDepthSample, WebParameter, WebLIMSStations.Description ",
        "FROM WebLIMSResults ",
        "LEFT JOIN WebLIMSStations ON WebLIMSResults.SamplingPoint = WebLIMSStations.StationID ",
        "WHERE SamplingPoint = '", input$site, "' ",
        "AND WebParameter = '", input$parameter, "'"
      )
      
      raw_data <- dbGetQuery(con, query)
      dbDisconnect(con)
      
      # Data cleaning here:
      clean_data <- raw_data %>%
        rename(FinalResult = WebResult,
               Qualifiers = Qualifier,
               RelativeDepthComments = RelativeDepthSample) %>%
        select(SamplingPoint, RelativeDepthComments, DateSampled, WebParameter, FinalResult, Qualifiers, Description) %>%
        mutate(
          DateSampled = as.Date(DateSampled),
          FinalResult = as.character(FinalResult),  # Ensure character before manipulation
          FinalResult = gsub("[^0-9.<>=]", "", FinalResult),  # Remove any non-numeric characters except <, >, .
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
          Qualifiers = if_else(is.na(Qualifiers) | Qualifiers == "", "None", Qualifiers),  # Handle empty and NA values
          RelativeDepthComments = if_else(is.na(trimws(RelativeDepthComments)) | trimws(RelativeDepthComments) == "", 
                                          "N/A", 
                                          toupper(trimws(RelativeDepthComments))) 
        ) %>%
        filter(!is.na(FinalResult)) %>%  # Remove rows where FinalResult is NA
        mutate(across(c(Qualifiers, RelativeDepthComments, DL), as.factor))
    }
    
    return(clean_data)
  })
  
  
  # Step 8: Use the reactive data for both plot and table
  # Render plot using the reactive data
  output$plot <- renderGirafe({
    clean_data <- get_data_for_plot_and_table()  # Get cleaned data
    
    if (nrow(clean_data) == 0) {
      return(NULL)  # Return nothing if data is empty
    }
    
    # Check if RelativeDepthComments has any non-NA values
    use_size <- any(clean_data$RelativeDepthComments != "N/A")
    
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
                                     "THERMOCLINE" = 4, "MID-DEPTH" = 4, "N/A" = 7),
                          name = "Relative Depth",
                          drop = TRUE)
    }
    
    girafe(ggobj = p)
  })
  
  # Render the data table using the same reactive data
  output$data_table <- DT::renderDT({
    clean_data <- get_data_for_plot_and_table()  # Get cleaned data
    
    clean_data$DateSampled <- format(as.Date(clean_data$DateSampled), "%m-%d-%Y")
    
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