library(shiny)
library(RSQLite)
library(zip)
library(DBI)
library(dplyr)
library(ggiraph)
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(DT)

# This app will work locally on a windows machine, or hosted on a microsoft server
# with windows. 

# This is a test version to pull from the SQL Server instead of the website access database!!

# Define constants
fallback_data_path <- "Data/WebLIMS_sql2.sqlite" # Update this with better database
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
    textOutput("db_message"),
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
      style = "width: 85%; margin-top: 20px; max-width: 1000px",
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
    
    is_linux <- Sys.info()[["sysname"]] == "Linux"

      tryCatch({
        server_con <- dbConnect(
          odbc::odbc(),
          Driver   = "SQL Server",
          Server   = "SQLDB",
          Database = "WQAR_and_WebLIMS",
          Trusted_Connection = "Yes")
        
        WebLIMSResults <- dbReadTable(server_con, "WebLIMSResults")
        WebLIMSStations  <- dbReadTable(server_con, "WebLIMSStations")
        server_grab <- TRUE
        
        # Close the SQL connection
        dbDisconnect(server_con)
       
        # Save tables to temp sqlite database
        runjs('document.getElementById("loading-message").textContent = "Initializing database...";')
   
      }, error = function(e) {
        message("Error fetching or initializing data.:", e$message)
      })
    
    
    if (is_linux || !server_grab) {
      runjs('document.getElementById("loading-message").textContent = "Using backup database...";')
      # Use SQLite if no Access database is available
      database_file <- fallback_data_path
      
      conn_sqlite <- dbConnect(RSQLite::SQLite(), database_file)
      
      # Fetch the tables:
      WebLIMSResults <- dbReadTable(conn_sqlite, "WebLIMSResults")
      WebLIMSStations  <- dbReadTable(conn_sqlite, "WebLIMSStations")
      
      dbDisconnect(conn_sqlite)
      
      # Get date range:
      date_min <- min(WebLIMSResults$DateSampled)
      date_max <- max(WebLIMSResults$DateSampled)
      
      db_message <- paste("Using backup database. Data available between ", date_min, " and ", date_max)
    
      } else {
      # Fetch the dates for full database:
      date_min <- min(WebLIMSResults$DateSampled)
      date_max <- max(WebLIMSResults$DateSampled)
      
      db_message <- paste0("Using most recent version of the database. Data available between ", 
                          format(date_min, "%m-%d-%Y"), " and ", format(date_max, "%m-%d-%Y"))
    }
    
    runjs('document.getElementById("loading-overlay").style.display = "none";')  # Hide overlay after success
    output$db_message <- renderText({ db_message })
    
    # # Remove whitespace then leftoin description to sites, creating ONE object
    WebLIMSStations <- WebLIMSStations %>% 
      mutate(StationID = str_trim(StationID)) %>% 
      distinct(StationID, .keep_all = TRUE)
    WebLIMSall <- WebLIMSResults %>% 
      mutate(StationID = str_trim(StationID)) %>% 
      left_join(WebLIMSStations, by = "StationID")
    
    return(WebLIMSall)
  })
  
  # Step 2: Call the fetch data function and store it:
  fetched_data <- reactiveVal(NULL)  # Store data once
  
  observeEvent(NULL, {
    fetched_data(fetch_data())  # Fetches data once and stores it
  }, once = TRUE)  # Ensures it only runs initially
  
  # Access data elsewhere with `fetched_data()`
  
  # TESTING:

  observe({
    data <- fetch_data()
    if (!is.null(data)) {
      fetched_data(data)  # Store the fetched data
    }
  })

  
  
  # Step 3: Pull a list of sites from SamplingPoint
  get_sites_from_data <- reactive({
    data <- fetched_data()  # Retrieve the stored data
    if (is.null(data)) return(NULL)  # Prevent errors if data isn't ready
 
    # Grab just site info
    site_info <- data %>% 
      select("StationID", "Description")
    
    # Ensure distinct site names with descriptions
    site_info <- unique(site_info[, c("StationID", "Description")])  
    
    return(site_info)
    })
  
  # Step 4: Populate dropdown with site options
  observe({
    site_info <- get_sites_from_data()
    if (is.null(site_info)) return()
    updateSelectizeInput(session, "site", choices = site_info$StationID, server = TRUE)
    updateSelectizeInput(session, "description", choices = unique(site_info$Description), server = TRUE)
  })
  
  # Step 5: Observe a description selection, update the site input:
  observeEvent(input$description, {
    req(input$description)
    site_info <- get_sites_from_data()
    if (!is.null(site_info) && input$description %in% site_info$Description) {
      site_id <- site_info %>% filter(Description == input$description) %>% pull(StationID)
      updateSelectizeInput(session, "site", selected = site_id[1])
    }
  })
  
  # Step 6: Fix the observeEvent for site selection to correctly update description
  observeEvent(input$site, {
    req(input$site)
    
    description <- get_sites_from_data() %>%
      filter(StationID == input$site) %>%
      pull(Description)
    
    if (length(description) > 0) {
      updateSelectizeInput(session, "description", selected = description[1])
    }
  })
  
  # Step 7: Dynamically update parameter options based on site selection
  observeEvent(input$site, {
    req(input$site)  # Ensure the site is selected
    
    data <- fetched_data()
    
    if (!is.null(data) && "WebParameter" %in% colnames(data)) {
      parameters <- data %>%
        filter(StationID == input$site) %>%
        distinct(WebParameter) %>%
        pull(WebParameter)
      
      updateSelectizeInput(session, "parameter", choices = parameters, server = TRUE)
    }
  })
  
  # Step 8: Query data for the selected site and parameter
  get_data_for_plot_and_table <- reactive({
    req(input$site, input$parameter)  # Ensure both site and parameter are selected
    
    # Use the stored data (WebLIMSResults)
    data <- fetched_data() # Fetch stored data
    
    # Filter data for the selected site and parameter
    clean_data <- data %>%
      filter(StationID == input$site, WebParameter == input$parameter)
   
    
     print("cleaned DATA BEFORE:")
    print(head(clean_data))
    
    # Renaming data
    clean_data <- clean_data %>% 
      rename(FinalResult = WebResult,
             Qualifiers = Qualifier,
             RelativeDepthComments = RelativeDepthSample) %>% 
      select(StationID, RelativeDepthComments, DateSampled, WebParameter, FinalResult, Qualifiers, Description)
    
    # Data cleaning here:
    clean_data <- clean_data %>%
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
    
    print("CLEANED FINAL")
    print(head(clean_data))
    
    return(clean_data)
  })
  
  
  # Step 9: Use the reactive data for both plot and table
  # Render plot using the reactive data
  output$plot <- renderGirafe({
    clean_data <- get_data_for_plot_and_table()  # Get cleaned data
    
    # Debugging: check clean_data structure
    print(head(clean_data))
    
    if (nrow(clean_data) == 0) {
      return(NULL)  # Return nothing if data is empty
    }
  
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
    
    DT::datatable(clean_data[, c("StationID", "WebParameter","DateSampled", "FinalResult", "DL", "Qualifiers", "RelativeDepthComments")],
                  colnames = c("Site", "Parameter", "Date", "Result", "Detection Limit", "Qualifiers", "Relative Depth"),
                  options = list(
                    pageLength = 20,
                    autoWidth = TRUE,
                    dom = "Bfrtip",  # This controls the placement of buttons like 'copy', 'csv', etc
                    buttons = c("copy", "csv", "excel"),
                    searching = FALSE,  # Disable the search function
                    columnDefs = list(
                      list(
                        targets = 2,  # The "Date" column is at index 2 (third column)
                        width = '250px'  # Adjust the width as needed (in pixels or percentages)
                      )
                    )
                  ),
                  rownames = FALSE)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)