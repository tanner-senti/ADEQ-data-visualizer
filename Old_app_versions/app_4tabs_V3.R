# Testing quick output
library(shiny)
library(dplyr)
library(DBI)
library(duckdb)
library(DT)
library(ggplot2)
library(ggiraph)
library(shinycssloaders)
library(shinyjs)
library(bslib)
library(stringr)
library(later)

### INFO ####
# This is a working version of the app that:
# Has 4 separate tabs with similar functions, adding capability with each tab.
# This was used to develope the multi-site, multi-param selection

# This version of the app was retired on 2025-10-20 and superseded by a version
# with caching and multi-site, multi-parameter selection.
################################################################################

#---------------------------------------------------------
# Load connection info and constants
#---------------------------------------------------------
readRenviron(".Renviron")
cache_data_path <- "WebLIMS_backup.duckdb"

#---------------------------------------------------------
# UI
#---------------------------------------------------------
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  useShinyjs(),

  # Loading messages:
  div(
    id = "loading-overlay",
    style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; 
           background-color: rgba(255, 255, 255, 0.8); z-index: 1000; 
           display: none; align-items: center; justify-content: center;",
    h3(
      id = "loading-message",
      "Loading data, please wait...",
      style = "color: #555;"
    )
  ),

  div(
    style = "display: flex; flex-direction: column; align-items: center; width: 100%;",
    br(),
    titlePanel("Water Quality Data Explorer"),
    htmlOutput("db_message"),
    br(),

    # Making input labels bold:
    tags$head(
      tags$style(HTML(
        "
.control-label {
  font-weight: bold;
}

#date_start, #date_end, #date_start2, #date_end2, #date_start3, #date_end3, #date_start4, #date_end4 {
  display: flex;
  align-items: center;
  margin-bottom: 10px;
}

#date_start label, #date_end label, #date_start2 label, #date_end2 label, #date_start3 label, #date_end3 label, #date_start4 label, #date_end4 label {
  display: inline-block;
  width: 50px;
  margin-right: 10px;
  margin-bottom: 0;
  font-weight: bold;
}

#date_start input, #date_end input, #date_start2 input, #date_end2 input, #date_start3 input, #date_end3 input, #date_start4 input, #date_end4 input {
  width: 120px !important;
  margin-left: 0px;
}

#date_end, #date_end2, #date_end3, #date_end4 {
  margin-left: 8px;
}
  "
      ))
    ),

    # Tab panels
    tabsetPanel(
      id = "main_tabs",

      # TAB 1: Multiple Sites, One Parameter
      tabPanel(
        "Multiple Sites",
        br(),
        wellPanel(
          style = "max-width: 800px;",
          fluidRow(
            column(
              6,
              selectizeInput(
                "sites",
                "Select Sites:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Search by SiteID...",
                  maxOptions = 10000
                )
              ),
              selectizeInput(
                "parameter",
                "Choose a parameter:",
                choices = NULL,
                multiple = FALSE,
                options = list(placeholder = "Search...", maxOptions = 1000)
              )
            ),
            column(
              6,
              selectizeInput(
                "descriptions",
                "Search by Description:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Search by Description...",
                  maxOptions = 10000
                )
              )
            )
          ),

          fluidRow(
            column(
              12,
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton(
                  "update_data",
                  "Display Data",
                  style = "font-size:16px; padding:10px 30px;
                                background-color:#0080b7; color:white;
                                border:none; border-radius:8px;",
                  icon = icon("sync")
                )
              )
            )
          )
        ),

        br(),
        div(
          style = "text-align: center; width:80%;",
          uiOutput("missing_sites_message")
        ),
        br(),

        # --- Plot and Plot Options side-by-side ---
        div(
          style = "display:flex; justify-content:center; align-items:flex-start; 
          gap:20px; width:auto; margin: auto; flex-wrap:wrap;",

          # Left: Plot
          div(
            style = "flex-grow:2; min-width: 550px; width: auto;",
            withSpinner(girafeOutput("plot", width = "auto", height = "auto"))
          ),

          # Right: Plot Options box
          conditionalPanel(
            condition = "output.show_plot",
            wellPanel(
              style = "flex:1; min-width:220px; max-width:220px; background-color:#f8f9fa; border-radius:4px;",
              h4("Plot Options"),
              hr(),
              radioButtons(
                "trend_type",
                "Add trend line:",
                choices = c("None", "Linear", "LOESS"),
                selected = "None",
                inline = FALSE
              ),
              br(),
              h5("Filter by date:"),
              dateInput(
                "date_start",
                "Start:",
                value = NULL,
                format = "yyyy-mm-dd",
                width = "145px"
              ),
              dateInput(
                "date_end",
                "End:",
                value = NULL,
                format = "yyyy-mm-dd",
                width = "145px"
              ),
              br(),
              downloadButton(
                "download_plot",
                "Download Plot",
                style = "font-size:16px; padding:10px; background-color:#0080b7;
                              color:white; border:none; border-radius:8px;"
              )
            )
          )
        ),

        div(
          style = "width:85%; margin-top:20px; max-width:700px;",
          DTOutput("data_table")
        )
      ),

      # TAB 2: One Site, Multiple Parameters
      tabPanel(
        "Multiple Parameters",
        br(),
        wellPanel(
          style = "max-width: 800px;",
          fluidRow(
            column(
              6,
              selectizeInput(
                "site_single",
                "Select Site:",
                choices = NULL,
                multiple = FALSE,
                options = list(
                  placeholder = "Search by SiteID...",
                  maxOptions = 10000
                )
              ),
              selectizeInput(
                "parameters_multi",
                "Choose parameters:",
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Search...", maxOptions = 1000)
              )
            ),
            column(
              6,
              selectizeInput(
                "description_single",
                "Search by Description:",
                choices = NULL,
                multiple = FALSE,
                options = list(
                  placeholder = "Search by Description...",
                  maxOptions = 10000
                )
              )
            )
          ),

          fluidRow(
            column(
              12,
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton(
                  "update_data2",
                  "Display Data",
                  style = "font-size:16px; padding:10px 30px;
                                background-color:#0080b7; color:white;
                                border:none; border-radius:8px;",
                  icon = icon("sync")
                )
              )
            )
          )
        ),

        br(),
        div(
          style = "text-align: center; width:80%;",
          uiOutput("missing_params_message")
        ),
        br(),

        # --- Plot and Plot Options side-by-side ---
        div(
          style = "display:flex; justify-content:center; align-items:flex-start; 
          gap:20px; width:auto; margin: auto; flex-wrap:wrap;",

          # Left: Plot
          div(
            style = "flex-grow:2; min-width: 550px; width: auto;",
            withSpinner(girafeOutput("plot2", width = "auto", height = "auto"))
          ),

          # Right: Plot Options box
          conditionalPanel(
            condition = "output.show_plot2",
            wellPanel(
              style = "flex:1; min-width:220px; max-width:220px; background-color:#f8f9fa; border-radius:4px;",
              h4("Plot Options"),
              hr(),
              radioButtons(
                "trend_type2",
                "Add trend line:",
                choices = c("None", "Linear", "LOESS"),
                selected = "None",
                inline = FALSE
              ),
              br(),
              h5("Filter by date:"),
              dateInput(
                "date_start2",
                "Start:",
                value = NULL,
                format = "yyyy-mm-dd",
                width = "145px"
              ),
              dateInput(
                "date_end2",
                "End:",
                value = NULL,
                format = "yyyy-mm-dd",
                width = "145px"
              ),
              br(),
              downloadButton(
                "download_plot2",
                "Download Plot",
                style = "font-size:16px; padding:10px; background-color:#0080b7;
                              color:white; border:none; border-radius:8px;"
              )
            )
          )
        ),

        div(
          style = "width:85%; margin-top:20px; max-width:700px;",
          DTOutput("data_table2")
        )
      ),

      # TAB 3: Multiple Sites, Multiple Parameters
      tabPanel(
        "Multiple Sites & Parameters",
        br(),
        wellPanel(
          style = "max-width: 800px;",
          fluidRow(
            column(
              6,
              selectizeInput(
                "sites_multi3",
                "Select Sites:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Search by SiteID...",
                  maxOptions = 10000
                )
              ),
              selectizeInput(
                "parameters_multi3",
                "Choose parameters:",
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Search...", maxOptions = 1000)
              )
            ),
            column(
              6,
              selectizeInput(
                "descriptions_multi3",
                "Search by Description:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Search by Description...",
                  maxOptions = 10000
                )
              )
            )
          ),

          fluidRow(
            column(
              12,
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton(
                  "update_data3",
                  "Display Data",
                  style = "font-size:16px; padding:10px 30px;
                                background-color:#0080b7; color:white;
                                border:none; border-radius:8px;",
                  icon = icon("sync")
                )
              )
            )
          )
        ),

        br(),
        div(
          style = "text-align: center; width:80%;",
          uiOutput("missing_data_message3")
        ),
        br(),

        # --- Plot and Plot Options side-by-side ---
        div(
          style = "display:flex; justify-content:center; align-items:flex-start; 
          gap:20px; width:auto; margin: auto; flex-wrap:wrap;",

          # Left: Plot
          div(
            style = "flex-grow:2; min-width: 550px; width: auto; max-width: 700px;",
            withSpinner(girafeOutput("plot3", width = "auto", height = "auto"))
          ),

          # Right: Plot Options box
          conditionalPanel(
            condition = "output.show_plot3",
            wellPanel(
              style = "flex:1; min-width:220px; max-width:220px; background-color:#f8f9fa; border-radius:4px;",
              h4("Plot Options"),
              hr(),
              radioButtons(
                "trend_type3",
                "Add trend line:",
                choices = c("None", "Linear", "LOESS"),
                selected = "None",
                inline = FALSE
              ),
              br(),
              h5("Filter by date:"),
              dateInput(
                "date_start3",
                "Start:",
                value = NULL,
                format = "yyyy-mm-dd",
                width = "145px"
              ),
              dateInput(
                "date_end3",
                "End:",
                value = NULL,
                format = "yyyy-mm-dd",
                width = "145px"
              ),
              br(),
              downloadButton(
                "download_plot3",
                "Download Plot",
                style = "font-size:16px; padding:10px; background-color:#0080b7;
                              color:white; border:none; border-radius:8px;"
              )
            )
          )
        ),

        div(
          style = "width:85%; margin-top:20px; max-width:700px;",
          DTOutput("data_table3")
        )
      ),
      #### TAB 4: Faceted Multi-Site / Multi-Parameter ----------
      tabPanel(
        "Faceted Multi",
        br(),
        wellPanel(
          style = "max-width: 800px;",
          fluidRow(
            column(
              6,
              selectizeInput(
                "sites_multi4",
                "Select Sites:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Search by SiteID...",
                  maxOptions = 10000
                )
              ),
              selectizeInput(
                "parameters_multi4",
                "Choose parameters:",
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Search...", maxOptions = 1000)
              )
            ),
            column(
              6,
              selectizeInput(
                "descriptions_multi4",
                "Search by Description:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Search by Description...",
                  maxOptions = 10000
                )
              )
            )
          ),
          fluidRow(
            column(
              12,
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton(
                  "update_data4",
                  "Display Data",
                  style = "font-size:16px; padding:10px 30px; background-color:#0080b7; color:white; border:none; border-radius:8px;",
                  icon = icon("sync")
                )
              )
            )
          )
        ),
        br(),
        # PLOT AND OPTIONS BOX DIV:
        div(
          style = "display:flex; justify-content:center; align-items:flex-start; 
  gap:10px; width:auto; margin: auto; flex-wrap:wrap;",

          # -Left: PLOT
          div(
            style = "flex-grow:4; min-width: 550px; width: auto;",
            withSpinner(girafeOutput("plot4", width = "auto", height = "auto"))
          ),

          # Right: Plot Options box
          conditionalPanel(
            condition = "output.show_plot4",
            wellPanel(
              style = "flex:1; min-width:220px; max-width:none; background-color:#f8f9fa; border-radius:4px;",
              h4("Plot Options"),
              hr(),
              h5("Filter by date:"),
              dateInput(
                "date_start4",
                "Start:",
                value = NULL,
                format = "yyyy-mm-dd",
                width = "145px"
              ),
              dateInput(
                "date_end4",
                "End:",
                value = NULL,
                format = "yyyy-mm-dd",
                width = "145px"
              ),
              br(),
              radioButtons(
                "facet_type",
                "Facet by:",
                choices = c("Site", "Parameter"),
                selected = "Site",
                inline = FALSE
              ),
              br(),
              radioButtons(
                "trend_type4",
                "Add trend line:",
                choices = c("None", "Linear", "LOESS"),
                selected = "None",
                inline = FALSE
              ),
              br(),
              downloadButton(
                "download_plot4",
                "Download Plot",
                style = "font-size:16px; padding:10px; background-color:#0080b7;
                            color:white; border:none; border-radius:8px;"
              )
            )
          )
        ),
        br(),
        div(
          style = "width:85%; margin-top:20px; max-width:700px;",
          DTOutput("data_table4")
        )
      )
    ),

    tags$footer(
      style = "width:100%; text-align:center; padding:10px; margin-top:40px;
             background-color:#f8f9fa; color:#6c757d; border-top:1px solid #dee2e6;",
      "Made by Tanner Senti for Arkansas Division of Environmental Quality"
    )
  )
)

#---------------------------------------------------------
# SERVER
#---------------------------------------------------------
server <- function(input, output, session) {
  # --- Step 1: Reactive fetch & caching ---
  fetch_data <- reactive({
    runjs('document.getElementById("loading-overlay").style.display = "flex";')

    server_grab <- FALSE
    database_file <- NULL
    db_message <- NULL

    use_cache <- FALSE
    cache_max_date <- NULL

    # Check if backup database exists and was updated recently
    if (file.exists(cache_data_path)) {
      tryCatch(
        {
          cache_con <- dbConnect(duckdb::duckdb(), cache_data_path)

          cache_max_date <- dbGetQuery(
            cache_con,
            "SELECT MAX(DateSampled) AS max_date FROM WebLIMSResults"
          )$max_date

          dbDisconnect(cache_con)

          # Calculate days since most recent data
          days_since_data <- as.numeric(difftime(
            Sys.time(),
            as.Date(cache_max_date),
            units = "days"
          ))

          use_cache <- days_since_data < 10
        },
        error = function(e) {
          message("Error checking cache: ", e$message)
        }
      )
    }

    if (use_cache) {
      message(
        ">>> USING CACHE - Most recent data is ",
        round(days_since_data, 1),
        " days old. Currently checks for new data every 10 days."
      )
      database_file <- cache_data_path
      cache_con <- dbConnect(duckdb::duckdb(), database_file)
      date_range <- dbGetQuery(
        cache_con,
        "SELECT MIN(DateSampled) AS min_date, MAX(DateSampled) AS max_date FROM WebLIMSResults"
      )
      dbExecute(
        cache_con,
        "UPDATE WebLIMSResults SET SamplingPoint = TRIM(SamplingPoint);"
      )
      dbExecute(
        cache_con,
        "UPDATE WebLIMSStations SET StationID = TRIM(StationID);"
      )
      dbDisconnect(cache_con)

      db_message <- paste(
        "Using cached database. Data available between",
        date_range$min_date,
        "and",
        date_range$max_date
      )
    } else {
      message(
        ">>> CACHE OUTDATED - Most recent data is ",
        round(days_since_data, 1),
        " days old. Attempting to fetch new data."
      )
      # Fetch from server (incremental if possible)
      runjs(
        'document.getElementById("loading-message").textContent = "Fetching latest database from server, please wait...";'
      )
      tryCatch(
        {
          server_con <- dbConnect(
            odbc::odbc(),
            Driver = "SQL Server",
            Server = Sys.getenv("SQL_SERVER"),
            Database = Sys.getenv("SQL_DATABASE"),
            Trusted_Connection = "Yes"
          )

          # Debugging testing
          dbListTables(server_con)

          # Get new data only (if cache exists)
          if (!is.null(cache_max_date)) {
            query <- paste0(
              "SELECT * FROM WebLIMSResults WHERE DateSampled > '",
              cache_max_date,
              "' ORDER BY StationID"
            )
            NewResults <- dbGetQuery(server_con, query)
            WebLIMSStations <- dbReadTable(server_con, "WebLIMSStations")

            # RENAME StationID to SamplingPoint to match backup database
            if ("StationID" %in% colnames(NewResults)) {
              NewResults <- NewResults %>% rename(SamplingPoint = StationID)
            }

            # Append to existing backup database
            if (nrow(NewResults) > 0) {
              message(">>> APPENDED ", nrow(NewResults), " new rows to cache")
              cache_con <- dbConnect(duckdb::duckdb(), cache_data_path)

              NewResults$DateSampled <- as.character(NewResults$DateSampled)
              dbWriteTable(
                cache_con,
                "WebLIMSResults",
                NewResults,
                append = TRUE
              )

              dbWriteTable(
                cache_con,
                "WebLIMSStations",
                WebLIMSStations,
                overwrite = TRUE
              )
              dbDisconnect(cache_con)
            } else {
              message(">>> NO NEW DATA FOUND WITHIN SERVER")
            }
          } else {
            # No cache exists, fetch all data
            message(">>> FETCHING ALL DATA - No cache found")
            WebLIMSResults <- dbGetQuery(
              server_con,
              "SELECT * FROM WebLIMSResults ORDER BY SamplingPoint"
            )
            WebLIMSStations <- dbReadTable(server_con, "WebLIMSStations")

            # RENAME StationID to SamplingPoint before writing
            if ("StationID" %in% colnames(WebLIMSResults)) {
              WebLIMSResults <- WebLIMSResults %>%
                rename(SamplingPoint = StationID)
            }

            cache_con <- dbConnect(duckdb::duckdb(), cache_data_path)
            WebLIMSResults$DateSampled <- as.character(
              WebLIMSResults$DateSampled
            )
            dbWriteTable(
              cache_con,
              "WebLIMSResults",
              WebLIMSResults,
              overwrite = TRUE
            )
            dbWriteTable(
              cache_con,
              "WebLIMSStations",
              WebLIMSStations,
              overwrite = TRUE
            )
            dbDisconnect(cache_con)
          }

          server_grab <- TRUE
          dbDisconnect(server_con)
          database_file <- cache_data_path
        },
        error = function(e) {
          message("Error fetching data: ", e$message)
        }
      )

      if (server_grab) {
        full_con <- dbConnect(duckdb::duckdb(), database_file)
        date_range <- dbGetQuery(
          full_con,
          "SELECT MIN(DateSampled) AS min_date, MAX(DateSampled) AS max_date FROM WebLIMSResults"
        )
        dbExecute(
          full_con,
          "UPDATE WebLIMSResults SET SamplingPoint = TRIM(SamplingPoint);"
        )
        dbExecute(
          full_con,
          "UPDATE WebLIMSStations SET StationID = TRIM(StationID);"
        )
        dbDisconnect(full_con)

        db_message <- paste(
          "Using most recent version of the database:",
          "<br>Data available between ",
          format(as.Date(date_range$min_date), "%m-%d-%Y"),
          " and ",
          format(as.Date(date_range$max_date), "%m-%d-%Y")
        )
      } else {
        # Server fetch failed, use backup if it exists

        # Input logic for a backup here. Not implemented because redundant with cached DB
        message(
          "Error with server and/or caching - attempting to use backup but none currently exist"
        )
      }
    }

    runjs('document.getElementById("loading-overlay").style.display = "none";')
    output$db_message <- renderUI({
      HTML(db_message)
    })
    database_file
  })

  database_path <- reactiveVal(NULL)

  observe({
    dbfile <- fetch_data()
    if (!is.null(dbfile)) database_path(dbfile)
  })

  # --- Step 2: Populate UI after fetch ---
  site_data <- reactiveVal(NULL)

  # Pull list of sites with data in WebLIMSResults and matching descriptions:
  observe({
    req(database_path())
    con <- dbConnect(duckdb::duckdb(), database_path())
    on.exit(dbDisconnect(con), add = TRUE)

    site_data(dbGetQuery(
      con,
      "SELECT DISTINCT SamplingPoint, WebLIMSStations.Description
                FROM WebLIMSResults
                LEFT JOIN WebLIMSStations ON WebLIMSResults.SamplingPoint = WebLIMSStations.StationID
                ORDER BY SamplingPoint"
    ))

    # Tab 1 inputs
    updateSelectizeInput(
      session,
      "sites",
      choices = site_data()$SamplingPoint,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "descriptions",
      choices = site_data()$Description,
      server = TRUE
    )

    # Tab 2 inputs
    updateSelectizeInput(
      session,
      "site_single",
      choices = site_data()$SamplingPoint,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "description_single",
      choices = site_data()$Description,
      server = TRUE
    )

    # Tab 3 inputs
    updateSelectizeInput(
      session,
      "sites_multi3",
      choices = site_data()$SamplingPoint,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "descriptions_multi3",
      choices = site_data()$Description,
      server = TRUE
    )

    # Tab 4 inputs
    updateSelectizeInput(
      session,
      "sites_multi4",
      choices = site_data()$SamplingPoint,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "descriptions_multi4",
      choices = site_data()$Description,
      server = TRUE
    )
  })

  # ============================================================
  # TAB 1: MULTIPLE SITES, ONE PARAMETER
  # ============================================================

  # --- Step 3: Sync sites <-> descriptions safely ---
  updating_selects <- reactiveVal(FALSE)

  observeEvent(
    input$sites,
    {
      if (updating_selects()) {
        return()
      }
      current_sites <- site_data() %>%
        filter(Description %in% input$descriptions) %>%
        pull(SamplingPoint)
      if (setequal(input$sites, current_sites)) {
        return()
      }
      updating_selects(TRUE)

      # Match descriptions to sites in the order sites were selected
      sel_desc <- sapply(input$sites, function(site) {
        site_data() %>%
          filter(SamplingPoint == site) %>%
          pull(Description) %>%
          head(1)
      })
      sel_desc <- unique(sel_desc)

      updateSelectizeInput(session, "descriptions", selected = sel_desc)

      later::later(function() updating_selects(FALSE), delay = 0.05)
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )

  observeEvent(
    input$descriptions,
    {
      if (updating_selects()) {
        return()
      }
      current_descs <- site_data() %>%
        filter(SamplingPoint %in% input$sites) %>%
        pull(Description)
      if (setequal(input$descriptions, current_descs)) {
        return()
      }
      updating_selects(TRUE)

      # Match sites to descriptions in the order descriptions were selected
      sel_sites <- sapply(input$descriptions, function(desc) {
        site_data() %>%
          filter(Description == desc) %>%
          pull(SamplingPoint)
      })
      sel_sites <- unique(unlist(sel_sites))

      updateSelectizeInput(session, "sites", selected = sel_sites)

      later::later(function() updating_selects(FALSE), delay = 0.05)
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )

  # --- Dynamic parameter list based on selected sites ---
  available_params <- reactive({
    req(input$sites)
    req(database_path())
    con <- dbConnect(duckdb::duckdb(), database_path())
    on.exit(dbDisconnect(con), add = TRUE)

    site_str <- paste(input$sites, collapse = "','")
    dbGetQuery(
      con,
      paste0(
        "SELECT DISTINCT WebParameter
     FROM WebLIMSResults
     WHERE SamplingPoint IN ('",
        site_str,
        "')"
      )
    )$WebParameter
  })

  observe({
    new_params <- sort(available_params())
    # keep current selection if still valid
    current_param <- isolate(input$parameter)
    if (!current_param %in% new_params) {
      current_param <- NULL
    }

    updateSelectizeInput(
      session,
      "parameter",
      choices = new_params,
      selected = current_param,
      server = TRUE
    )
  })

  # --- Step 4: Load data only when button is clicked ---
  get_data_for_plot_and_table <- eventReactive(input$update_data, {
    sites <- isolate(input$sites)
    parameter <- isolate(input$parameter)

    req(sites, parameter)
    req(database_path())
    con <- dbConnect(duckdb::duckdb(), database_path())
    on.exit(dbDisconnect(con), add = TRUE)

    site_str <- paste(input$sites, collapse = "','")
    query <- paste0(
      "SELECT SamplingPoint, DateSampled, WebResult, Qualifier,
       RelativeDepthSample, WebParameter, WebLIMSStations.Description
       FROM WebLIMSResults
       LEFT JOIN WebLIMSStations ON WebLIMSResults.SamplingPoint = WebLIMSStations.StationID
       WHERE SamplingPoint IN ('",
      site_str,
      "') AND WebParameter = '",
      input$parameter,
      "'"
    )
    raw_data <- dbGetQuery(con, query)

    # Return NULL if no data
    if (nrow(raw_data) == 0) {
      return(NULL)
    }

    # Data cleaning
    clean_data <- raw_data %>%
      rename(
        FinalResult = WebResult,
        Qualifiers = Qualifier,
        RelativeDepthComments = RelativeDepthSample
      ) %>%
      select(
        SamplingPoint,
        RelativeDepthComments,
        DateSampled,
        WebParameter,
        FinalResult,
        Qualifiers,
        Description
      ) %>%
      mutate(
        DateSampled = as.Date(DateSampled),
        FinalResult = as.character(FinalResult),
        FinalResult = gsub("[^0-9.<>=]", "", FinalResult),
        DL = case_when(
          grepl("^>", FinalResult) ~ ">DL",
          grepl("^<", FinalResult) ~ "<DL",
          TRUE ~ "Measured value"
        ),
        FinalResult = case_when(
          grepl("^>", FinalResult) ~
            suppressWarnings(as.numeric(sub(">", "", FinalResult))),
          grepl("^<", FinalResult) ~
            suppressWarnings(as.numeric(sub("<", "", FinalResult))) / 2,
          TRUE ~ suppressWarnings(as.numeric(trimws(FinalResult)))
        ),
        Qualifiers = if_else(
          is.na(Qualifiers) | Qualifiers == "",
          "None",
          Qualifiers
        ),
        RelativeDepthComments = if_else(
          is.na(trimws(RelativeDepthComments)) |
            trimws(RelativeDepthComments) == "",
          "surface",
          tolower(trimws(RelativeDepthComments))
        )
      ) %>%
      mutate(
        legend_shape = case_when(
          # Make a column for the legend shape with generic Qualifier label, or DL or measured Value
          Qualifiers != "None" ~ "Qualifier",
          TRUE ~ DL
        )
      ) %>%
      filter(!is.na(FinalResult)) %>%
      mutate(across(c(Qualifiers, DL), as.factor)) %>%
      mutate(
        RelativeDepthComments = factor(
          RelativeDepthComments,
          levels = c(
            "surface",
            "epilimnion",
            "thermocline",
            "mid-depth",
            "hypolimnion"
          )
        )
      )

    return(clean_data)
  })

  # Update date range immediately after data loads
  observeEvent(input$update_data, {
    df <- get_data_for_plot_and_table()
    if (!is.null(df) && nrow(df) > 0) {
      updateDateInput(session, "date_start", value = min(df$DateSampled))
      updateDateInput(session, "date_end", value = max(df$DateSampled))
    }
  })

  # Message about sites with missing data:
  observeEvent(input$update_data, {
    df <- get_data_for_plot_and_table()

    # Use isolate() to prevent reactivity on input changes
    selected_sites <- isolate(input$sites)
    selected_param <- isolate(input$parameter)

    # Determine missing sites
    missing_sites <- setdiff(selected_sites, unique(df$SamplingPoint))

    # Render message
    output$missing_sites_message <- renderUI({
      if (length(missing_sites) > 0) {
        HTML(
          paste0(
            "<b>No data for parameter '",
            selected_param,
            "' available for sites: ",
            paste(missing_sites, collapse = ", "),
            ".</b>"
          )
        )
      } else {
        NULL
      }
    })
  })

  selected_sites <- reactiveVal(NULL)
  selected_param <- reactiveVal(NULL)

  observeEvent(input$update_data, {
    selected_sites(isolate(input$sites))
    selected_param(isolate(input$parameter))
  })

  # --- Step 5: Filter by date ---
  filtered_data <- reactive({
    df <- get_data_for_plot_and_table()
    req(df, nrow(df) > 0)

    date_start <- input$date_start
    date_end <- input$date_end
    req(date_start, date_end)

    df <- df %>%
      filter(
        DateSampled >= date_start,
        DateSampled <= date_end
      )

    df
  })

  # --- Step 6–8: Plot / download / table ---
  make_plot <- function(selected_plot_dat) {
    # Check if RelativeDepthComments has any non-NA values
    use_size <- any(selected_plot_dat$RelativeDepthComments != "surface")

    base_aes <- aes(
      x = DateSampled,
      y = FinalResult,
      color = SamplingPoint,
      shape = legend_shape,
      tooltip = paste0(
        "Site: ",
        SamplingPoint,
        "<br>Date: ",
        DateSampled,
        "<br>Result: ",
        FinalResult,
        "<br>Qualifier: ",
        Qualifiers,
        "<br>Depth: ",
        RelativeDepthComments
      )
    )

    p <- ggplot(selected_plot_dat, base_aes) +
      {
        if (use_size) {
          geom_point_interactive(
            aes(size = RelativeDepthComments),
            alpha = 0.7
          )
        } else {
          geom_point_interactive(alpha = 0.7, size = 2.5)
        }
      } +
      scale_shape_manual(
        values = c(
          "Measured value" = 16,
          "<DL" = 25,
          ">DL" = 24,
          "Qualifier" = 5
        )
      ) +
      {
        if (use_size) {
          scale_size_manual(
            values = c(
              "epilimnion" = 2.5,
              "hypolimnion" = 5.5,
              "thermocline" = 4,
              "mid-depth" = 4,
              "surface" = 2.5
            ),
            name = "Relative Depth",
            drop = TRUE
          )
        }
      } +
      scale_x_date(date_labels = "%Y-%m-%d") +
      theme_classic(base_size = 14) +
      labs(
        x = "Date",
        y = "Result",
        color = "Site",
        shape = "Values"
      ) +
      ggtitle(selected_param()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # --- Trend line logic ---
    if (input$trend_type == "Linear") {
      p <- p +
        geom_smooth(
          method = "lm",
          se = FALSE,
          aes(group = SamplingPoint, color = SamplingPoint),
          linetype = "dashed"
        )
    } else if (input$trend_type == "LOESS") {
      p <- p +
        geom_smooth(
          method = "loess",
          span = 0.8,
          se = FALSE,
          aes(group = SamplingPoint, color = SamplingPoint),
          linetype = "dashed"
        )
    }

    p
  }

  output$plot <- renderGirafe({
    dat <- filtered_data()
    req(nrow(dat) > 0)
    girafe(
      ggobj = make_plot(dat),
      options = list(opts_toolbar(saveaspng = FALSE))
    )
  })

  # Track if plot is being shown:
  show_plot <- reactive({
    df <- get_data_for_plot_and_table()
    !is.null(df) && nrow(df) > 0
  })

  # Only show plot/options box when there is a plot to show:
  output$show_plot <- reactive({
    show_plot()
  })
  outputOptions(output, "show_plot", suspendWhenHidden = FALSE)

  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(
        paste(selected_sites(), collapse = "-"),
        "_",
        selected_param(),
        "_plot.png"
      )
    },
    content = function(file) {
      ggsave(file, make_plot(filtered_data()), width = 6, height = 4, dpi = 300)
    }
  )

  output$data_table <- renderDT({
    dat <- filtered_data()
    req(nrow(dat) > 0)
    dat$DateSampled <- format(dat$DateSampled, "%Y-%m-%d")
    datatable(
      dat[, c(
        "SamplingPoint",
        "WebParameter",
        "DateSampled",
        "FinalResult",
        "DL",
        "Qualifiers",
        "RelativeDepthComments"
      )],
      colnames = c(
        "Site",
        "Parameter",
        "Date",
        "Result",
        "Detection Limit",
        "Qualifiers",
        "Relative Depth"
      ),
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        dom = "Bfrtip",
        buttons = list(list(
          extend = "csv",
          text = "Download CSV",
          filename = paste0(
            paste(input$sites, collapse = "-"),
            "_",
            input$parameter
          )
        )),
        searching = FALSE
      ),
      rownames = FALSE
    )
  })

  # ============================================================
  # TAB 2: ONE SITE, MULTIPLE PARAMETERS
  # ============================================================

  # --- Sync site <-> description for Tab 2 ---
  updating_selects2 <- reactiveVal(FALSE)

  observeEvent(
    input$site_single,
    {
      if (updating_selects2()) {
        return()
      }
      updating_selects2(TRUE)

      sel_desc <- site_data() %>%
        filter(SamplingPoint == input$site_single) %>%
        pull(Description) %>%
        head(1)

      updateSelectizeInput(session, "description_single", selected = sel_desc)

      later::later(function() updating_selects2(FALSE), delay = 0.05)
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$description_single,
    {
      if (updating_selects2()) {
        return()
      }
      updating_selects2(TRUE)

      sel_site <- site_data() %>%
        filter(Description == input$description_single) %>%
        pull(SamplingPoint) %>%
        head(1)

      updateSelectizeInput(session, "site_single", selected = sel_site)

      later::later(function() updating_selects2(FALSE), delay = 0.05)
    },
    ignoreInit = TRUE
  )

  # --- Dynamic parameter list based on selected site ---
  available_params2 <- reactive({
    req(input$site_single)
    req(database_path())
    con <- dbConnect(duckdb::duckdb(), database_path())
    on.exit(dbDisconnect(con), add = TRUE)

    dbGetQuery(
      con,
      paste0(
        "SELECT DISTINCT WebParameter
     FROM WebLIMSResults
     WHERE SamplingPoint = '",
        input$site_single,
        "'"
      )
    )$WebParameter
  })

  observe({
    new_params <- sort(available_params2())
    # keep current selection if still valid
    current_params <- isolate(input$parameters_multi)
    valid_params <- intersect(current_params, new_params)

    updateSelectizeInput(
      session,
      "parameters_multi",
      choices = new_params,
      selected = valid_params,
      server = TRUE
    )
  })

  # --- Load data only when button is clicked (Tab 2) ---
  get_data_for_plot_and_table2 <- eventReactive(input$update_data2, {
    site <- isolate(input$site_single)
    parameters <- isolate(input$parameters_multi)

    req(site, parameters)
    req(database_path())
    con <- dbConnect(duckdb::duckdb(), database_path())
    on.exit(dbDisconnect(con), add = TRUE)

    param_str <- paste(parameters, collapse = "','")
    query <- paste0(
      "SELECT SamplingPoint, DateSampled, WebResult, Qualifier,
       RelativeDepthSample, WebParameter, WebLIMSStations.Description
       FROM WebLIMSResults
       LEFT JOIN WebLIMSStations ON WebLIMSResults.SamplingPoint = WebLIMSStations.StationID
       WHERE SamplingPoint = '",
      site,
      "' AND WebParameter IN ('",
      param_str,
      "')"
    )
    raw_data <- dbGetQuery(con, query)

    # Return NULL if no data
    if (nrow(raw_data) == 0) {
      return(NULL)
    }

    # Data cleaning
    clean_data <- raw_data %>%
      rename(
        FinalResult = WebResult,
        Qualifiers = Qualifier,
        RelativeDepthComments = RelativeDepthSample
      ) %>%
      select(
        SamplingPoint,
        RelativeDepthComments,
        DateSampled,
        WebParameter,
        FinalResult,
        Qualifiers,
        Description
      ) %>%
      mutate(
        DateSampled = as.Date(DateSampled),
        FinalResult = as.character(FinalResult),
        FinalResult = gsub("[^0-9.<>=]", "", FinalResult),
        DL = case_when(
          grepl("^>", FinalResult) ~ ">DL",
          grepl("^<", FinalResult) ~ "<DL",
          TRUE ~ "Measured value"
        ),
        FinalResult = case_when(
          grepl("^>", FinalResult) ~
            suppressWarnings(as.numeric(sub(">", "", FinalResult))),
          grepl("^<", FinalResult) ~
            suppressWarnings(as.numeric(sub("<", "", FinalResult))) / 2,
          TRUE ~ suppressWarnings(as.numeric(trimws(FinalResult)))
        ),
        Qualifiers = if_else(
          is.na(Qualifiers) | Qualifiers == "",
          "None",
          Qualifiers
        ),
        RelativeDepthComments = if_else(
          is.na(trimws(RelativeDepthComments)) |
            trimws(RelativeDepthComments) == "",
          "surface",
          tolower(trimws(RelativeDepthComments))
        )
      ) %>%
      mutate(
        legend_shape = case_when(
          # Make a column for the legend shape with generic Qualifier label, or DL or measured Value
          Qualifiers != "None" ~ "Qualifier",
          TRUE ~ DL
        )
      ) %>%
      filter(!is.na(FinalResult)) %>%
      mutate(across(c(Qualifiers, DL), as.factor)) %>%
      mutate(
        RelativeDepthComments = factor(
          RelativeDepthComments,
          levels = c(
            "surface",
            "epilimnion",
            "thermocline",
            "mid-depth",
            "hypolimnion"
          )
        )
      )

    return(clean_data)
  })

  # Update date range immediately after data loads (Tab 2)
  observeEvent(input$update_data2, {
    df <- get_data_for_plot_and_table2()
    if (!is.null(df) && nrow(df) > 0) {
      updateDateInput(session, "date_start2", value = min(df$DateSampled))
      updateDateInput(session, "date_end2", value = max(df$DateSampled))
    }
  })

  # Message about parameters with missing data (Tab 2):
  observeEvent(input$update_data2, {
    df <- get_data_for_plot_and_table2()

    selected_site <- isolate(input$site_single)
    selected_params <- isolate(input$parameters_multi)

    # Determine missing parameters
    missing_params <- setdiff(selected_params, unique(df$WebParameter))

    # Render message
    output$missing_params_message <- renderUI({
      if (length(missing_params) > 0) {
        HTML(
          paste0(
            "<b>No data for site '",
            selected_site,
            "' available for parameters: ",
            paste(missing_params, collapse = ", "),
            ".</b>"
          )
        )
      } else {
        NULL
      }
    })
  })

  selected_site2 <- reactiveVal(NULL)
  selected_params2 <- reactiveVal(NULL)

  observeEvent(input$update_data2, {
    selected_site2(isolate(input$site_single))
    selected_params2(isolate(input$parameters_multi))
  })

  # --- Filter by date (Tab 2) ---
  filtered_data2 <- reactive({
    df <- get_data_for_plot_and_table2()
    req(df, nrow(df) > 0)

    date_start <- input$date_start2
    date_end <- input$date_end2
    req(date_start, date_end)

    df <- df %>%
      filter(
        DateSampled >= date_start,
        DateSampled <= date_end
      )

    df
  })

  # --- Plot / download / table (Tab 2) ---
  make_plot2 <- function(selected_plot_dat) {
    # Check if RelativeDepthComments has any non-NA/non-"surface" values
    use_size <- any(selected_plot_dat$RelativeDepthComments != "surface")

    base_aes <- aes(
      x = DateSampled,
      y = FinalResult,
      color = WebParameter,
      shape = legend_shape,
      tooltip = paste0(
        "Parameter: ",
        WebParameter,
        "<br>Date: ",
        DateSampled,
        "<br>Result: ",
        FinalResult,
        "<br>Qualifier: ",
        Qualifiers,
        "<br>Depth: ",
        RelativeDepthComments
      )
    )

    p <- ggplot(selected_plot_dat, base_aes) +
      {
        if (use_size) {
          geom_point_interactive(
            aes(size = RelativeDepthComments),
            alpha = 0.7
          )
        } else {
          geom_point_interactive(alpha = 0.7, size = 2.5)
        }
      } +
      scale_shape_manual(
        values = c(
          "Measured value" = 16,
          "<DL" = 25,
          ">DL" = 24,
          "Qualifier" = 5
        )
      ) +
      {
        if (use_size) {
          scale_size_manual(
            values = c(
              "epilimnion" = 2.5,
              "hypolimnion" = 5.5,
              "thermocline" = 4,
              "mid-depth" = 4,
              "surface" = 2.5
            ),
            name = "Relative Depth",
            drop = TRUE
          )
        }
      } +
      theme_classic(base_size = 14) +
      labs(x = "Date", y = "Result", color = "Parameter", shape = "Values") +
      scale_color_discrete(labels = function(x) {
        stringr::str_wrap(x, width = 30)
      }) +
      guides(
        color = guide_legend(label.hjust = 0, ncol = 1, byrow = TRUE),
        shape = guide_legend(label.hjust = 0, ncol = 1, byrow = TRUE)
      ) +
      ggtitle(selected_site2()) +
      scale_x_date(date_labels = "%Y-%m-%d") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 10)
      )

    # --- Trend line logic ---
    if (input$trend_type2 == "Linear") {
      p <- p +
        geom_smooth(
          method = "lm",
          se = FALSE,
          aes(group = WebParameter, color = WebParameter),
          linetype = "dashed"
        )
    } else if (input$trend_type2 == "LOESS") {
      p <- p +
        geom_smooth(
          method = "loess",
          span = 0.8,
          se = FALSE,
          aes(group = WebParameter, color = WebParameter),
          linetype = "dashed"
        )
    }

    return(p)
  }

  output$plot2 <- renderGirafe({
    dat <- filtered_data2()
    req(nrow(dat) > 0)
    girafe(
      ggobj = make_plot2(dat),
      options = list(opts_toolbar(saveaspng = FALSE))
    )
  })

  # Track if plot is being shown (Tab 2):
  show_plot2 <- reactive({
    df <- get_data_for_plot_and_table2()
    !is.null(df) && nrow(df) > 0
  })

  output$show_plot2 <- reactive({
    show_plot2()
  })
  outputOptions(output, "show_plot2", suspendWhenHidden = FALSE)

  output$download_plot2 <- downloadHandler(
    filename = function() {
      paste0(
        selected_site2(),
        "_",
        paste(selected_params2(), collapse = "-"),
        "_plot.png"
      )
    },
    content = function(file) {
      ggsave(
        file,
        make_plot2(filtered_data2()),
        width = 6,
        height = 4,
        dpi = 300
      )
    }
  )

  output$data_table2 <- renderDT({
    dat <- filtered_data2()
    req(nrow(dat) > 0)
    dat$DateSampled <- format(dat$DateSampled, "%Y-%m-%d")
    datatable(
      dat[, c(
        "SamplingPoint",
        "WebParameter",
        "DateSampled",
        "FinalResult",
        "DL",
        "Qualifiers",
        "RelativeDepthComments"
      )],
      colnames = c(
        "Site",
        "Parameter",
        "Date",
        "Result",
        "Detection Limit",
        "Qualifiers",
        "Relative Depth"
      ),
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        dom = "Bfrtip",
        buttons = list(list(
          extend = "csv",
          text = "Download CSV",
          filename = paste0(
            input$site_single,
            "_",
            paste(input$parameters_multi, collapse = "-")
          )
        )),
        searching = FALSE
      ),
      rownames = FALSE
    )
  })

  # ============================================================
  # TAB 3: MULTIPLE SITES, MULTIPLE PARAMETERS
  # ============================================================

  # --- Sync sites <-> descriptions for Tab 3 ---
  updating_selects3 <- reactiveVal(FALSE)

  observeEvent(
    input$sites_multi3,
    {
      if (updating_selects3()) {
        return()
      }
      current_sites <- site_data() %>%
        filter(Description %in% input$descriptions_multi3) %>%
        pull(SamplingPoint)
      if (setequal(input$sites_multi3, current_sites)) {
        return()
      }
      updating_selects3(TRUE)

      sel_desc <- sapply(input$sites_multi3, function(site) {
        site_data() %>%
          filter(SamplingPoint == site) %>%
          pull(Description) %>%
          head(1)
      })
      sel_desc <- unique(sel_desc)

      updateSelectizeInput(session, "descriptions_multi3", selected = sel_desc)

      later::later(function() updating_selects3(FALSE), delay = 0.05)
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )

  observeEvent(
    input$descriptions_multi3,
    {
      if (updating_selects3()) {
        return()
      }
      current_descs <- site_data() %>%
        filter(SamplingPoint %in% input$sites_multi3) %>%
        pull(Description)
      if (setequal(input$descriptions_multi3, current_descs)) {
        return()
      }
      updating_selects3(TRUE)

      sel_sites <- sapply(input$descriptions_multi3, function(desc) {
        site_data() %>%
          filter(Description == desc) %>%
          pull(SamplingPoint)
      })
      sel_sites <- unique(unlist(sel_sites))

      updateSelectizeInput(session, "sites_multi3", selected = sel_sites)

      later::later(function() updating_selects3(FALSE), delay = 0.05)
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )

  # --- Dynamic parameter list based on selected sites (Tab 3) ---
  available_params3 <- reactive({
    req(input$sites_multi3)
    req(database_path())
    con <- dbConnect(duckdb::duckdb(), database_path())
    on.exit(dbDisconnect(con), add = TRUE)

    site_str <- paste(input$sites_multi3, collapse = "','")
    dbGetQuery(
      con,
      paste0(
        "SELECT DISTINCT WebParameter
     FROM WebLIMSResults
     WHERE SamplingPoint IN ('",
        site_str,
        "')"
      )
    )$WebParameter
  })

  observe({
    new_params <- sort(available_params3())
    current_params <- isolate(input$parameters_multi3)
    valid_params <- intersect(current_params, new_params)

    updateSelectizeInput(
      session,
      "parameters_multi3",
      choices = new_params,
      selected = valid_params,
      server = TRUE
    )
  })

  # --- Load data only when button is clicked (Tab 3) ---
  get_data_for_plot_and_table3 <- eventReactive(input$update_data3, {
    sites <- isolate(input$sites_multi3)
    parameters <- isolate(input$parameters_multi3)

    req(sites, parameters)
    req(database_path())
    con <- dbConnect(duckdb::duckdb(), database_path())
    on.exit(dbDisconnect(con), add = TRUE)

    site_str <- paste(sites, collapse = "','")
    param_str <- paste(parameters, collapse = "','")
    query <- paste0(
      "SELECT SamplingPoint, DateSampled, WebResult, Qualifier,
       RelativeDepthSample, WebParameter, WebLIMSStations.Description
       FROM WebLIMSResults
       LEFT JOIN WebLIMSStations ON WebLIMSResults.SamplingPoint = WebLIMSStations.StationID
       WHERE SamplingPoint IN ('",
      site_str,
      "') AND WebParameter IN ('",
      param_str,
      "')"
    )
    raw_data <- dbGetQuery(con, query)

    # Return NULL if no data
    if (nrow(raw_data) == 0) {
      return(NULL)
    }

    # Data cleaning
    clean_data <- raw_data %>%
      rename(
        FinalResult = WebResult,
        Qualifiers = Qualifier,
        RelativeDepthComments = RelativeDepthSample
      ) %>%
      select(
        SamplingPoint,
        RelativeDepthComments,
        DateSampled,
        WebParameter,
        FinalResult,
        Qualifiers,
        Description
      ) %>%
      mutate(
        DateSampled = as.Date(DateSampled),
        FinalResult = as.character(FinalResult),
        FinalResult = gsub("[^0-9.<>=]", "", FinalResult),
        DL = case_when(
          grepl("^>", FinalResult) ~ ">DL",
          grepl("^<", FinalResult) ~ "<DL",
          TRUE ~ "Measured value"
        ),
        FinalResult = case_when(
          grepl("^>", FinalResult) ~
            suppressWarnings(as.numeric(sub(">", "", FinalResult))),
          grepl("^<", FinalResult) ~
            suppressWarnings(as.numeric(sub("<", "", FinalResult))) / 2,
          TRUE ~ suppressWarnings(as.numeric(trimws(FinalResult)))
        ),
        Qualifiers = if_else(
          is.na(Qualifiers) | Qualifiers == "",
          "None",
          Qualifiers
        ),
        RelativeDepthComments = if_else(
          is.na(trimws(RelativeDepthComments)) |
            trimws(RelativeDepthComments) == "",
          "surface",
          tolower(trimws(RelativeDepthComments))
        )
      ) %>%
      mutate(
        legend_shape = case_when(
          # Make a column for the legend shape with generic Qualifier label, or DL or measured Value
          Qualifiers != "None" ~ "Qualifier",
          TRUE ~ DL
        )
      ) %>%
      filter(!is.na(FinalResult)) %>%
      mutate(across(c(Qualifiers, DL), as.factor)) %>%
      mutate(
        RelativeDepthComments = factor(
          RelativeDepthComments,
          levels = c(
            "surface",
            "epilimnion",
            "thermocline",
            "mid-depth",
            "hypolimnion"
          )
        )
      )

    # Create combined grouping variable for site-parameter combinations
    clean_data <- clean_data %>%
      mutate(SiteParam = paste(SamplingPoint, WebParameter, sep = " - "))

    return(clean_data)
  })

  # Update date range immediately after data loads (Tab 3)
  observeEvent(input$update_data3, {
    df <- get_data_for_plot_and_table3()
    if (!is.null(df) && nrow(df) > 0) {
      updateDateInput(session, "date_start3", value = min(df$DateSampled))
      updateDateInput(session, "date_end3", value = max(df$DateSampled))
    }
  })

  # Message about missing data (Tab 3):
  observeEvent(input$update_data3, {
    df <- get_data_for_plot_and_table3()

    selected_sites <- isolate(input$sites_multi3)
    selected_params <- isolate(input$parameters_multi3)

    # Create all possible combinations
    all_combos <- expand.grid(
      Site = selected_sites,
      Parameter = selected_params,
      stringsAsFactors = FALSE
    )

    # Get actual combinations in data
    if (!is.null(df) && nrow(df) > 0) {
      actual_combos <- df %>%
        select(SamplingPoint, WebParameter) %>%
        distinct() %>%
        rename(Site = SamplingPoint, Parameter = WebParameter)

      # Find missing combinations
      missing_combos <- anti_join(
        all_combos,
        actual_combos,
        by = c("Site", "Parameter")
      )

      output$missing_data_message3 <- renderUI({
        if (nrow(missing_combos) > 0) {
          missing_text <- paste(
            paste0(missing_combos$Site, " (", missing_combos$Parameter, ")"),
            collapse = ", "
          )
          HTML(
            paste0(
              "<b>No data available for the following site-parameter combinations: ",
              missing_text,
              ".</b>"
            )
          )
        } else {
          NULL
        }
      })
    } else {
      output$missing_data_message3 <- renderUI({
        HTML("<b>No data available for selected sites and parameters.</b>")
      })
    }
  })

  selected_sites3 <- reactiveVal(NULL)
  selected_params3 <- reactiveVal(NULL)

  observeEvent(input$update_data3, {
    selected_sites3(isolate(input$sites_multi3))
    selected_params3(isolate(input$parameters_multi3))
  })

  # --- Filter by date (Tab 3) ---
  filtered_data3 <- reactive({
    df <- get_data_for_plot_and_table3()
    req(df, nrow(df) > 0)

    date_start <- input$date_start3
    date_end <- input$date_end3
    req(date_start, date_end)

    df <- df %>%
      filter(
        DateSampled >= date_start,
        DateSampled <= date_end
      )

    df
  })

  # --- Plot / download / table (Tab 3) ---
  make_plot3 <- function(selected_plot_dat) {
    # Check if RelativeDepthComments has any non-"surface" values
    use_size <- any(selected_plot_dat$RelativeDepthComments != "surface")

    base_aes <- aes(
      x = DateSampled,
      y = FinalResult,
      color = SiteParam,
      shape = legend_shape,
      tooltip = paste0(
        "Site: ",
        SamplingPoint,
        "<br>Parameter: ",
        WebParameter,
        "<br>Date: ",
        DateSampled,
        "<br>Result: ",
        FinalResult,
        "<br>Qualifier: ",
        Qualifiers,
        "<br>Depth: ",
        RelativeDepthComments
      )
    )

    p <- ggplot(selected_plot_dat, base_aes) +
      {
        if (use_size) {
          geom_point_interactive(
            aes(size = RelativeDepthComments),
            alpha = 0.7
          )
        } else {
          geom_point_interactive(alpha = 0.7, size = 2.5)
        }
      } +
      scale_shape_manual(
        values = c(
          "Measured value" = 16,
          "<DL" = 25,
          ">DL" = 24,
          "Qualifier" = 5
        )
      ) +
      {
        if (use_size) {
          scale_size_manual(
            values = c(
              "epilimnion" = 2.5,
              "hypolimnion" = 5.5,
              "thermocline" = 4,
              "mid-depth" = 4,
              "surface" = 2.5
            ),
            name = "Relative Depth",
            drop = TRUE
          )
        }
      } +
      theme_classic(base_size = 14) +
      labs(
        x = "Date",
        y = "Result",
        color = "Site - Parameter",
        shape = "Values"
      ) +
      scale_color_discrete(labels = function(x) {
        stringr::str_wrap(x, width = 25)
      }) +
      guides(
        color = guide_legend(label.hjust = 0, ncol = 1, byrow = TRUE),
        shape = guide_legend(label.hjust = 0, ncol = 1, byrow = TRUE)
      ) +
      ggtitle("Multiple Sites & Parameters") +
      scale_x_date(date_labels = "%Y-%m-%d") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 9)
      )

    # --- Trend line logic ---
    if (input$trend_type3 == "Linear") {
      p <- p +
        geom_smooth(
          method = "lm",
          se = FALSE,
          aes(group = SiteParam, color = SiteParam),
          linetype = "dashed"
        )
    } else if (input$trend_type3 == "LOESS") {
      p <- p +
        geom_smooth(
          method = "loess",
          span = 0.8,
          se = FALSE,
          aes(group = SiteParam, color = SiteParam),
          linetype = "dashed"
        )
    }

    return(p)
  }

  output$plot3 <- renderGirafe({
    dat <- filtered_data3()
    req(nrow(dat) > 0)
    girafe(
      ggobj = make_plot3(dat),
      options = list(opts_toolbar(saveaspng = FALSE))
    )
  })

  # Track if plot is being shown (Tab 3):
  show_plot3 <- reactive({
    df <- get_data_for_plot_and_table3()
    !is.null(df) && nrow(df) > 0
  })

  output$show_plot3 <- reactive({
    show_plot3()
  })
  outputOptions(output, "show_plot3", suspendWhenHidden = FALSE)

  output$download_plot3 <- downloadHandler(
    filename = function() {
      paste0(
        paste(selected_sites3(), collapse = "-"),
        "_",
        paste(selected_params3(), collapse = "-"),
        "_plot.png"
      )
    },
    content = function(file) {
      ggsave(
        file,
        make_plot3(filtered_data3()),
        width = 8,
        height = 6,
        dpi = 300
      )
    }
  )

  output$data_table3 <- renderDT({
    dat <- filtered_data3()
    req(nrow(dat) > 0)
    dat$DateSampled <- format(dat$DateSampled, "%Y-%m-%d")
    datatable(
      dat[, c(
        "SamplingPoint",
        "WebParameter",
        "DateSampled",
        "FinalResult",
        "DL",
        "Qualifiers",
        "RelativeDepthComments"
      )],
      colnames = c(
        "Site",
        "Parameter",
        "Date",
        "Result",
        "Detection Limit",
        "Qualifiers",
        "Relative Depth"
      ),
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        dom = "Bfrtip",
        buttons = list(list(
          extend = "csv",
          text = "Download CSV",
          filename = paste0(
            paste(input$sites_multi3, collapse = "-"),
            "_",
            paste(input$parameters_multi3, collapse = "-")
          )
        )),
        searching = FALSE
      ),
      rownames = FALSE
    )
  })

  # ============================================================
  # TAB 4: FACETED MULTI-SITE / MULTI-PARAMETER PLOT
  # ============================================================

  # --- Sync sites <-> descriptions for Tab 3 ---
  updating_selects4 <- reactiveVal(FALSE)

  observeEvent(
    input$sites_multi4,
    {
      if (updating_selects4()) {
        return()
      }
      current_sites <- site_data() %>%
        filter(Description %in% input$descriptions_multi4) %>%
        pull(SamplingPoint)
      if (setequal(input$sites_multi4, current_sites)) {
        return()
      }
      updating_selects4(TRUE)

      sel_desc <- sapply(input$sites_multi4, function(site) {
        site_data() %>%
          filter(SamplingPoint == site) %>%
          pull(Description) %>%
          head(1)
      })
      updateSelectizeInput(
        session,
        "descriptions_multi4",
        selected = unique(sel_desc)
      )
      later::later(function() updating_selects4(FALSE), delay = 0.05)
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )

  observeEvent(
    input$descriptions_multi4,
    {
      if (updating_selects4()) {
        return()
      }
      current_descs <- site_data() %>%
        filter(SamplingPoint %in% input$sites_multi4) %>%
        pull(Description)
      if (setequal(input$descriptions_multi4, current_descs)) {
        return()
      }
      updating_selects4(TRUE)

      sel_sites <- sapply(input$descriptions_multi4, function(desc) {
        site_data() %>%
          filter(Description == desc) %>%
          pull(SamplingPoint)
      })
      updateSelectizeInput(
        session,
        "sites_multi4",
        selected = unique(unlist(sel_sites))
      )
      later::later(function() updating_selects4(FALSE), delay = 0.05)
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )

  # --- Dynamic parameter list based on selected sites (Tab 4) ---
  # Dynamic parameters
  available_params4 <- reactive({
    req(input$sites_multi4)
    con <- dbConnect(duckdb::duckdb(), database_path())
    on.exit(dbDisconnect(con), add = TRUE)
    site_str <- paste(input$sites_multi4, collapse = "','")
    dbGetQuery(
      con,
      paste0(
        "SELECT DISTINCT WebParameter FROM WebLIMSResults WHERE SamplingPoint IN ('",
        site_str,
        "')"
      )
    )$WebParameter
  })

  observe({
    new_params <- sort(available_params4())
    current_params <- isolate(input$parameters_multi4)
    valid_params <- intersect(current_params, new_params)
    updateSelectizeInput(
      session,
      "parameters_multi4",
      choices = new_params,
      selected = valid_params,
      server = TRUE
    )
  })

  # Load data on button click
  get_data_for_plot_and_table4 <- eventReactive(input$update_data4, {
    sites <- isolate(input$sites_multi4)
    parameters <- isolate(input$parameters_multi4)
    req(sites, parameters)
    con <- dbConnect(duckdb::duckdb(), database_path())
    on.exit(dbDisconnect(con), add = TRUE)

    site_str <- paste(sites, collapse = "','")
    param_str <- paste(parameters, collapse = "','")
    query <- paste0(
      "SELECT SamplingPoint, DateSampled, WebResult, Qualifier,
     RelativeDepthSample, WebParameter, WebLIMSStations.Description
     FROM WebLIMSResults
     LEFT JOIN WebLIMSStations ON WebLIMSResults.SamplingPoint = WebLIMSStations.StationID
     WHERE SamplingPoint IN ('",
      site_str,
      "') AND WebParameter IN ('",
      param_str,
      "')"
    )
    raw_data <- dbGetQuery(con, query)
    if (nrow(raw_data) == 0) {
      return(NULL)
    }

    # Same cleaning logic as Tab 3
    clean_data <- raw_data %>%
      rename(
        FinalResult = WebResult,
        Qualifiers = Qualifier,
        RelativeDepthComments = RelativeDepthSample
      ) %>%
      select(
        SamplingPoint,
        RelativeDepthComments,
        DateSampled,
        WebParameter,
        FinalResult,
        Qualifiers,
        Description
      ) %>%
      mutate(
        DateSampled = as.Date(DateSampled),
        FinalResult = as.character(FinalResult),
        FinalResult = gsub("[^0-9.<>=]", "", FinalResult),
        DL = case_when(
          grepl("^>", FinalResult) ~ ">DL",
          grepl("^<", FinalResult) ~ "<DL",
          TRUE ~ "Measured value"
        ),
        FinalResult = case_when(
          grepl("^>", FinalResult) ~
            suppressWarnings(as.numeric(sub(">", "", FinalResult))),
          grepl("^<", FinalResult) ~
            suppressWarnings(as.numeric(sub("<", "", FinalResult))) / 2,
          TRUE ~ suppressWarnings(as.numeric(trimws(FinalResult)))
        ),
        Qualifiers = if_else(
          is.na(Qualifiers) | Qualifiers == "",
          "None",
          Qualifiers
        ),
        RelativeDepthComments = if_else(
          is.na(trimws(RelativeDepthComments)) |
            trimws(RelativeDepthComments) == "",
          "surface",
          tolower(trimws(RelativeDepthComments))
        )
      ) %>%
      mutate(
        legend_shape = case_when(Qualifiers != "None" ~ "Qualifier", TRUE ~ DL)
      ) %>%
      filter(!is.na(FinalResult)) %>%
      mutate(across(c(Qualifiers, DL), as.factor)) %>%
      mutate(
        RelativeDepthComments = factor(
          RelativeDepthComments,
          levels = c(
            "surface",
            "epilimnion",
            "thermocline",
            "mid-depth",
            "hypolimnion"
          )
        )
      ) %>%
      mutate(SiteParam = paste(SamplingPoint, WebParameter, sep = " - "))

    return(clean_data)
  })

  # Update date range immediately after data loads (Tab 4)
  observeEvent(input$update_data4, {
    df <- get_data_for_plot_and_table4()
    if (!is.null(df) && nrow(df) > 0) {
      updateDateInput(session, "date_start4", value = min(df$DateSampled))
      updateDateInput(session, "date_end4", value = max(df$DateSampled))
    }
  })

  # Message about missing data (Tab 4):
  observeEvent(input$update_data4, {
    df <- get_data_for_plot_and_table4()

    selected_sites <- isolate(input$sites_multi4)
    selected_params <- isolate(input$parameters_multi4)

    # Create all possible combinations
    all_combos <- expand.grid(
      Site = selected_sites,
      Parameter = selected_params,
      stringsAsFactors = FALSE
    )

    # Get actual combinations in data
    if (!is.null(df) && nrow(df) > 0) {
      actual_combos <- df %>%
        select(SamplingPoint, WebParameter) %>%
        distinct() %>%
        rename(Site = SamplingPoint, Parameter = WebParameter)

      # Find missing combinations
      missing_combos <- anti_join(
        all_combos,
        actual_combos,
        by = c("Site", "Parameter")
      )

      output$missing_data_message4 <- renderUI({
        if (nrow(missing_combos) > 0) {
          missing_text <- paste(
            paste0(missing_combos$Site, " (", missing_combos$Parameter, ")"),
            collapse = ", "
          )
          HTML(
            paste0(
              "<b>No data available for the following site-parameter combinations: ",
              missing_text,
              ".</b>"
            )
          )
        } else {
          NULL
        }
      })
    } else {
      output$missing_data_message4 <- renderUI({
        HTML("<b>No data available for selected sites and parameters.</b>")
      })
    }
  })

  selected_sites4 <- reactiveVal(NULL)
  selected_params4 <- reactiveVal(NULL)

  observeEvent(input$update_data4, {
    selected_sites4(isolate(input$sites_multi4))
    selected_params4(isolate(input$parameters_multi4))
  })

  # --- Filter by date (Tab 4) ---
  filtered_data4 <- reactive({
    df <- get_data_for_plot_and_table4()
    req(df, nrow(df) > 0)

    date_start <- input$date_start4
    date_end <- input$date_end4
    req(date_start, date_end)

    df <- df %>%
      filter(
        DateSampled >= date_start,
        DateSampled <= date_end
      )

    df
  })

  # --- Plot / download / table (Tab 4) ---
  make_plot4 <- function(selected_plot_dat) {
    use_size <- any(selected_plot_dat$RelativeDepthComments != "surface")

    # Determine faceting
    facet_choice <- input$facet_type
    if (is.null(facet_choice)) {
      facet_choice <- "Site"
    }

    # Set facet and color variable for legend
    if (facet_choice == "Parameter") {
      color_var <- "SamplingPoint" # Legend shows Site
      facet_formula <- ~WebParameter
      color_label <- "Site"
    } else {
      color_var <- "WebParameter" # Legend shows Parameter
      facet_formula <- ~SamplingPoint
      color_label <- "Parameter"
    }

    # Compute Y-axis limits for all data (when using fixed y scale):
    y_range <- range(selected_plot_dat$FinalResult, na.rm = TRUE)

    # Base aesthetics
    base_aes <- aes(
      x = DateSampled,
      y = FinalResult,
      tooltip = paste0(
        "Site: ",
        SamplingPoint,
        "<br>Parameter: ",
        WebParameter,
        "<br>Date: ",
        DateSampled,
        "<br>Result: ",
        FinalResult,
        "<br>Qualifier: ",
        Qualifiers,
        "<br>Depth: ",
        RelativeDepthComments
      ),
      shape = legend_shape
    )

    # Color aesthetic only affects legend, actual grouping by SiteParam
    p <- ggplot(selected_plot_dat, base_aes) +
      {
        if (use_size) {
          geom_point_interactive(
            aes(
              size = RelativeDepthComments,
              color = .data[[color_var]],
              group = SiteParam
            ),
            alpha = 0.7
          )
        } else {
          geom_point_interactive(
            aes(color = .data[[color_var]], group = SiteParam),
            alpha = 0.7,
            size = 2.5
          )
        }
      } +
      scale_shape_manual(
        values = c(
          "Measured value" = 16,
          "<DL" = 25,
          ">DL" = 24,
          "Qualifier" = 5
        )
      ) +
      {
        if (use_size) {
          scale_size_manual(
            values = c(
              "epilimnion" = 2.5,
              "hypolimnion" = 5.5,
              "thermocline" = 4,
              "mid-depth" = 4,
              "surface" = 2.5
            ),
            name = "Relative Depth",
            drop = TRUE
          )
        }
      } +
      theme_classic(base_size = 14) +
      labs(
        x = "Date",
        y = "Result",
        color = color_label,
        shape = "Values"
      ) +
      scale_color_discrete(labels = function(x) {
        stringr::str_wrap(x, width = 25)
      }) +
      guides(
        color = guide_legend(label.hjust = 0, ncol = 1, byrow = TRUE),
        shape = guide_legend(label.hjust = 0, ncol = 1, byrow = TRUE)
      ) +
      #ggtitle("Multiple Sites & Parameters") +
      scale_x_date(date_labels = "%Y-%m-%d") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 9)
      ) +
      #facet_wrap(facet_formula, scales = "free_y") # Use this line to have Y axis scale to each data combo rather than fixed across all sites
      scale_y_continuous(limits = y_range) + # fixed scale across facets
      facet_wrap(facet_formula, scales = "fixed")

    # --- Trend line logic (grouped by SiteParam) ---
    if (input$trend_type4 == "Linear") {
      p <- p +
        geom_smooth(
          method = "lm",
          se = FALSE,
          aes(group = SiteParam, color = .data[[color_var]]),
          linetype = "dashed"
        )
    } else if (input$trend_type4 == "LOESS") {
      p <- p +
        geom_smooth(
          method = "loess",
          span = 0.8,
          se = FALSE,
          aes(group = SiteParam, color = .data[[color_var]]),
          linetype = "dashed"
        )
    }

    return(p)
  }

  output$plot4 <- renderGirafe({
    dat <- filtered_data4()
    req(nrow(dat) > 0)
    girafe(
      ggobj = make_plot4(dat),
      options = list(opts_toolbar(saveaspng = FALSE))
    )
  })

  # Track if plot is being shown (Tab 4):
  show_plot4 <- reactive({
    df <- get_data_for_plot_and_table4()
    !is.null(df) && nrow(df) > 0
  })

  output$show_plot4 <- reactive({
    show_plot4()
  })
  outputOptions(output, "show_plot4", suspendWhenHidden = FALSE)

  output$download_plot4 <- downloadHandler(
    filename = function() {
      paste0(
        paste(selected_sites4(), collapse = "-"),
        "_",
        paste(selected_params4(), collapse = "-"),
        "_plot.png"
      )
    },
    content = function(file) {
      ggsave(
        file,
        make_plot4(filtered_data4()),
        width = 8,
        height = 6,
        dpi = 300
      )
    }
  )

  output$data_table4 <- renderDT({
    dat <- filtered_data4()
    req(nrow(dat) > 0)
    dat$DateSampled <- format(dat$DateSampled, "%Y-%m-%d")
    datatable(
      dat[, c(
        "SamplingPoint",
        "WebParameter",
        "DateSampled",
        "FinalResult",
        "DL",
        "Qualifiers",
        "RelativeDepthComments"
      )],
      colnames = c(
        "Site",
        "Parameter",
        "Date",
        "Result",
        "Detection Limit",
        "Qualifiers",
        "Relative Depth"
      ),
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        dom = "Bfrtip",
        buttons = list(list(
          extend = "csv",
          text = "Download CSV",
          filename = paste0(
            paste(input$sites_multi4, collapse = "-"),
            "_",
            paste(input$parameters_multi4, collapse = "-")
          )
        )),
        searching = FALSE
      ),
      rownames = FALSE
    )
  })
}


#---------------------------------------------------------
shinyApp(ui, server)
