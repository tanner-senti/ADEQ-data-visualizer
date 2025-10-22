# Auto-install missing packages
packages <- c(
  "shiny",
  "dplyr",
  "DBI",
  "duckdb",
  "DT",
  "ggplot2",
  "ggiraph",
  "shinycssloaders",
  "shinyjs",
  "bslib",
  "stringr",
  "later",
  "ggimage"
)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

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
library(ggimage)

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
    div(
      style = "display: flex; align-items: center; gap: 30px; justify-content: center; width: 100%;",
      tags$a(
        href = 'https://www.adeq.state.ar.us/water/',
        tags$img(
          src = 'adeq_logo.png',
          height = 100,
          width = 80 * 2.85 * 1.75
        ),
        target = "_blank"
      ),
      titlePanel("Water Quality Explorer")
    ),
    htmlOutput("db_message"),

    br(),

    # Making input labels bold:
    tags$head(
      tags$style(HTML(
        "
.control-label {
  font-weight: bold;
}

#date_start4, #date_end4 {
  display: flex;
  align-items: center;
  margin-bottom: 10px;
}

#date_start4 label, #date_end4 label {
  display: inline-block;
  width: 50px;
  margin-right: 10px;
  margin-bottom: 0;
  font-weight: bold;
}

#date_start4 input, #date_end4 input {
  width: 120px !important;
  margin-left: 0px;
}

#date_end4 {
  margin-left: 8px;
}
  "
      ))
    ),

    # Input Panel
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
              "Expecto Patronum",
              style = "font-size:16px; padding:10px 30px; background-color:#0080b7; color:white; border:none; border-radius:8px;",
              icon = icon("sync")
            )
          )
        )
      )
    ),

    br(),
    div(
      style = "text-align: center; width:80%;",
      uiOutput("missing_data_message4")
    ),

    br(),

    # Plot and Plot Options side-by-side
    div(
      style = "display: flex; justify-content: center; width: 100%; gap: 20px; padding: 0 20px; box-sizing: border-box;",

      # Left: Plot
      div(
        style = "flex-grow: 4; min-width: 550px;  max-width: 680px;", # This line specifies the container that the plot is held within
        withSpinner(girafeOutput("plot4", width = "auto", height = "auto")) # This line specifies the plot sizes within the container
      ),

      # Right: Plot Options box
      conditionalPanel(
        condition = "output.show_plot4",
        wellPanel(
          style = "flex: 0 0 220px; max-height: 600px; background-color: #f8f9fa; border-radius: 4px;",
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
          #br(),
          radioButtons(
            "facet_type",
            "Facet by:",
            choices = c("Site", "Parameter"),
            selected = "Site",
            inline = FALSE
          ),
          #br(),
          radioButtons(
            "trend_type4",
            "Add trend line:",
            choices = c("None", "Linear", "LOESS"),
            selected = "None",
            inline = FALSE
          ),
          conditionalPanel(
            condition = "input.trend_type4 != 'None'",
            checkboxInput(
              "show_ci4",
              "Confidence interval",
              value = TRUE
            )
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

    # Conditionally show the stats table:
    conditionalPanel(
      condition = "output.show_plot4",
      div(
        style = "width: 50%; max-width: 700px;",
        conditionalPanel(
          condition = "input.trend_type4 == 'Linear'",
          # h5("Linear Model Statistics:"), Here and in Server for stats table
          uiOutput("stats_or_message4")
        ),
        uiOutput("loess_message4")
      )
    ),

    br(),

    # Data Table
    div(
      style = "display: flex; justify-content: center; width: 100%;",
      div(
        style = "width: 85%; max-width: 700px;",
        tags$style(HTML(
          "
      #data_table4 {
        font-size: 12px;
      }
      #data_table4 thead th {
        font-size: 13px;
      }
    "
        )),
        DTOutput("data_table4")
      )
    ),

    br(),

    div(
      style = "width: 100%; text-align: center; padding: 15px; margin-top: 20px; background-color: #fff;",
      a(
        "Click here for the Water Quality Monitoring Data webpage and details about qualifiers",
        href = "https://www.adeq.state.ar.us/techsvs/env_multi_lab/water_quality_station.aspx",
        target = "_blank",
        style = "color: #0080b7; text-decoration: none; font-weight: bold;"
      )
    ),

    tags$footer(
      style = "width: 100%; text-align: center; padding: 10px; margin-top: 40px;
             background-color: #f8f9fa; color: #6c757d; border-top: 1px solid #dee2e6;",
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

    # Main inputs
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
  # Main body (style 4): FACETED MULTI-SITE / MULTI-PARAMETER PLOT
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
            suppressWarnings(as.numeric(sub("<", "", FinalResult))),
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

  # Reactive observer for default facet behavior:
  observe({
    dat <- filtered_data4()
    req(nrow(dat) > 0)

    n_sites <- n_distinct(dat$SamplingPoint)
    n_params <- n_distinct(dat$WebParameter)

    # Determine default facet
    if (n_params > 1 && n_sites == 1) {
      default_facet <- "Site"
    } else if (n_sites > 1 && n_params == 1) {
      default_facet <- "Parameter"
    } else {
      default_facet <- "Site"
    }

    updateRadioButtons(session, "facet_type", selected = default_facet)
  })

  # Reactive for building the stats table w/ trend line:
  model_stats4 <- reactive({
    dat <- filtered_data4()
    req(nrow(dat) > 0)

    filtered_dat <- dat %>%
      filter(DL == "Measured value") %>%
      group_by(SamplingPoint, WebParameter) %>%
      filter(n() >= 8)

    # Return empty data frame if no groups meet criteria
    if (nrow(filtered_dat) == 0) {
      return(data.frame())
    }

    # print(summary(lm(filtered_dat$FinalResult ~ filtered_dat$DateSampled)))

    filtered_dat %>%
      summarise(
        N = n(),
        R_squared = summary(lm(FinalResult ~ DateSampled))$r.squared,
        `p-value` = summary(lm(FinalResult ~ DateSampled))$coefficients[2, 4],
        .groups = "drop"
      ) %>%
      rename("Site" = SamplingPoint, "Parameter" = WebParameter)
  })

  # --- Plot / download / table (Tab 4) ---
  make_plot4 <- function(selected_plot_dat) {
    use_size <- any(selected_plot_dat$RelativeDepthComments != "surface")

    # Create a uniqueID for interactive plot features:
    selected_plot_dat <- selected_plot_dat %>%
      mutate(.point_id = seq_len(n()))

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
    y_buffer <- diff(y_range) * 0.15 # Add 15% buffer
    y_range <- c(y_range[1] - y_buffer, y_range[2] + y_buffer)

    ## Map legend_shape to image paths
    selected_plot_dat <- selected_plot_dat %>%
      mutate(
        point_image = case_when(
          legend_shape == "Measured value" ~ "www/measured.png",
          legend_shape == "<DL" ~ "www/below_dl.png",
          legend_shape == ">DL" ~ "www/above_dl.png",
          legend_shape == "Qualifier" ~ "www/above_dl.png"
        )
      )

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
      data_id = .point_id
    )

    # Color aesthetic only affects legend, actual grouping by SiteParam
    p <- ggplot(selected_plot_dat, base_aes) +
      {
        if (use_size) {
          geom_image(
            aes(
              image = point_image,
              size = RelativeDepthSample,
              group = SiteParam
            )
          )
        } else {
          geom_image(
            aes(
              image = point_image,
              group = SiteParam
            ),
            size = 0.05
          )
        }
      } +
      # Add invisible points for color legend only
      geom_point(
        aes(color = .data[[color_var]]),
        alpha = 0, # completely transparent
        size = 0
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
      theme_classic(base_size = 11) +
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
        color = guide_legend(
          label.hjust = 0,
          ncol = if (length(unique(selected_plot_dat[[color_var]])) > 8) {
            2
          } else {
            1
          },
          byrow = TRUE
        ),
        shape = guide_legend(label.hjust = 0, ncol = 1, byrow = TRUE)
      ) +
      #ggtitle("Multiple Sites & Parameters") +
      scale_x_date(date_labels = "%Y-%m-%d") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        strip.text = element_text(size = 9, lineheight = 1)
      ) +
      coord_cartesian(ylim = y_range) + # fixed scale without clipping
      facet_wrap(
        facet_formula,
        scales = "fixed",
        labeller = labeller(.default = label_wrap_gen(width = 20))
      )

    # --- Trend line logic (Interactive - grouped by Site-Param) ---
    if (input$trend_type4 == "Linear") {
      # Filter to only groups with 8+ measured values
      linear_dat <- selected_plot_dat %>%
        filter(DL == "Measured value") %>%
        group_by(SiteParam, SamplingPoint, WebParameter, .add = TRUE) %>%
        filter(n() >= 8) %>%
        ungroup()

      if (nrow(linear_dat) > 0) {
        # Compute model stats per group for tooltips
        stats_df <- linear_dat %>%
          group_by(SiteParam, SamplingPoint, WebParameter) %>%
          summarise(
            N = n(),
            R2 = summary(lm(FinalResult ~ DateSampled))$r.squared,
            pval = summary(lm(FinalResult ~ DateSampled))$coefficients[2, 4],
            .groups = "drop"
          ) %>%
          mutate(
            tooltip = paste0(
              "Trend line<br>",
              "Site: ",
              SamplingPoint,
              "<br>",
              "Parameter: ",
              WebParameter,
              "<br>",
              "N: ",
              N,
              "<br>",
              "R²: ",
              sprintf("%.3f", R2),
              "<br>",
              "p-value: ",
              sprintf("%.4f", pval)
            )
          )

        # Join tooltips to linear data
        linear_dat <- left_join(
          linear_dat,
          stats_df,
          by = c("SiteParam", "SamplingPoint", "WebParameter")
        )

        p <- p +
          geom_smooth(
            data = linear_dat,
            method = "lm",
            se = input$show_ci4,
            inherit.aes = FALSE,
            aes(
              x = DateSampled,
              y = FinalResult,
              group = SiteParam,
              color = .data[[color_var]]
            ),
            alpha = 0.2, # faint confidence band
            linetype = "dashed"
          ) +
          # --- interactive dashed line only ---
          geom_smooth_interactive(
            data = linear_dat,
            method = "lm",
            se = FALSE,
            inherit.aes = FALSE,
            aes(
              x = DateSampled,
              y = FinalResult,
              group = SiteParam,
              color = .data[[color_var]],
              tooltip = tooltip,
              data_id = SiteParam
            ),
            linetype = "dashed",
            size = 0.8
          )
      }
    } else if (input$trend_type4 == "LOESS") {
      loess_dat <- selected_plot_dat %>%
        filter(!is.na(FinalResult))

      # Compute N per group
      stats_df <- loess_dat %>%
        group_by(SiteParam, SamplingPoint, WebParameter) %>%
        summarise(
          N = n(),
          .groups = "drop"
        )

      # Join N back to loess data
      loess_dat <- left_join(
        loess_dat,
        stats_df,
        by = c("SiteParam", "SamplingPoint", "WebParameter")
      )

      # --- non-interactive CI ribbon ---
      p <- p +
        geom_smooth(
          data = loess_dat,
          method = "loess",
          span = 0.8,
          se = input$show_ci4,
          inherit.aes = FALSE,
          aes(
            x = DateSampled,
            y = FinalResult,
            group = SiteParam,
            color = .data[[color_var]]
          ),
          alpha = 0.2, # faint confidence band
          linetype = "dashed"
        ) +
        # --- interactive line only ---
        geom_smooth_interactive(
          data = loess_dat,
          method = "loess",
          span = 0.8,
          se = FALSE,
          inherit.aes = FALSE,
          aes(
            x = DateSampled,
            y = FinalResult,
            group = SiteParam,
            color = .data[[color_var]],
            tooltip = paste0(
              "LOESS trend<br>",
              "Site: ",
              SamplingPoint,
              "<br>Parameter: ",
              WebParameter,
              "<br>N: ",
              N
            ),
            data_id = SiteParam
          ),
          linetype = "dashed",
          size = 0.8
        )
    }

    return(p)
  }

  output$plot4 <- renderGirafe({
    dat <- filtered_data4()

    req(nrow(dat) > 0)

    suppressMessages(girafe(
      ggobj = make_plot4(dat),
      options = list(
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none")
      )
    ))
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

  # Stats table for trend lines:
  output$stats_or_message4 <- renderUI({
    req(input$trend_type4 == "Linear")
    stats <- model_stats4()

    if (nrow(stats) == 0) {
      div(
        style = "padding: 10px; background-color: #fff3cd; border-left: 4px solid #ffc107; color: #333;",
        p(
          "No groups have 8 or more measured values. Linear model statistics are not available."
        )
      )
    } else {
      div(
        #dataTableOutput("stats_table4"), Here and line in UI for table
        div(
          style = "padding: 10px; background-color: #e7f3ff; border-left: 4px solid #0080b7; color: #333;", #"padding: 8px; font-size: 12px; color: #666; margin-top: 8px;",
          p(
            "Note: Values below or above detection limits were excluded from linear model analyses. Minimum of 8 samples required."
          )
        )
      )
    }
  })

  output$stats_table4 <- renderDataTable({
    req(nrow(model_stats4()) > 0)
    model_stats4() %>%
      mutate(
        R_squared = round(R_squared, 4),
        `p-value` = round(`p-value`, 4)
      ) %>%
      datatable(options = list(pageLength = 10, dom = 'tp'), rownames = FALSE)
  })
  # Message about LOESS:
  output$loess_message4 <- renderUI({
    if (input$trend_type4 == "LOESS") {
      div(
        style = "padding: 10px; background-color: #e7f3ff; border-left: 4px solid #0080b7; color: #333;",
        p(
          "LOESS is exploratory only. Detection limit values are included but formal statistics are not applicable. Minimum of 8 samples required."
        )
      )
    }
  })

  # Full data table output:
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
