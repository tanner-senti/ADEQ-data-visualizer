library(RSQLite)
library(DBI)
library(odbc)
library(fs) # For working with file sizes
library(dplyr)

# Specify the path for your SQLite database file
sqlite_file <- "Data/WebLIMS_sql.sqlite"

# Create an empty SQLite database
sqlite_con <- dbConnect(SQLite(), sqlite_file)

# Close the connection
dbDisconnect(sqlite_con)



# Set paths for your databases
access_file <- "Data/WQARWebLIMS_web.mdb"
sqlite_file <- "Data/WebLIMS_sql.sqlite"

# Connect to the Access database
access_con <- dbConnect(odbc::odbc(),
                        .connection_string = paste0(
                          "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
                          "Dbq=", access_file, ";"))

# Connect to the SQLite database
sqlite_con <- dbConnect(SQLite(), sqlite_file)

# List all tables in the Access database
tables <- c("WebLIMSResults", "WebLIMSStations")

# Loop through each table and copy it to SQLite
for (table in tables) {
  # Read data from Access
  data <- dbReadTable(access_con, table)
  
  # Write data to SQLite
  dbWriteTable(sqlite_con, table, data, overwrite = TRUE)
}

# Close connections
dbDisconnect(access_con)
dbDisconnect(sqlite_con)

# Compare file sizes
access_size <- file_size(access_file)
sqlite_size <- file_size(sqlite_file)

cat("Access Database Size:", access_size, "\n")
cat("SQLite Database Size:", sqlite_size, "\n")


# Downloading database ----

# Define constants
zip_url <- "https://www.adeq.state.ar.us/downloads/WebDatabases/TechnicalServices/WQARWebLIMS/WQARWebLIMS_web.zip"
fallback_data_path <- "Data/cleaned_agfc_lakes.csv"
#temp_dir <- tempdir()
temp_dir <- "Data"

# Download the zip file
zip_path <- file.path(temp_dir, "data.zip")
download.file(zip_url, zip_path, mode = "wb")

# Unzip the file
unzip(zip_path, exdir = temp_dir)
access_file <- list.files(temp_dir, pattern = "\\mdb$", full.names = TRUE)[1]

# Connect to the Access database
con <- dbConnect(odbc::odbc(), .connection_string = paste(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
  "Dbq=", access_file, ";",
  "Uid=Admin;Pwd=;", sep = ""
))

# Example: Read and clean data
raw_data <- dbReadTable(con, "WebLIMSResults")  # Replace with actual table name

clean_data <- raw_data %>%
  filter(SamplingPoint == "LOUA015A") %>% 
  filter(WebParameter == "Depth, Secchi disk depth (m)") %>% 
  mutate(DateSampled = as.Date(DateSampled),
         FinalResult = as.numeric(FinalResult))

clean_data2 <- cleaned_agfc_lakes %>%
  filter(SamplingPoint == "LOUA015A") %>% 
  filter(WebParameter == "Depth, Secchi disk depth (m)") %>% 
  mutate(DateSampled = as.Date(DateSampled),
         FinalResult = as.numeric(FinalResult))
ggplot(clean_data, aes(x = DateSampled, y = FinalResult)) +  # Replace with actual columns
  geom_point() +
  ggtitle(clean_data$SamplingPoint[1])

dbDisconnect(con)

clean_data

