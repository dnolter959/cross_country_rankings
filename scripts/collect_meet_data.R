library(tidyverse)
library(rvest)
library(lubridate)
library(RMySQL)
library(DBI)
library(RMariaDB)

source("scripts/functions/functions_for_load_meet_data.R")
source("scripts/functions/functions_for_clean_meet_data.R")

# Connect to DB
mydb <- dbConnect(MySQL(), user='root', password="RunningisfuN'1", dbname='CROSS_COUNTRY', host='localhost')

# Declare meet_ids
meet_ids = c("15859")

# Save meet data to db
for (i in meet_ids) {
  load_clean_and_upload_data(i)
}

# load_clean_and_upload_data
load_clean_and_upload_data <- function(meet_id) {
  tffrs_meet_wbpg <- read_html(str_c("https://www.tfrrs.org/results/xc/", meet_id))
  tables          <- tffrs_meet_wbpg %>% html_table()
  table_titles    <- get_table_titles(tffrs_meet_wbpg)
  meet_date       <- get_meet_date(tffrs_meet_wbpg)
  runner_names    <- get_runner_names(tffrs_meet_wbpg) 
  runner_ids      <- get_runner_ids(tffrs_meet_wbpg)
  
  for (j in seq_along(1:length(tables))) {
    table       <- tables[[j]]
    table_title <- table_titles[[j]]
    
    # Skip compiled team results (we only care about individual results)
    if (compiled_team_results(table_title)) { next }
    
    # clean and upload data
    sql_table <- clean_and_compile_data(meet_id, table, table_title, meet_date, runner_names, runner_ids)
    dbWriteTable(mydb, "RESULTS", sql_table, row.names=FALSE, append=TRUE)
  }
}
