# clean_and_compile_date
clean_and_compile_data <- function(meet_id, table, table_title, meet_date, runner_names, runner_ids) {
  table %>% 
    mutate(
      meet_id         = strtoi(meet_id),
      date            = meet_date,
      gender          = find_gender(table_title),
      place           = PL,
      runner_id       = find_runner_ids(NAME, runner_names, runner_ids),
      school_year     = YEAR,
      team            = TEAM,
      distance_meters = find_distance(table_title),
      time_string     = TIME,
      time_seconds    = times_seconds(TIME)
    ) %>% 
    select(meet_id, date, gender, place, runner_id, school_year, 
           team, distance_meters, time_string, time_seconds)  
}

# compiled_team_results?
compiled_team_results <- function(table_title) {
  grepl("team", tolower(table_title), fixed=TRUE)
}

# find_runner_ids
find_runner_ids <- function(names, runner_names, runner_ids) {
  names %>% map_int(~ find_runner_id(.x, runner_names, runner_ids))
}

find_runner_id <- function(name, runner_names, runner_ids) {
  runner_wbpg <- runner_ids[match(name, runner_names)]
  snip_start <- str_locate(runner_wbpg, "athletes/")[2] + 1
  snip_end <- str_locate(runner_wbpg, ".html")[1] - 1
  runner_id <- substring(runner_wbpg, snip_start, snip_end)
  strtoi(runner_id)
}

# find_gender
find_gender <- function(table_title) {
  if (grepl("women", tolower(table_title), fixed=TRUE)) 
    return('W') 
  else 
    return('M')
}

# find_distance
find_distance <- function(table_title) {
  lowcase_title <- tolower(table_title)
  if (grepl("4k", lowcase_title, fixed=TRUE) | grepl("4000", lowcase_title, fixed=TRUE))
    return(4000)
  else if (grepl("5k", lowcase_title, fixed=TRUE) | grepl("5000", lowcase_title, fixed=TRUE)) 
    return(5000)
  else if (grepl("6k", lowcase_title, fixed=TRUE) | grepl("6000", lowcase_title, fixed=TRUE))
    return(6000)
  else if (grepl("8k", lowcase_title, fixed=TRUE) | grepl("8000", lowcase_title, fixed=TRUE))
    return(8000)
  else if (grepl("10k", lowcase_title, fixed=TRUE) | grepl("10000", lowcase_title, fixed=TRUE))
    return(10000)
  else return(NA)
}

# time_seconds
time_seconds <- function(time) {
  if (!grepl(":", time, fixed=TRUE)) return(NA)
  
  split_string <- str_split(time, ":")[[1]]
  
  if (length(split_string) == 3) {
    hours   <- as.double(str_split(time, ":")[[1]][1])
    minutes <- as.double(str_split(time, ":")[[1]][2])
    seconds <- as.double(str_split(time, ":")[[1]][3])
  }
  else if (length(split_string) == 2) {
    hours   <- 0
    minutes <- as.double(str_split(time, ":")[[1]][1])
    seconds <- as.double(str_split(time, ":")[[1]][2])
  }
  else { return(NA) }

  hours * (60*60) + minutes * 60 + seconds
}

times_seconds <- function(times) {
  times %>% map_dbl(~ time_seconds(.x))
}