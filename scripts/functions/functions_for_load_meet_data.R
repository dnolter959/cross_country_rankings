# get_table_titles
get_table_titles <- function(tffrs_meet_wbpg) {
  tffrs_meet_wbpg %>% 
    html_elements(".font-weight-500") %>% 
    html_text() %>% 
    map_chr(~ substring(.x, 1, str_locate(.x, "\\(*\\)")[[1]]))  
}

# get_meet_date
get_meet_date <- function(tffrs_meet_wbpg) {
  tffrs_meet_wbpg %>% 
    html_elements(".panel-heading-normal-text:nth-child(1)") %>% 
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    mdy()
}

# get_runner_names
get_runner_names <- function(tffrs_meet_wbpg) {
  tffrs_meet_wbpg %>% 
    html_elements("td:nth-child(2) div a") %>% 
    html_text() %>%
    str_trim()
}

# get_runner_ids
get_runner_ids <- function(tffrs_meet_wbpg) {
  tffrs_meet_wbpg %>% 
    html_elements("td:nth-child(2) div a") %>% 
    html_attr("href")
}