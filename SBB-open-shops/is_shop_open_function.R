##install.packages("dplyr")
##install.packages("tidyverse")
##install.packages("jsonlite")
##install.packages("lubridate")
library(dplyr)
library(tidyverse)
library(jsonlite)
library(lubridate)

is_open_at_specific_hours <- function(oh_json, selected_day, selected_time) {
  if (length(oh_json) == 0) {
    return(FALSE)
  }
  
  selected_time <- as.POSIXlt(selected_time, format = "%T")
  selected_day <- as.integer(selected_day) %% 7 + 1
  
  for (oh in oh_json) {
    day_from <- oh$day_from
    day_to <- oh$day_to
    time_from <- as.POSIXlt(oh$time_from, format = "%T")
    time_to <- as.POSIXlt(oh$time_to, format = "%T")
    
    if ((is.na(day_to) || day_to == "null" || selected_day >= day_from) && 
        (is.na(day_to) || is.null(day_to) || (selected_day <= day_to))) {
      
      if (selected_time >= time_from && selected_time <= time_to) {
        return(TRUE)
      }
    }
  }
  
  return(FALSE)
}

test_case_1 <- list(
  list(day_from = 1, day_to = 7, time_from = "08:00:00", time_to = "18:00:00")
)
selected_day <- 3
selected_time <- "19:00:00"

result <- is_open_at_specific_hours(test_case_1, selected_day, selected_time)
print(result)

test_case_2 <- list(
  list(day_from = 1, day_to = 5, time_from = "08:00:00", time_to = "18:00:00"),
  list(day_from = 6, day_to = 7, time_from = "10:00:00", time_to = "16:00:00")
)

selected_day <- 7
selected_time <- "09:00:00"

result <- is_open_at_specific_hours(test_case_2, selected_day, selected_time)
print(result)
