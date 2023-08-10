##install.packages("dplyr")
##install.packages("tidyverse")
##install.packages("jsonlite")
##install.packages("lubridate")
library(dplyr)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(purrr)


shop_has_hours <- function(openhours_data) {
  if (is.null(openhours_data) || all(sapply(openhours_data, is.null))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


is_shop_open <- function(openhours_data, current_day, current_time) {
  if (!shop_has_hours(openhours_data)) {
    return("No opening hours published")
  }
  
  for (i in 1:length(openhours_data)) {
    day_from <- openhours_data[[i]]$day_from
    day_to <- openhours_data[[i]]$day_to
    time_from <- openhours_data[[i]]$time_from
    time_to <- openhours_data[[i]]$time_to
    
    if ((is.na(day_to) || current_day >= day_from) && 
        (is.na(day_to) || current_day <= day_to)) {
      
      if (current_time >= time_from && current_time <= time_to) {
        return("The shop is currently open")
      }
    }
  }
  
  return("The shop is currently closed")
}



# Example usage
# Example usage
selected_shop <- sbbshops[41, ]  # Replace with the shop you're interested in
current_day <- as.integer(format(Sys.time(), "%w")) + 1  # 1: Sunday, 7: Saturday
current_time <- format(Sys.time(), "%H:%M:%S")

shop_status <- is_shop_open(selected_shop$openhours_list_1, current_day, current_time)
cat(shop_status, "\n")

