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

  for (i in 1:length(openhours_data)) {
    day_from <- openhours_data[[i]]$day_from
    day_to <- openhours_data[[i]]$day_to
    time_from <- openhours_data[[i]]$time_from
    time_to <- openhours_data[[i]]$time_to
    
    if ((is.na(day_to) || current_day >= day_from) && 
        (is.na(day_to) || current_day <= day_to)) {
      
      if (current_time >= time_from && current_time <= time_to) {
        return(TRUE)
      }
    }
  }
  
  return(FALSE)
}

format_opening_hours <- function(hours_list) {
  if (is.null(hours_list)) {
    return("No opening hours published")
  } else {
    days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    openhours_table <- rep("Closed", 7)  # Initialize with Closed for all days
    
    for (entry in hours_list) {
      print(entry)  # Print the entry to debug
      if (is.list(entry) && !is.null(entry$day_from) && !is.na(entry$day_from) &&
          !is.null(entry$time_from) && !is.null(entry$time_to)) {
        
        if (is.na(entry$day_to)) {
          entry$day_to <- entry$day_from
        }
        
        day_from_index <- as.integer(entry$day_from) + 1
        day_to_index <- as.integer(entry$day_to) + 1
        
        print(day_from_index)  # Print the day_from_index to debug
        print(day_to_index)    # Print the day_to_index to debug
        
        for (day in day_from_index:day_to_index) {
          openhours_table[day] <- paste0(days[day], ": ", entry$time_from, " - ", entry$time_to)
        }
      }
    }
    
    return(openhours_table)
  }
}









# Example usage
selected_shop <- sbbshops[41, ]  
current_day <- as.integer(format(Sys.time(), "%w")) + 1  # 1: Sunday, 7: Saturday
current_time <- format(Sys.time(), "%H:%M:%S")

shop_status <- is_shop_open(selected_shop$openhours_list_1, current_day, current_time)
cat(shop_status, "\n")

