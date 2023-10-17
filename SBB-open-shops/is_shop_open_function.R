##install.packages("tidyverse")
##install.packages("jsonlite")
##install.packages("lubridate")
##install.packages("purrr")
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

# Define a function to convert day numbers to day names
day_num_to_name <- function(day_num) {
  day_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  return(day_names[day_num])
}

# Define a function to expand the openhours_list_1 column into openhours_table
expand_openhours <- function(openhours_list) {
  # Create an empty data frame to store the expanded open hours
  openhours_table <- data.frame(day_from = integer(0), day = character(0), time_from = character(0), time_to = character(0), stringsAsFactors = FALSE)
  
  # Check if the openhours_list is NULL
  if (is.null(openhours_list)) {
    # If it is NULL, assume the shop is open 24/7
    for (day_num in 1:7) {
      openhours_table <- rbind(openhours_table, data.frame(day_from = day_num, day = day_num_to_name(day_num), time_from = "00:00:00", time_to = "24:00:00"))
    }
  } else {
    for (row in 1:length(openhours_list)) {
      day_from <- openhours_list[[row]]$valid_from
      day_to <- openhours_list[[row]]$valid_until
      opening_hours <- openhours_list[[row]]$openinghours
      
      for (day_num in 1:7) {
        if (is.null(day_from) || (day_num >= day_from && (is.null(day_to) || day_num <= day_to))) {
          day_name <- day_num_to_name(day_num)
          for (oh in opening_hours) {
            time_from <- oh$time_from
            time_to <- oh$time_to
            openhours_table <- rbind(openhours_table, data.frame(day_from = day_num, day = day_name, time_from = time_from, time_to = time_to))
          }
        }
      }
    }
  }
  
  # Order the resulting data frame by day_from
  openhours_table <- openhours_table[order(openhours_table$day_from), ]
  
  return(openhours_table)
}


# Define a function to format a data frame as a string
format_df_as_string <- function(df) {
  if (is.null(df)) {
    return("")
  } else {
    return(paste(capture.output(print(df, row.names = FALSE)), collapse = "\n"))
  }
}
