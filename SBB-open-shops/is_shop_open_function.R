##install.packages("dplyr")
##install.packages("tidyverse")
##install.packages("jsonlite")
##install.packages("lubridate")
library(dplyr)
library(tidyverse)
library(jsonlite)
library(lubridate)

is_open_at_specific_hours <- function(oh_json_str, selected_day, selected_time) {
  if (nchar(oh_json_str) == 0) {
    return(FALSE)
  }
  
  oh_json <- jsonlite::fromJSON(oh_json_str)
  
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

# Iterate through each row of the sbbshops dataset
for (i in 1:nrow(sbbshops)) {
  shop_open_hours <- sbbshops$openhours[i]
  
  # Skip shops with missing or incorrect opening hours data
  if (!is.null(shop_open_hours)) {
    tryCatch({
      shop_open_hours <- fromJSON(shop_open_hours)
      
      # Test the shop's opening hours
      is_open_now <- is_open_at_specific_hours(shop_open_hours, selected_day, selected_time)
      
      # Print the result for each shop
      cat("Shop:", sbbshops$Name[i], "is open:", is_open_now, "\n")
    }, error = function(e) {
      cat("Shop:", sbbshops$Name[i], "has incorrect opening hours information\n")
    })
  } else {
    cat("Shop:", sbbshops$Name[i], "has no opening hours information\n")
  }
}

# Function to check if a shop is open at a specific day and time

is_shop_open <- function(shop, selected_day, selected_time) {
  if (!is.null(shop$openhours)) {
    oh_list <- tryCatch(jsonlite::fromJSON(shop$openhours, simplifyVector = FALSE),
                        error = function(e) NULL)
    
    if (!is.null(oh_list)) {
      return(is_open_at_specific_hours(oh_list, selected_day, selected_time))
    }
  }
  
  return(FALSE)  # Assume closed if no opening hours information or parsing error
}
# Example of how to apply the function to the sbbshops dataset
selected_day <- 7  # Example day (Sunday)
selected_time <- "09:00:00"  # Example time (09:00 AM)

# Apply the function to each row of the sbbshops dataset
sbbshops$is_open <- mapply(
  is_shop_open, 
  sbbshops, 
  MoreArgs = list(selected_day, selected_time)
)

# Print the resulting dataset
print(sbbshops)

current_datetime <- as.POSIXct("2023-07-20 12:00:00")
sbbshops$is_open <- sapply(sbbshops$openhours, is_open_at_specific_hours, current_datetime)

print(sbbshops)




# Function to check if a shop is open based on separate columns
is_shop_open_from_columns <- function(day_from, day_to, time_from, time_to, selected_day, selected_time) {
  # Convert semicolon-separated values to character vectors
  day_from_vec <- unlist(strsplit(day_from, ";"))
  day_to_vec <- unlist(strsplit(day_to, ";"))
  time_from_vec <- unlist(strsplit(time_from, ";"))
  time_to_vec <- unlist(strsplit(time_to, ";"))
  
  # Convert selected_time to POSIXlt
  selected_time <- as.POSIXlt(selected_time, format = "%T")
  
  # Check if the selected_day matches any of the specified days
  if (selected_day %in% day_from_vec && selected_day %in% day_to_vec) {
    # Find the corresponding index for the selected_day
    idx <- which(selected_day == day_from_vec)
    
    # Check for valid time values
    if (length(time_from_vec) >= idx && length(time_to_vec) >= idx) {
      # Convert the day values to integers for comparison
      day_from_int <- as.integer(day_from_vec)
      day_to_int <- as.integer(day_to_vec)
      
      # Iterate through each time range
      for (i in idx) {
        # Convert time_from and time_to to POSIXlt
        time_from_posix <- as.POSIXlt(time_from_vec[i], format = "%T", tz = "UTC")
        time_to_posix <- as.POSIXlt(time_to_vec[i], format = "%T", tz = "UTC")
        
        # Check if selected_time falls within the specified time range
        if (!is.na(time_from_posix) && !is.na(time_to_posix) &&
            selected_time >= time_from_posix && selected_time <= time_to_posix) {
          return(TRUE)
        }
      }
    }
  }
  
  return(FALSE)
}

# Apply the function to each row of the sbbshops dataset
sbbshops$is_open <- mapply(
  is_shop_open_from_columns, 
  sbbshops$day_from, 
  sbbshops$day_to,
  sbbshops$Zeit.ab,
  sbbshops$Zeit.zu,
  MoreArgs = list(selected_day, selected_time)
)

print(sbbshops)




