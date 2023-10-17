##install.packages("shinyDatetimePickers")
##install.packages("DT")
##install.packages("leaflet")
##install.packages("shinydashboard")
##install.packages("shinyWidgets")
##install.packages("bs4Dash")

library(tidyverse)
library(jsonlite)
library(lubridate)
library(shiny)
library(shinyDatetimePickers)
library(shinydashboard)
library(DT)
library(leaflet)
library(stringr)
library(shinyWidgets)
library(bs4Dash)
library(purrr)
library(readr)

##define functions
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
    # If it is not NULL, expand the open hours according to the rules you provided
    for (row in 1:nrow(openhours_list)) {
      day_from <- openhours_list[row, "day_from"]
      day_to <- ifelse(is.na(openhours_list[row, "day_to"]), day_from, openhours_list[row, "day_to"])
      time_from <- openhours_list[row, "time_from"]
      time_to <- openhours_list[row, "time_to"]
      
      for (day_num in day_from:day_to) {
        # Check if there are already rows for this day
        existing_rows <- which(openhours_table$day_from == day_num)
        if (length(existing_rows) > 0) {
          # If there are existing rows, remove the second time it's called
          if (openhours_table[existing_rows[1], "time_to"] < time_from) {
            openhours_table[existing_rows, "day"] <- paste("")
            day_name <- paste("")
          } else {
            openhours_table[existing_rows, "day"] <- paste("")
            day_name <- paste("")
          }
        } else {
          # If there are no existing rows, use the regular day name
          day_name <- day_num_to_name(day_num)
        }
        
        # Add a new row to the openhours_table
        openhours_table <- rbind(openhours_table, data.frame(day_from = day_num, day = day_name, time_from = time_from, time_to = time_to))
      }
    }
    
    # Add rows with NA for days that are not mentioned (i.e. the shop is closed)
    for (day_num in 1:7) {
      if (!day_num %in% openhours_table$day_from) {
        openhours_table <- rbind(openhours_table, data.frame(day_from = day_num, day = day_num_to_name(day_num), time_from = NA, time_to = NA))
      }
    }
    
    # Order the resulting data frame by day_from
    openhours_table <- openhours_table[order(openhours_table$day_from), ]
  }
  
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

##data import
sbbopenshops<-read_delim("offnungszeiten-shops.csv",delim = ";", escape_double = TRUE,
                         trim_ws = FALSE)

sbbshops<-sbbopenshops %>% 
  select(c(1,5,7:13,17,34:37)) %>% 
  ##filter(!category %in% c("Öffentlicher Verkehr","Piktogramm (Übrige)", "Piktogramm SBB Schalter")) %>% 
  filter(!category %in% c("Öffentlicher Verkehr","Piktogramm (Übrige)")) %>% 
  mutate(logourl=paste("https://stations.sbb.cartaro-enterprise.com", logo,sep = ""),
         logourl2=paste("<img src=", "\"", logourl, "\""," width=\"80\"></img>",sep=""),
         category=case_when(category=="Piktogramm SBB Schalter" ~ "SBB Schalter",
                            category=="Services P"~"Services", 
                            category=="Services IM"~"Services",
                            category=="Services-Übrige"~"Services",
                            category=="Kombinierte Mobilität"~"Mobilität", TRUE~category),
         subcategories=case_when(subcategories=="Dienstleistungen SBB Services"~"SBB Services",
                                 subcategories=="SBB Services"~"SBB Services",
                                 subcategories=="SBB"~"SBB Services",TRUE~subcategories),
         subcategories = strsplit(subcategories, "[\n /]+"))%>%
  mutate(openhours_list = strsplit(as.character(openhours), "\\]\\s*\\[", perl = TRUE)) %>%
  unnest(openhours_list) %>%
  mutate(openhours_list = paste0("[", openhours_list, "]")) %>%
  mutate(openhours_list = map(openhours_list, ~ suppressWarnings(fromJSON(.x))))%>%
  unnest_wider(openhours_list, names_sep = "_")

sbbshops<-sbbshops %>% 
  mutate(openhours_table = lapply(sbbshops$openhours_list_1, expand_openhours))
sbbshops<-sbbshops %>% 
  mutate(openhours_char= lapply(sbbshops$openhours_table, function(openhours) {
    if (!is.null(openhours) && is.data.frame(openhours)) {
      # Extract and format the open hours data
      openhours_text <- paste(openhours$day, openhours$time_from, openhours$time_to, sep = " - ")
      paste(openhours_text, collapse = "<br>")
    } else {
      ""
    }
  }),
  openhours_char=as.character(openhours_char))


# Define UI
ui <- fluidPage(tags$head(
  tags$style(HTML("
      body {
        background-color:  #2d327d; 
      }
    "))
),
  titlePanel( div(style = "text-align: left;",
                  img(src = 'SBB_NEG_2F_SCOTCH_100.png', height=55),
                  img(src = '3044_Shopping.svg',width=95)
  )),
                sidebarLayout(
                  sidebarPanel(
                    style = "background-color: #C60018;color:#F6F6F6",
                    selectInput(
                      inputId = "station_selector",
                      label = "Train Station",
                      choices = sort(unique(sbbshops$Haltestellen.Name)),
                    ),
                    selectInput(
                      inputId = "cat_selector",
                      label = "Category",
                      choices = c("", sort(unique(sbbshops$category))),
                      selected = ""  # Set default selection to NULL
                    ),
                    uiOutput("subcat_selector"),
                    actionButton("clear_selection_button", "Clear Selection", class = "btn-clear"),
                    tags$style(".btn-clear { margin-bottom: 20px; }"),
                    # Add margin above the button
                    airDatepickerInput(
                      inputId = "Time",
                      label="Date&Time",
                      value = NULL,
                      timepicker=TRUE
                    ),
                    actionButton("set_time_now", "Set to now", width = 89.64),
                    actionButton("reset_time", "Reset",width = 60),
                    width = 3
                  ),
                  mainPanel(
                    style = "background-color: #E5E5E5;",
                    h3(textOutput("table_title")),
                    leafletOutput("map", height = 350),
                    div(style = "height: 350px; width:100%; overflow-y: auto;",
                        dataTableOutput("shops_output")),
                    width = 9
                  )
                ))

#server function
server <- function(input, output, session) {
 
  #the title above the map and table, reactive on which station is selected
  output$table_title <- renderText({
    station <- input$station_selector
    if (!is.null(station)) {
      paste("Shops at", station, "station")
    }
  })
  
  ##reactive subcategory selector to only show subcategory selector when categories shopping/services are selected.
  output$subcat_selector <- renderUI({
    if (!is.null(input$cat_selector)) {
      if (input$cat_selector %in% c("Shopping", "Services")) {
        filtered_subcategories <-
          unique(sbbshops$subcategories[sbbshops$category == input$cat_selector])
        if (length(filtered_subcategories) > 0) {
          filtered_subcategories <- unlist(filtered_subcategories)
          selectInput(
            inputId = "subcat_selector",
            label = "Subcategory",
            choices = c("",sort(filtered_subcategories)),
            selected = ""
          )
        } else {
          # Return NULL or an empty element if there are no subcategories
          NULL
        }
      }
    }
  })
  
  ##the action button to clear selections in the category and subcategory selector
  observeEvent(input$clear_selection_button, {
    updateSelectInput(session, "cat_selector", selected = "")
    updateSelectInput(session, "subcat_selector", selected = "")
    selected_shop(NULL)
  })
  
  ##action button to set time to now
  observeEvent(input$set_time_now, {
    updateAirDateInput(session, "Time",value=Sys.time())
  })
  
  ##action button to reset time
  observeEvent(input$reset_time, {
    updateAirDateInput(session, "Time",clear=TRUE)
  })
  
  #reactive expression to highlight selected shop(s) on the map
  selected_shop <- reactiveVal(NULL)
  
  #leaflet map with markers on all the shops at the selected train station.
  output$map <- renderLeaflet({
    station <- input$station_selector
    category <- input$cat_selector
    subcategory <- input$subcat_selector
    
    #observer for selecting rows
    observeEvent(input$shops_output_rows_selected, {
      selected_row <- input$shops_output_rows_selected
      if (length(selected_row) > 0) {
        selected_shop_name <- filtered_data[selected_row, "Name"]
        selected_shop(selected_shop_name)
      } else {
        selected_shop(NULL)  # Remove the selected shop name
      }
    })
    
    map <- leaflet() %>%
      addTiles()  # Add default map tiles
    
    if (!is.null(station)) {
      filtered_data <- sbbshops[sbbshops$Haltestellen.Name == station,]
      if (!is.null(category) && category != "") {
        filtered_data <- filtered_data[filtered_data$category == category,]
        
        if (!is.null(subcategory) && subcategory != "") {
          filtered_data <-
            filtered_data[grep(subcategory, filtered_data$subcategories),]
        }
      }
      
      # Center the map on the selected station
      if (nrow(filtered_data) > 0) {
        lat <-
          as.numeric(unlist(strsplit(filtered_data$Geoposition[1], ", "))[1])
        lon <-
          as.numeric(unlist(strsplit(filtered_data$Geoposition[1], ", "))[2])
        
        # Add markers to the map
        selected_rows <- input$shops_output_rows_selected
        for (i in 1:nrow(filtered_data)) {
          lat <-
            as.numeric(unlist(strsplit(
              filtered_data$Geoposition[i], ", "
            ))[1])
          lon <-
            as.numeric(unlist(strsplit(
              filtered_data$Geoposition[i], ", "
            ))[2])
          name <- filtered_data$Name[i]
          
          #if row/shop is selected, show it as red and bigger
          if (i %in% selected_rows) {
            icon_url <- "pin-l-shop+f00.png"
            icon <-
              makeIcon(
                iconUrl = icon_url,
                iconWidth = 45,
                iconHeight = 112.5
              )
          }
          #if not, show it/them as a normal blue smaller pin
          else {
            icon_url <- "pin-l-shop+12a.png"
            icon <-
              makeIcon(
                iconUrl = icon_url,
                iconWidth = 35,
                iconHeight = 87.5
              )
          }
          map <-
            addMarkers(
              map,
              lat = lat,
              lng = lon,
              popup = name,
              icon = icon
            )
        }
      } else {
        # If no matching shops, center the map on the location of the selected station
        station_data <-
          sbbshops[sbbshops$Haltestellen.Name == station,]
        lat <-
          as.numeric(unlist(strsplit(station_data$Geoposition[1], ", "))[1])
        lon <-
          as.numeric(unlist(strsplit(station_data$Geoposition[1], ", "))[2])
        map <- setView(lng = lon,
                       lat = lat,
                       zoom = 17,
                       map)
      }
    }
    map
  })
  
  #data table containing all shops at the selected train station
  output$shops_output <- renderDataTable({
    station <- input$station_selector
    category <- input$cat_selector
    subcategory <- input$subcat_selector
    if (!is.null(station)) {
      filtered_data <- sbbshops[sbbshops$Haltestellen.Name == station,]
      if (!is.null(category) && category != "") {
        filtered_data <- filtered_data[filtered_data$category == category, ]
        if (!is.null(subcategory) && subcategory != "") {
          filtered_data <-
            filtered_data[grep(subcategory, filtered_data$subcategories), ]
        }
      }
      shops_list <- filtered_data$Name
      logos_list <- filtered_data$logourl2
      loc_list <- filtered_data$name_affix
      cat_list <- filtered_data$category
      hours_list <- filtered_data$openhours_char
      #do the table if the list  is bigger than 1
      if (length(shops_list) > 0) {
        shops_df <- data.frame(
          Shop_Logos = logos_list,
          Shop_Names = shops_list,
          Shop_Locs = loc_list,
          Shop_Cats = cat_list,
          Shop_Hours = hours_list
        )
        #the table itself
        datatable(
          shops_df,
          escape = FALSE,
          #allowing html for the images
          colnames = c("", "Shop", "Location", "Category", "Opening Hours"),
          options = list(
            pageLength = 10,
            lengthChange = FALSE,
            lengthMenu = list(c(-1), c("All")),
            pagingType = "numbers",
            columnDefs = list(list(
              targets = c(0, 2),  # disable sorting for location and logo
              orderable = FALSE
            ),
            list(
              targets = c(1,2,3,4),  # Index of the "Opening Hours" column
              className = "dt-right",  # Center-align content
              render = JS(
                "function(data, type, row) {",
                "  return '<div style=\"font-size: 12.679999828338622158696580299874767px;\">' + data + '</div>';",
                "}"
              )
            )
            ),
            order = list(list(1, 'asc'))  # list alphabetically by the shop name
          ),
          rownames = FALSE
        ) %>% 
          formatStyle(5, width = '200px')#format opening hours row to be big enough to display one days opening hours
                                         #in one line
      }
      else {
        datatable(
          data.frame(x = "No Shops matching the selection found.", y = ""),
          rownames = FALSE,
          colnames = "",
          options = list(
            paging = FALSE,
            searching = F,
            info = F,
            ordering = F
          )
        )##shows an empty navigationless table saying"No Shops matching the selection found." if there
        ##are no shops matching the selection.
      }
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
