##install.packages("shinyDatetimePickers")
##install.packages("DT")
##install.packages("leaflet")
##install.packages("shinydashboard")
##install.packages("shinyWidgets")
##install.packages("bs4Dash")

library(shiny)
library(shinyDatetimePickers)
library(shinydashboard)
library(DT)
library(leaflet)
library(stringr)
library(shinyWidgets)
library(bs4Dash)
library(purrr)

# Define UI
ui <- fluidPage(titlePanel("SBB Open Shops"),
                
                sidebarLayout(
                  sidebarPanel(
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
                    actionButton("set_time_now", "Set to now"),
                    actionButton("reset_time", "Reset")
                  ),
                  mainPanel(
                    h3(textOutput("table_title")),
                    leafletOutput("map", height = 350),
                    div(style = "height: 350px; width: 100%; overflow-y: auto;",
                        dataTableOutput("shops_output"))
                  )
                ))

#server function
server <- function(input, output, session) {
  
  #the functions needed for datetime
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
  
  format_opening_hours <- function(openhours) {
    if (is.null(openhours)) {
      return("No opening hours published")
    } else {
      opening_hours_per_day <- lapply(openhours, function(hours) {
        if (!is.null(hours) && !is.null(hours$day_from) && !is.null(hours$time_from) && !is.null(hours$time_to)) {
          if (!is.null(hours$day_to)) {
            paste0("Day ", hours$day_from, " to ", hours$day_to, ": ",
                   hours$time_from, " - ", hours$time_to)
          } else {
            paste0("Day ", hours$day_from, ": ",
                   hours$time_from, " - ", hours$time_to)
          }
        } else {
          "No opening hours available for this day"
        }
      })
      return(paste(unlist(opening_hours_per_day), collapse = "<br>"))
    }
  }
  
  
  
  
  
  #the title above the map and table, reactive on which station is selected
  output$table_title <- renderText({
    station <- input$station_selector
    if (!is.null(station)) {
      paste("Shops at", station, "station")
    }
  })
  
  #observer for datetimevalue
  observeEvent(input$Time, {
    datetime_value<-input$Time
    print(datetime_value)
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
    time <- input$Time
    filtered_data <- sbbshops
    
    if (!is.null(station)) {
      filtered_data <- filtered_data[filtered_data$Haltestellen.Name == station,]
      
      if (!is.null(category) && category != "") {
        filtered_data <- filtered_data[filtered_data$category == category, ]
        
        if (!is.null(subcategory) && subcategory != "") {
          filtered_data <- filtered_data[grep(subcategory, filtered_data$subcategories), ]
        }
      }
      
      shops_list <- filtered_data$Name
      logos_list <- filtered_data$logourl2
      loc_list <- filtered_data$name_affix
      cat_list <- filtered_data$category
      
      # Filter shops based on whether they have opening hours
      has_hours <- sapply(filtered_data$openhours_list_1, shop_has_hours)
      filtered_data <- filtered_data[has_hours, ]
      
      #do the table if the list  is bigger than 1
      if (nrow(filtered_data) > 0) {  # Check if there are any rows
        opening_hours_list <- purrr::map(filtered_data$openhours_list_1, format_opening_hours)
        
        shops_df <- data.frame(
          Shop_Logos = logos_list,
          Shop_Names = shops_list,
          Location = loc_list,
          Category = cat_list,
          Opening_Hours = opening_hours_list
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
            )),
            order = list(list(1, 'asc')) #list alphabetically by the shop name
          ),
          rownames = FALSE #remove the numbering of the shops
        )
      } else {
        datatable(
          data.frame(x = "No Shops matching the selection found.", y = ""),
          rownames = FALSE,
          colnames = "",
          options = list(
            paging = FALSE,
            searching = FALSE,
            info = FALSE,
            ordering = FALSE
          )
        )  # shows an empty navigationless table saying "No Shops matching the selection found."
      }
    }
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
