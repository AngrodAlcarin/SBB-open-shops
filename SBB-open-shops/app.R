# 
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
##install.packages("shinyDatetimePickers")
##install.packages("DT")
##install.packages("leaflet")
##install.packages("shinydashboard")
##install.packages("shinyWidgets")

library(shiny)
library(shinyDatetimePickers)
library(shinydashboard)
library(DT)
library(leaflet)
library(stringr)
library(shinyWidgets)

# Define UI for application that draws a histogram


ui <- fluidPage(
  titlePanel("SBB Open Shops"),
  
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
      actionButton("clear_selection_button", "Clear Selection",class = "btn-clear"),
      tags$style(".btn-clear { margin-bottom: 20px; }"),  # Add margin above the button
      datetimeMaterialPickerInput(
        inputId = "Time",
        label = "Choose time",
        value = NULL,
        style = NULL
      )
    ),
    
    mainPanel(
      h3(textOutput("table_title")),
      leafletOutput("map", height = 350),
      div(
        style = "height: 350px; width: 100%; overflow-y: auto;",
        dataTableOutput("shops_output")
      )
      
    )
  )
)



server <- function(input, output, session) {
  
  #the title above the map and table, reactive on which station is selected
  output$table_title <- renderText({
    station <- input$station_selector
    if (!is.null(station)) {
      paste("Shops at", station, "station")
    }
  })
  
  #data table containing all shops at the selected train station

    
  output$subcat_selector <- renderUI({
    if (!is.null(input$cat_selector)) {
      if (input$cat_selector %in% c("Shopping", "Services")){
        filtered_subcategories <- unique(sbbshops$subcategories[sbbshops$category == input$cat_selector])
        if (length(filtered_subcategories) > 0) {
          filtered_subcategories <- unlist(filtered_subcategories)
          selectInput(
            inputId = "subcat_selector",
            label = "Subcategory",
            choices = sort(filtered_subcategories),
            selected=NULL
          )
      } else {
        # Return NULL or an empty element if there are no subcategories
        NULL
      }
     }
    }
  })
        
           
    #reactive expression to highlight selected shop(s) on the map    
    selected_shop <- reactiveVal(NULL)
    
    #leaflet map with markers on all the shops at the selected train station.
    output$map <- renderLeaflet({
      station <- input$station_selector
      
      #observer for debugging selecting rows  
      observeEvent(input$shops_output_rows_selected, {
        selected_row <- input$shops_output_rows_selected
        if (length(selected_row) > 0) {
          selected_shop_name <- filtered_data[selected_row, "Name"]
          selected_shop(selected_shop_name)
          
          # Print the selected row index and corresponding shop name
          cat("Selected Row Index:", selected_row, "\n")
          cat("Selected Shop Name:", selected_shop_name, "\n")
        } else {
          selected_shop(NULL)  # Remove the selected shop name
        }
          
          
        })
      
      observeEvent(input$clear_selection_button, {
        updateSelectInput(session, "cat_selector", selected = "")
        updateSelectInput(session, "subcat_selector", selected = "")
      })
    
      
      if (!is.null(station)) {
        filtered_data <- sbbshops[sbbshops$Haltestellen.Name == station, ]
        
        map <- leaflet() %>%
          addTiles()  # Add default map tiles
        #make selected shops red and bigger highlighted
        selected_rows <- input$shops_output_rows_selected
        #unlist the latitude and longitude from the dataset, it was in a single column
        for (i in 1:nrow(filtered_data)) {
          lat <- as.numeric(unlist(strsplit(filtered_data$Geoposition[i], ", "))[1])
          lon <- as.numeric(unlist(strsplit(filtered_data$Geoposition[i], ", "))[2])
          name <- filtered_data$Name[i]
          #if row is selected, show it as red and bigger
          if (i %in% selected_rows) {
            icon_url <- "pin-l-shop+f00.png"
            icon <- makeIcon(iconUrl = icon_url, iconWidth = 45, iconHeight = 112.5)
          } 
          #if not, show it as a normal blue smaller pin
          else {
            icon_url <- "pin-l-shop+12a.png"
            icon <- makeIcon(iconUrl = icon_url, iconWidth = 35, iconHeight = 87.5)
          }
          
          map <- addMarkers(map, lat = lat, lng = lon, popup = name, icon = icon)
        }
        
        map
      }
    })
    output$shops_output <- renderDataTable({
      station <- input$station_selector
      category <- input$cat_selector
      subcategory <- input$subcat_selector
      if (!is.null(station)) {
        filtered_data <- sbbshops[sbbshops$Haltestellen.Name == station, ]
        if (!is.null(category)&&category !=""){
          filtered_data<-filtered_data[filtered_data$category==category,]
          if (!is.null(subcategory)&&subcategory!=""){
            filtered_data<-filtered_data[grep(subcategory, filtered_data$subcategories),]
          }
        }
        shops_list <- filtered_data$Name
        logos_list<-filtered_data$logourl2
        loc_list<-filtered_data$name_affix
        cat_list<-filtered_data$category
        if (length(shops_list) > 0) {
          shops_df <- data.frame(Shop_Logos =logos_list,
                                 Shop_Names = shops_list,
                                 Shop_Names=loc_list,
                                 Shop_Names=cat_list)    
        #the table itself
        datatable(
          shops_df,
          escape=FALSE, #allowing html for the images
          colnames = c("","Shop","Location", "Category"),
          options = list(
            pageLength = 10,
            lengthChange = FALSE,
            lengthMenu = list(c(-1), c("All")),
            pagingType = "numbers",
            columnDefs = list(
              list(
                targets = c(0,2),  # disable sorting for location and logo
                orderable = FALSE
              )
            ),
            order = list(list(1, 'asc')) #list alphabetically by the shop name
          ),
          rownames = FALSE#remove the numbering of the shops
        )
        }
        else {
          datatable(data.frame(x="No Shops matching the selection found.",y=""),
                    rownames = FALSE,
                    colnames="",
                    options=list(
                      paging=FALSE,
                      searching=F,
                      info=F,
                      ordering=F
                    ))
        }
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
