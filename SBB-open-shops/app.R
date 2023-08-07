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

library(shiny)
library(shinyDatetimePickers)
library(DT)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("SBB Open Shops"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="station_selector",
                        label="Train Station",
                        choices=sort(unique(sbbshops$Haltestellen.Name))
                        ),
            datetimeMaterialPickerInput(inputId="Time",label="Choose time", value = NULL, style = NULL)
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
          h3(textOutput("table_title")),
          leafletOutput("map"),
          dataTableOutput("shops_output")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$table_title <- renderText({
    station <- input$station_selector
    if (!is.null(station)) {
      paste("Shops at", station, "station")
    }
  })
  
  output$map <- renderLeaflet({
    station <- input$station_selector
    
    if (!is.null(station)) {
      filtered_data <- sbbshops[sbbshops$Haltestellen.Name == station, ]
      
      map <- leaflet() %>%
        addTiles()  # Add default map tiles
      
      for (i in 1:nrow(filtered_data)) {
        lat <- as.numeric(unlist(strsplit(filtered_data$Geoposition[i], ", "))[1])
        lon <- as.numeric(unlist(strsplit(filtered_data$Geoposition[i], ", "))[2])
        name <- filtered_data$Name[i]
        
        map <- addMarkers(map, lat = lat, lng = lon, popup = name)
      }
      
      map
    }
  })
  
  
  output$shops_output <- renderDataTable({
    station <- input$station_selector
    if (!is.null(station)) {
      filtered_data <- sbbshops[sbbshops$Haltestellen.Name == station, ]
      shops_list <- filtered_data$Name
      logos_list<-filtered_data$logourl2
      loc_list<-filtered_data$name_affix
      if (length(shops_list) > 0) {
        shops_df <- data.frame(Shop_Logos =logos_list,
                               Shop_Names = shops_list,
                               Shop_Names=loc_list)
        
        
        datatable(
          shops_df,
          escape=FALSE,
          colnames = c("","Shops","Location"),
          options = list(
            pageLength = 25,
            lengthChange = FALSE,
            lengthMenu = list(c(-1), c("All")),
            pagingType = "numbers",
            columnDefs = list(
              list(
                targets = c(0,2),  # Index of the column (0-based) you want to disable sorting for
                orderable = FALSE
              )
            ),
            order = list(list(1, 'asc')) 
          ),
          rownames = FALSE
        )
      }
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
