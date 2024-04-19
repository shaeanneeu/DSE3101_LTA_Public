#install.packages("shinythemes")
#install.packages("shinyWidgets")
#install.packages("shinyTime")
#install.packages("DT")
#install.packages("shinybusy")

library(tidyverse)
library(shiny)
library(shinythemes)
library(leaflet)
library(htmltools)
library(shinybusy)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  ### Title ###
  titlePanel(h1("Attraction Discovery Dashboard")),
  
  sidebarLayout(
    ### Sidebar ###
    sidebarPanel(
      ## Location
      h5(strong("Select Location on Map")),
      textOutput("test"),
      
      br(),
      
      ## Time Input
      sliderInput("travel_time", "Travel Time (in Minutes)", min = 10, max = 60, value = 10, step = 5),
      
      br(),
      
      ## Attraction Inputs 
      checkboxGroupInput('attraction_type', 'Attraction Types', 
                         c("Museum", "Food", "Shopping", "Heritage", "Nature")),                        
      
      ## Button 
      actionButton("trigger", "Submit")
    ),
    
    ### Main Panel ####
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leaflet::leafletOutput('map', width = 950, height = 700)),
        tabPanel("Recommendations",  DT::DTOutput("tablerecs"))
      )
    )
  ),
  ### CSS Formatting ###
  tags$head(
    tags$style(HTML(".shiny-notification {
                position: fixed;
                bottom: 20px; 
                left: 20px; 
            }"))
  )
)

server <- function(input, output, session){
  
  ## Handling Inputs 
  selected_location <- reactiveVal(FALSE)
  selected_attraction <- reactiveVal(character(0))
  observe({
    selected_types <- input$attraction_type
    selected_attraction(selected_types)  
  })
  
  ## Marker Icons 
  icon_list <- iconList(
    Input = makeIcon("marker_input.png", iconWidth = 40, iconHeight = 55),
    Museum = makeIcon("marker_museum.png", iconWidth = 40, iconHeight = 55),
    Food = makeIcon("marker_food.png", iconWidth = 40, iconHeight = 55),
    Shopping = makeIcon("marker_shopping.png", iconWidth = 40, iconHeight = 55),
    Heritage = makeIcon("marker_heritage.png", iconWidth = 40, iconHeight = 55),
    Nature = makeIcon("marker_nature.png", iconWidth = 40, iconHeight = 55)
    )
  
  ## Initial Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      addProviderTiles(providers$OneMapSG.Default) %>%
      setView(103.81, 1.35, zoom = 13) %>%
      addLegend("bottomleft", 
                colors = c("firebrick","orchid", "orange", "cornflowerblue", "hotpink", "forestgreen"),
                labels = c("Your location", "Museum", "Food", "Shopping", "Heritage", "Nature"),
                title = "Legend",
                opacity = 1)
  })
  
  ## Map Clicking
  observeEvent(input$map_click, {
    click <- input$map_click
    lat <- click$lat
    lng <- click$lng
    
    proxy <- leafletProxy("map")
    proxy %>% clearMarkers() %>%
      addMarkers(group = "input", lng, lat, icon = icon_list['Input']) %>%
      setView(lng, lat, zoom = 16)
    
    output$test <- renderText({ 
      paste0("You have selected ", "(", round(lat,2), ", ", round(lng,2), ")")
    })
    selected_location(TRUE)
  })
  

  ## Retrieving Outputs 
  observeEvent(input$trigger, {
    show_modal_spinner(spin = "spring",
                       color = "grey",
                       text = "Loading...")
    ## Error Message
    if(!selected_location()) {
      showNotification("Please select location on map!", duration = 5, type = "error", closeButton = TRUE)
      remove_modal_spinner()
      return()
    }
    
    if(length(selected_attraction()) == 0) {
      showNotification("Please select attraction types!", duration = 5, type = "error", closeButton = TRUE)
      remove_modal_spinner()
      return()
    }
    
    ## Obtaining Recommendations from Backend 
    attraction_type_vector = as.vector(strsplit(selected_attraction(), split = " "))
    
    source('backend_code.R')
    
    df = wanted_attractions(input$map_click$lat, input$map_click$lng, attraction_type_vector,
                            input$travel_time) 
    
    ## Preparing backend output for UI 
    df_real = df %>%
      mutate(Occupancy = case_when((Load == "SEA" & TravelMode == "Bus") ~ "Seats Available" ,
                                   (Load == "SDA" & TravelMode == "Bus") ~ "Standing Available",
                                   (Load == "LSD" & TravelMode == "Bus") ~ "Limited Standing",
                                   TRUE ~ 'NIL')) %>%
      mutate(Directions = case_when(TravelMode == "Bus" ~ paste("Take bus", ServiceNo, "
                                                               from", OriginBusStopName, "to", 
                                                               DestinationBusStopName),
                                    TravelMode == "Walk" ~ "Just walk!")) %>%
      mutate(TravelTime = case_when(TravelMode == "Bus" ~ paste(round(ETA,0) + 10), #Add 10 minutes to account for walking time
                                    TravelMode == "Walk" ~ "Less than 10"))  
    
    ## Display Recommendations as table 
    output$tablerecs = DT::renderDT({
      df_real %>% 
        select(Destination, Type, TravelTime, Occupancy, `Website Link`) %>%
        DT::datatable()
    })

    ## Displaying Recommendations on Map
    proxy <- leafletProxy("map")
    
    proxy %>% clearGroup(group = "recommendations")
    
    popup_content <- paste("Name:", df_real$Destination, "<br/>",
                           "Attraction Type:", df_real$Type, "<br/>",
                           "Time to Destination:", df_real$TravelTime, "minutes", "<br/>",
                           "How to get there: ", df_real$Directions, "<br/>",
                           "Current Bus Occupancy:", df_real$Occupancy, "<br/>", "<br/>",
                           "<div style='text-align:center;'>",
                           "<img src='", df_real$`Image Link`, "' height='150' width='225'>", "</div>")
    
    popup_content <- ifelse(!is.na(df_real$`Website Link`) & df_real$`Website Link` != "", 
                            paste0(popup_content, "<br/>",
                                   "<a href='", df_real$`Website Link`, "' target='_blank'>Link to Website</a>"),
                            popup_content)
    
    remove_modal_spinner()
    
    proxy %>% addMarkers(group = "recommendations",
                         df_real$DestinationLongitude, df_real$DestinationLatitude, 
                         icon = icon_list[df_real$Type],
                         popup = popup_content)
    
    ## Notifications
    if(nrow(df_real) > 0) {
      showNotification("Click on each marker for more info!", duration = 5, type = "message")
    } else {
      showNotification("Please increase travelling time window.", duration = 5, type = "warning")
    }
  })
}

shinyApp(ui = ui, server = server)
