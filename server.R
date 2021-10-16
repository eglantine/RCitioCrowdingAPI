# devtools::install_github("eglantine/RCitioPackage",ref ="main")
library(shiny)
library(httr)
library(ggplot2)
library(dplyr)
library(xml2)
library(jsonlite)
library(RCitioPackage)
library(data.table)

function(input, output, session) {
  
  session_id = eventReactive(input$doLogin,{
    session_id = getSessionId(input$login, input$password, input$group, input$env)
    
    if(!is.null(session_id)){
      print(paste0("Login successful with session_id ",session_id))
      
      return(session_id)
    }
    
  })
  # observe({
  #   
  #   if(!is.null(session_id())){
  #     api_base_url = buildBaseUrl(input$group, input$env)
  #   }
  # })
  
  raw_occupancy_data = reactive({
    if(!is.null(session_id())){
      api_base_url = buildBaseUrl(input$group, input$env)
      
      print ("Querying API")
      
      raw_occupancy_data = getPredictedOccupancyData(api_base_url,
                                                     session_id=session_id(),
                                                     input$service_date)
      
      print ("Data retrieved")
      
      return(raw_occupancy_data)
    }
  })
  
  referential = reactive({
    
    if(!is.null(session_id())){
      api_base_url = buildBaseUrl(input$group, input$env)
      
      stops = getReferentialSection(api_base_url,session_id(),"stops")
      stops = data.frame(stops$id, stops$station_id)
      
      stations = getReferentialSection(api_base_url,session_id(),"stations")
      stations = data.frame(stations$id, stations$name)
      
      gtfs_stops = read.csv("ctfs/python_agencies_staging_orleansmetropole_referential_gtfs_stops.csv")
      gtfs_stops = data.frame(gtfs_stops$gtfs_id, gtfs_stops$stop_id)
      
      referential = merge(stops,
                          gtfs_stops,
                          by.x = "stops.id",
                          by.y = "gtfs_stops.stop_id")
      
      referential = merge(referential,
                          stations,
                          by.x = "stops.id",
                          by.y = "stations.id")
      
      names(referential) = c("stop_id", "station_id", "gtfs_stop_id", "station_name")
      
      return(referential)
    }
    
  })
  
  clean_predicted_occupancy = reactive({
    
    if(!is.null(session_id())){
    
    clean_predicted_occupancy = merge(raw_occupancy_data(),
                                      referential(),
                                      by.x = "terminus_gtfs_stop_id",
                                      by.y = "gtfs_stop_id")
    
    hours = gsub("(15|30|45):00","00:00",clean_predicted_occupancy$time)
    line_and_direction = paste(clean_predicted_occupancy$line_short_name,clean_predicted_occupancy$station_name,sep = "_")
    clean_predicted_occupancy = data.frame(clean_predicted_occupancy,hours, line_and_direction)
    }
  })
  
  output$raw_occupancy_data = renderDataTable({
    raw_occupancy_data()
  })
  
  output$downloadRawData <- downloadHandler(
    filename = function() {
      paste0(input$group," - ",input$kpi," - ", input$service_date, ".csv")
    },
    content = function(file) {
      write.csv2(clean_predicted_occupancy(), file, row.names = FALSE)
    }
  )
  
  output$heatmap<-renderPlot({
    graph_data = clean_predicted_occupancy() %>%
      group_by(.data[[input$aggregation_x]], .data[[input$aggregation_y]]) %>%
      summarise(occupancy = sum(occupancy),
                occupancy_rate = sum(occupancy)/sum(capacity))
    
    ggplot(graph_data, aes(.data[[input$aggregation_x]], .data[[input$aggregation_y]])) +
      geom_point(aes(size = occupancy, colour = occupancy_rate)) +
      scale_color_gradientn(colors = c("#80C480", "#FFC165", "#E63323")) +
      theme(axis.text.x=element_text(angle=45, hjust=1),
            panel.background = element_rect(fill = "#EFEFF7", colour = "white"))
  })
  
}