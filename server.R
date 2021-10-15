# devtools::install_github("eglantine/RCitioPackage",ref ="main")
library(shiny)
library(httr)
library(ggplot2)
library(dplyr)
library(xml2)
library(jsonlite)
library(RCitioPackage)

function(input, output, session) {
  
  session_id = eventReactive(input$doLogin,{
    session_id = getSessionId(input$login, input$password, input$group, input$env)
    
    if(!is.null(session_id)){
      print(paste0("Login successful with session_id ",session_id))
      
      return(session_id)
    }
    
  })
  observe({
    
    if(!is.null(session_id())){
      api_base_url = buildBaseUrl(input$group, input$env)
    }
  })
  
  clean_predicted_occupancy = reactive({
    if(!is.null(session_id())){
      api_base_url = buildBaseUrl(input$group, input$env)
      
      agency_id = getAgencyId(api_base_url, session_id())
      
      print ("Loading data")
      
      predicted_occupancy_data = getPredictedOccupancyData(api_base_url,
                                                           session_id=session_id(),
                                                           input$service_date)
      
      stops = getReferentialSection(api_base_url,session_id(),"stops")
      stops = data.frame(stops$id, stops$station_id)
      
      stations = getReferentialSection(api_base_url,session_id(),"stations")
      stations = data.frame(stations$id, stations$name)
      
      gtfs_stops = read.csv("ctfs/python_agencies_staging_orleansmetropole_referential_gtfs_stops.csv")
      gtfs_stops = data.frame(gtfs_stops$gtfs_id, gtfs_stops$stop_id)
      
      clean_predicted_occupancy = merge(predicted_occupancy_data,
                                        gtfs_stops,
                                        by.x = "terminus_gtfs_stop_id",
                                        by.y = "gtfs_stops.gtfs_id")
            
      clean_predicted_occupancy = merge(predicted_occupancy_data,
                                        gtfs_stops,
                                        by.x = "gtfs_stop_id",
                                        by.y = "gtfs_stops.gtfs_id")
      
      clean_predicted_occupancy = merge(clean_predicted_occupancy,
                                        stops,
                                        by.x = "gtfs_stops.stop_id",
                                        by.y = "stops.id")
      
      clean_predicted_occupancy = merge(clean_predicted_occupancy,
                                        stations,
                                        by.x = "stops.station_id",
                                        by.y = "stations.id")
      
      hours = gsub("(15|30|45):00","00:00",clean_predicted_occupancy$time)
      line_and_direction = paste(clean_predicted_occupancy$line_short_name,clean_predicted_occupancy$stations.name,sep = "_")
      clean_predicted_occupancy = data.frame(clean_predicted_occupancy,hours, line_and_direction)
      
      
      }
  })
  
  output$clean_predicted_occupancy = renderTable({
    clean_predicted_occupancy()
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