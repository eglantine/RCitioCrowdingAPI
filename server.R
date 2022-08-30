#devtools::install_github("eglantine/RCitioPackage",ref ="main")
#devtools::install_git("https://gitlab.dev.cit.io/citio/dev/personal_tools/r_citio_package", credentials=git2r::cred_token("GITLAB_PAT"))
library(shiny)
library(httr)
library(ggplot2)
library(dplyr)
library(xml2)
library(jsonlite)
library(data.table)
library(devtools)
library(remotes)
library(RCitioPackage)
library(readr)

function(input, output, session) {
  
  session_id = eventReactive(input$doLogin,{
    
    session_id = getSessionId(input$login, input$password, input$agency, input$env)
    
    if(!is.null(session_id)){
      print(paste(Sys.time(), "Login successful with session_id",session_id, "for user", input$login))
      
      return(session_id)
    }
    
  })
  
  api_base_url = reactive({
    buildBaseUrl(input$agency, "api", input$env)
  })
  
  prediction_base_url = reactive({
    buildBaseUrl(input$agency, "prediction", input$env)
  })
  
  
  raw_occupancy_data = reactive({
    
    shiny::validate(
      shiny::need(input$doLogin, "Veuillez vous connecter pour accéder aux données."),
      shiny::need(session_id()!= "", "Connexion impossible. Veuillez vérifier votre identifiant et votre mot de passe.")
    )
    
    print(paste(Sys.time(),"Querying API"))
    showNotification(paste("Récupération des données de",
                           input$agency, 
                           "(",input$env, ")"))
    
    
    
    raw_occupancy_data = getPredictedOccupancyData2(prediction_base_url(),
                                                    session_id = session_id(),
                                                    service_date = input$service_date,
                                                    show_all_occupancy = TRUE,
                                                    version = input$api_version)
    
    shiny::validate(
      shiny::need(nrow(raw_occupancy_data)>0, "Pas de prédiction disponible pour ces paramètres.")
    )
    
    print(paste(Sys.time(),"Data retrieved"))
    
    return(raw_occupancy_data)
  })
  
  referential = reactive({
    
    referential = getCloudReferentialSection(api_base_url(),session_id(),"gtfs_stops") %>%
      select(gtfs_id, stop_id) %>%
      left_join(y = getCloudReferentialSection(api_base_url(),session_id(),"stops"),
                by = c("stop_id" = "id")) %>%
      select(gtfs_id, stop_id, station_id) %>%
      left_join(y = getCloudReferentialSection(api_base_url(),session_id(),"stations"),
                by = c("station_id" = "id")) %>%
      rename(station_name =  name) %>%
      mutate(gtfs_id = as.character(gtfs_id)) %>%
      select(gtfs_id, stop_id, station_id, station_name)
    
    # stops = getCloudReferentialSection(api_base_url(),session_id(),"stops")
    # stops = data.frame(stops$id, stops$station_id)
    # 
    # stations = getCloudReferentialSection(api_base_url(),session_id(),"stations")
    # stations = data.frame(stations$id, stations$name)
    # 
    # gtfs_stops = getCloudReferentialSection(api_base_url(),session_id(),"gtfs_stops")
    # gtfs_stops = data.frame(gtfs_stops$gtfs_id, gtfs_stops$stop_id)
    # 
    # referential = merge(stops,
    #                     gtfs_stops,
    #                     by.x = "stops.id",
    #                     by.y = "gtfs_stops.stop_id")
    # 
    # referential = merge(referential,
    #                     stations,
    #                     by.x = "stops.station_id",
    #                     by.y = "stations.id")
    # 
    # names(referential) = c("stop_id", "station_id", "gtfs_stop_id", "station_name")
    
    return(referential)
    
  })
  
  clean_predicted_occupancy = reactive({
    
    clean_predicted_occupancy = raw_occupancy_data() %>%
      left_join(y = referential(), 
                by = c("terminus_gtfs_stop_id" = "gtfs_id")) %>%
      rename(terminus_station_name = station_name,
             terminus_stop_id = stop_id,
             terminus_station_id =  station_id) %>%
      left_join(y = referential(), 
                by = c("gtfs_stop_id" = "gtfs_id")) %>%
      mutate(hours = gsub("(15|30|45):00","00:00", time),
             line_and_direction = paste(line_short_name,terminus_station_name,sep = "_"))
    
    # clean_predicted_occupancy = merge(raw_occupancy_data(),
    #                                   referential(),
    #                                   by.x = "terminus_gtfs_stop_id",
    #                                   by.y = "gtfs_stop_id",
    #                                   all.x = T)
    # names(clean_predicted_occupancy)[names(clean_predicted_occupancy) == "station_name"] <- "terminus_station_name"
    # names(clean_predicted_occupancy)[names(clean_predicted_occupancy) == "stop_id"] <- "terminus_stop_id"
    # names(clean_predicted_occupancy)[names(clean_predicted_occupancy) == "station_id"] <- "terminus_station_id"
    # 
    # clean_predicted_occupancy = merge(clean_predicted_occupancy,
    #                                   referential(),
    #                                   by.x = "gtfs_stop_id",
    #                                   by.y = "gtfs_stop_id",
    #                                   all.x = T)
    # 
    # hours = gsub("(15|30|45):00","00:00",clean_predicted_occupancy$time)
    # line_and_direction = paste(clean_predicted_occupancy$line_short_name,clean_predicted_occupancy$terminus_station_name,sep = "_")
    # clean_predicted_occupancy = data.frame(clean_predicted_occupancy,hours, line_and_direction)
  })
  
  output$clean_predicted_occupancy = renderDataTable({
    clean_predicted_occupancy()
  })
  
  output$downloadRawData <- downloadHandler(
    filename = function() {
      paste0(input$agency," - ",input$env," - ", input$date, ".csv")
    },
    content = function(file) {
      write.csv2(clean_predicted_occupancy(), file, row.names = FALSE)
    }
  )
  
  heatmap = reactive({
    
    graph_data = clean_predicted_occupancy() %>%
      group_by(.data[[input$aggregation_x]], .data[[input$aggregation_y]]) %>%
      summarise(occupancy = sum(.data[[input$occupancy_type]]),
                occupancy_rate = sum(occupancy)/sum(capacity))
    
    ggplot(graph_data, aes(.data[[input$aggregation_x]], .data[[input$aggregation_y]])) +
      geom_point(aes(size = occupancy, colour = occupancy_rate)) +
      scale_color_gradientn(colors = c("#80C480", "#FFC165", "#E63323")) +
      theme(axis.text.x=element_text(angle=45, hjust=1),
            panel.background = element_rect(fill = "#EFEFF7", colour = "white"))
  })
  
  output$heatmap <- renderPlot({
    print(heatmap())
  })
  
  output$downloadHeatmap <- downloadHandler(
    filename = function() {
      paste0(input$agency," - ",input$env," - ", input$date, ".png")
    },
    content = function(file) {
      ggsave(file,heatmap())
    }
  )
  
}
