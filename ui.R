library(httr)
library(purrr)
library(shiny)

agency_list = map(content(GET("http://django.gateway.staging.cit.io/agencies/")), 1)
prediction_api = sapply(content(GET("http://django.gateway.staging.cit.io/agencies/")), function(x) x$prediction_apis)
active_agency_list = agency_list[prediction_api == TRUE]
active_agency_list = sort(unlist(active_agency_list))

shinyUI(fluidPage(
  theme = "bootstrap.css",
  img(src = "logo_citio.png",
      style="display: block; 
                      margin-left: auto; 
                      margin-right: auto;"),
  
  headerPanel("Données de prédiction"),
  
  sidebarPanel(
    tags$h3("Connexion"),
    textInput("login", "Identifiant", "eglantine@cit.io"),
    passwordInput("password", "Mot de passe",""),
    selectInput("agency", "Réseau",
                choices=active_agency_list,selected = "orleansmetropole"),
    selectInput("env", "Environnement", 
                choices=c("staging", "production")),
    
    actionButton("doLogin", "Se connecter"),
    
    
    tags$h3("Données"),
    dateInput("service_date", 
              "Date ", 
              value = Sys.Date(),
              weekstart = 1,
              language = "fr",
              format = "dd/mm/yyyy"
    ),
    downloadButton("downloadRawData", "Télécharger les données brutes"),
    
    tags$h3("Visualisation"),
    selectInput("occupancy_type", "Type de charge",
                choices=c("Mixte" = "occupancy",
                          "Comptage" = "occupancy_counting_cells",
                          "Billettique réhaussée" = "occupancy_adjusted_ticketing",
                          "Unifiée" =  "occupancy_unified"),
                selected = "occupancy"),
    
    selectInput("aggregation_y", "Axe vertical",
                choices=c("Ligne" = "line_short_name",
                          "Ligne et direction" = "line_and_direction",
                          "Heure (quart d'heure)" =  "time",
                          "Heure" = "hours"),
                selected = "line_short_name"),
    
     selectInput("aggregation_x", "Axe horizontal",
                choices=c("Ligne" = "line_short_name",
                          "Ligne et direction" = "line_and_direction",
                          "Heure (quart d'heure)" =  "time",
                          "Heure" = "hours"),
                selected = "time"),
       numericInput("plot_heigth", "Taille du graphique (px)", value = 400),

       downloadButton('downloadHeatmap', "Export image")
    
    
    
  ),
  
  mainPanel(
    tabsetPanel(
      type = "tabs", 
                tabPanel("Visualisation", plotOutput("heatmap")),
                tabPanel("Données brutes", dataTableOutput("clean_predicted_occupancy"))
    )
  )
  
))
