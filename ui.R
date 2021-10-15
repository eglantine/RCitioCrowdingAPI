library(httr)
library(purrr)
library(shiny)
 
agency_list = map(content(GET("http://django.gateway.cit.io/agencies/")), 1)
agency_list = sort(unlist(agency_list))

shinyUI(fluidPage(theme = "bootstrap.css",
                  img(src = "logo_citio.png",
                      style="display: block; margin-left: auto; margin-right: auto;"),
                  
                  headerPanel("Données de prédiction"),
                  
                  sidebarPanel(
                    tags$h3("Connexion"),
                    textInput("login", "Identifiant", "eglantine@cit.io"),
                    passwordInput("password", "Mot de passe",""),
                    selectInput("group", "Réseau",
                                choices=agency_list,selected = "orleansmetropole"),
                    selectInput("env", "Environnement", 
                                choices=c("staging", "production")),
                    
                    actionButton("doLogin", "Se connecter"),

                    
                    tags$h3("Données"),
                    dateInput("service_date", "Date ", value = Sys.Date(),),
                    downloadButton("downloadRawData", "Télécharger les données brutes"),
                    
                    tags$h3("Visualisation"),
                    selectInput("aggregation_y", "Axe vertical", 
                                choices=c("line_short_name", 
                                          "time",
                                          "line_and_direction",
                                          "hours"),
                                selected = "line_short_name"),
                    selectInput("aggregation_x", "Axe horizontal", 
                                choices=c("line_short_name", 
                                          "line_and_direction",
                                          "time",
                                          "hours"),
                                selected = "time")
                    
                    
                  ),
                  
                  mainPanel(
                    tabsetPanel(type = "tabs", 
                                tabPanel("Visualisation", plotOutput("heatmap")),
                                tabPanel("Données brutes", dataTableOutput("clean_predicted_occupancy"))
                    )
                  )
                  )
        )
