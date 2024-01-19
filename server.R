###Library
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(readxl)
library(lubridate)
library(data.table)
library(dplyr)
library(tidyr)


source(file = "script.R")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ######Partie Analyse ##########
  
  output$selectAthlete <- renderUI({
    selectInput("Sujet",
                "Chosir un sujet : ",
                choices = unique(Sujet),
                multiple = FALSE)
  })
  
  output$selectDate <- renderUI({
    selectInput("Date",
                "Chosir une Date : ",
                choices = date,
                multiple = TRUE)
  })
  
  output$tableau_anth <- renderDT({
    sujet_select <- input$Sujet
    date_select <- input$Date
    
    # Vérifier si un sujet est sélectionné
    if (!is.null(sujet_select)) {
      data_sujet <- data_num[data_num$Sujet == sujet_select,]
      
      # Vérifier si des lignes sont présentes pour le sujet sélectionné
      if (nrow(data_sujet) > 0) {
        # Filtrer par date si une date est sélectionnée
        if (!is.null(date_select)) {
          data_anthro <- data_sujet %>%
            filter(Variable %in% c("Age", "Poids", "Masse_Grasse"),
                   Date %in% date_select) %>%
            arrange(desc(Date))
          
          # Renommer la colonne "Valeur" avec la date de la prise de valeur
          data_anthro <- data_anthro %>%
            pivot_wider(names_from = Date, values_from = Valeur)
        } else {
          # Si aucune date n'est sélectionnée, prendre toutes les dates
          data_anthro <- data_sujet %>%
            filter(Variable %in% c("Age", "Poids", "Masse_Grasse")) %>%
            arrange(desc(Date))
          
          # Renommer la colonne "Valeur" avec la date de la prise de valeur
          data_anthro <- data_anthro %>%
            pivot_wider(names_from = Date, values_from = Valeur)
        }
        
        return(datatable(data_anthro))
      } else {
        # Aucune ligne pour le sujet sélectionné
        return(NULL)
      }
    } else {
      # Aucun sujet sélectionné
      return(NULL)
    }
  })
  
  
  
  
  ######Partie Tableau donnée ##########
  output$tableau_data <- renderDT({
    lire_tableau()
  })
  
  
  
  
  
  
  
  ######Partie gestion des données ##########
  
  
  
  
  
  
  
  ########### Observe event ################################################
  #Observe event selection de l'athlete
  observeEvent(input$Sujet, {
    updateSelectInput(session,
                      "selectDate",
                      "Choisir une date : ",
                      choices = data_num$Date[data_num$Sujet == input$Sujet])
  })
}
