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
library(plotly)


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
  
  
  ### Partie Performances 
  
  output$graph_CMJ_FC_recup_IFT30_15_SJ <- renderPlotly({
    sujet_select <- input$Sujet
    date_select <- input$Date
    
    # Vérifier si un sujet est sélectionné
    if (!is.null(sujet_select)) {
      data_sujet <- data_num[data_num$Sujet == sujet_select, ]
      
      # Vérifier si des lignes sont présentes pour le sujet sélectionné
      if (nrow(data_sujet) > 0) {
        # Filtrer par date si une date est sélectionnée
        if (!is.null(date_select)) {
          data_CMJ <- data_sujet %>%
            filter(Variable == "CMJ", Date %in% date_select) %>%
            arrange(desc(Date))
          data_FC_recup <- data_sujet %>%
            filter(Variable == "FC_recup", Date %in% date_select) %>%
            arrange(desc(Date))
          data_IFT30_15 <- data_sujet %>%
            filter(Variable == "IFT30_15", Date %in% date_select) %>%
            arrange(desc(Date))
          data_SJ <- data_sujet %>%
            filter(Variable == "SJ", Date %in% date_select) %>%
            arrange(desc(Date))
        } else {
          # Si aucune date n'est sélectionnée, prendre toutes les dates
          data_CMJ <- data_sujet %>%
            filter(Variable == "CMJ") %>%
            arrange(desc(Date))
          data_FC_recup <- data_sujet %>%
            filter(Variable == "FC_recup") %>%
            arrange(desc(Date))
          data_IFT30_15 <- data_sujet %>%
            filter(Variable == "IFT30_15") %>%
            arrange(desc(Date))
          data_SJ <- data_sujet %>%
            filter(Variable == "SJ") %>%
            arrange(desc(Date))
        }
        
        # Créer les graphiques plotly et les afficher côte à côte
        p1 <- plot_ly(data_CMJ, x = ~Date, y = ~Valeur, type = "scatter", mode = "markers") %>%
          layout(title = "Sauts CMJ par date",
                 xaxis = list(title = "Date"),
                 yaxis = list(title = "Hauteur du saut"))
        
        p2 <- plot_ly(data_FC_recup, x = ~Date, y = ~Valeur, type = "scatter", mode = "markers") %>%
          layout(title = "Fréquence cardiaque de récupération par date",
                 xaxis = list(title = "Date"),
                 yaxis = list(title = "Fréquence cardiaque de récupération"))
        
        p3 <- plot_ly(data_IFT30_15, x = ~Date, y = ~Valeur, type = "scatter", mode = "markers") %>%
          layout(title = "IFT30_15 par date",
                 xaxis = list(title = "Date"),
                 yaxis = list(title = "IFT30_15"))
        
        p4 <- plot_ly(data_SJ, x = ~Date, y = ~Valeur, type = "scatter", mode = "markers") %>%
          layout(title = "Sauts SJ par date",
                 xaxis = list(title = "Date"),
                 yaxis = list(title = "Hauteur du saut"))
        
        subplot(p1, p2, p3, p4, nrows = 1)
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
