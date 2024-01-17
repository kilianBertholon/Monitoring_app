###Library
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(readxl)
library(lubridate)
library(data.table)
library(dplyr)


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
  
  
  
  
  ######Partie Tableau donnée ##########
  output$tableau_data <- renderDT({lire_tableau()})
  
  
  
  
  
  
  
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




