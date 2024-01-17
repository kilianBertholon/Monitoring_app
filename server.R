###Library
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(readxl)
library(lubridate)
library(data.table)


source(file = "script.R")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ######Partie Analyse ##########
  
  
  
  
  
  
  ######Partie Tableau donnée ##########
  output$tableau_data <- renderDT({lire_tableau()})
  
  
  
  
  
  
  
  ######Partie gestion des données ##########
  
  
}




