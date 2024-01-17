###Library
library(shiny)
library(shinydashboard)
library(shinyjs)

ui <- dashboardPage(
  #Couleur de la page 
  skin = "red",
  
  #Gestion des paramètres pour la partie haute
  dashboardHeader(title = "Application de Monitoring",
                  titleWidth = 450),
  
  #Paramètres de sidebar
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Analyse", tabName = "analyse", icon = icon("fa-sharp fa-solid fa-magnifying-glass-chart")),
      menuItem("Donnée", tabName = "donnee", icon = icon("fa-sharp fa-solid fa-database")),
      menuItem("Gestion donnée", tabName = "gestion", icon = icon("fa-sharp fa-solid fa-download"))
    )
  ),
  
  #Paramétres centraux
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"
  ))
))