###Library
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)

ui <- dashboardPage(
  #Couleur de la page
  skin = "red",
  
  #Gestion des paramètres pour la partie haute
  dashboardHeader(title = "Application de Monitoring",
                  titleWidth = 450),
  
  #Paramètres de sidebar
  dashboardSidebar(width = 350,
                   sidebarMenu(
                     menuItem(
                       "Analyse",
                       tabName = "analyse",
                       icon = icon("fa-sharp fa-solid fa-magnifying-glass-chart")
                     ),
                     menuItem(
                       "Donnée",
                       tabName = "donnee",
                       icon = icon("fa-sharp fa-solid fa-database")
                     ),
                     menuItem(
                       "Gestion donnée",
                       tabName = "gestion",
                       icon = icon("fa-sharp fa-solid fa-download")
                     )
                   )),
  
  #Paramétres centraux
  dashboardBody(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  tabItems(
    tabItem(
      "analyse",
      fluidRow(column(
        width = 12,
        box(title = "Analyse par athlètes",
            fluidRow(
              column(width = 6, uiOutput("selectAthlete")),
              column(width = 6, uiOutput("selectDate"))
            ))
      )),
      fluidRow(column(
        width = 12,
        box(width = 12, title = "Données anthropométriques",
            DTOutput("tableau_anth"))
      )),
      fluidRow(column(
        width = 12,
        box(width = 12, 
            title = "Performances",
            plotlyOutput("graph_CMJ_FC_recup_IFT30_15_SJ"))
      ))
    ),
    
    tabItem("donnee",
            fluidPage(DTOutput("tableau_data"))),
    tabItem("gestion")
  ))
)