###Library
library(shiny)
library(shinydashboard)
library(shinyjs)

ui <- dashboardPage(
  dashboardHeader(title = "Application de Monitoring"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analyse", tabName = "analyse", icon = icon("fa-sharp fa-solid fa-magnifying-glass-chart")),
      menuItem("Donnée", tabName = "donnee", icon = icon("fa-sharp fa-solid fa-database")),
      menuItem("Gestion donnée", tabName = "gestion", icon = icon("fa-sharp fa-solid fa-download"))
    )
  ),
  dashboardBody()
)