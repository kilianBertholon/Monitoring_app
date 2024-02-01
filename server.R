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
      data_sujet <- data_num[data_num$Sujet == sujet_select, ]
      
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
  
  # Fonction pour créer un graphique plotly avec des lignes reliant les points
  # Fonction pour créer un graphique plotly avec des lignes reliant les points de chaque athlète
  create_plotly_chart <-
    function(data,
             variable_name,
             title,
             highlight_color) {
      sujet_select <- input$Sujet
      data_sujet <- data[data$Sujet == sujet_select,]
      
      # Charger les ranges depuis le fichier Excel
      ranges <-
        read_excel("data/Range_value.xlsx")
      
      # Filtrer les ranges en fonction de la variable sélectionnée
      selected_range <- ranges[ranges$variable == variable_name,]
      
      # Ajouter un rectangle de couleur représentant la plage de valeurs
      rectangle <- list(
        type = "rect",
        x0 = min(data$Date),
        x1 = max(data$Date),
        y0 = selected_range$range_min,
        y1 = selected_range$range_max,
        fillcolor = "rgba(152, 251, 152, 0.5)",
        # Couleur verte avec opacité réduite
        line = list(width = 0),
        layer = "below"
      )
      
      # Créer le graphique Plotly
      plot_ly(
        data,
        x = ~ Date,
        y = ~ Valeur,
        type = "scatter",
        mode = "markers",
        marker = list(
          color = ~ ifelse(data$Sujet == sujet_select, highlight_color, "gray")
        ),
        showlegend = FALSE
      ) %>%
        add_trace(
          data = data_sujet,
          type = "scatter",
          mode = "lines",
          line = list(color = highlight_color),
          inherit = FALSE
        ) %>%
        layout(
          title = title,
          xaxis = list(title = "Date"),
          yaxis = list(title = variable_name),
          shapes = rectangle  # Ajouter le rectangle de couleur pour la plage de valeurs
        )
    }
  
  # Observer event pour mettre à jour les graphiques
  observe({
    sujet_select <- input$Sujet
    date_select <- input$Date
    
    # Vérifier si un sujet est sélectionné
    if (!is.null(sujet_select)) {
      data_sujet <- data_num
      data_sujet$highlight_color <-
        ifelse(data_sujet$Sujet == sujet_select, "blue", "gray")
      
      # Filtrer par date si une date est sélectionnée
      if (!is.null(date_select)) {
        data_sujet <- data_sujet %>%
          filter(Date %in% date_select) %>%
          arrange(desc(Date))
      } else {
        # Si aucune date n'est sélectionnée, prendre toutes les dates
        data_sujet <- data_sujet %>%
          arrange(desc(Date))
      }
      
      # Créer les graphiques plotly avec lignes reliant les points de chaque athlète
      output$graph_CMJ <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "CMJ", ],
                            "Hauteur du saut(cm)", "Sauts CMJ", "blue")
      })
      
      output$graph_FC_recup <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "FC_recup", ],
                            "Fréquence cardiaque de récupération (bpm)",
                            "FC récup",
                            "blue")
      })
      
      output$graph_IFT30_15 <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "IFT30_15", ],
                            "IFT30_15 (km/h)", "IFT30_15", "blue")
      })
      
      output$graph_SJ <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "SJ", ],
                            "Hauteur du saut (cm)", "Sauts SJ", "blue")
      })
      
      output$graph_Azote_ureique <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Azote_ureique", ],
                            "Azote uréique", "Azote uréique", "blue")
      })
      
      output$graph_Magnesium <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Magnesium", ],
                            "Magnesium", "Magnesium", "blue")
      })
      
      output$graph_Bilirubine <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Bilirubine", ],
                            "Bilirubine", "Bilirubine", "blue")
      })
      
      
      output$Lactate_deshydrogenase <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Lactate_deshydrogenase", ],
                            "Lactate deshydrogenase",
                            "Lactate deshydrogenase",
                            "blue")
      })
      
      output$Creatine_kinase <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Creatine_kinase", ],
                            "Creatine kinase",
                            "Creatine kinase",
                            "blue")
      })
      
      output$Acide_Urique <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Acide_Urique", ],
                            "Acide Urique", "Acide Urique", "blue")
      })
      
      output$Proteine_C_reactive <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Proteine_C_reactive", ],
                            "Proteine C reactive",
                            "Proteine C reactive",
                            "blue")
      })
      
      output$Sodium <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Sodium", ],
                            "Sodium", "Sodium", "blue")
      })
      
      output$Potassium <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Potassium", ],
                            "Potassium", "Potassium", "blue")
      })
      
      output$Calcium <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Calcium", ],
                            "Calcium", "Calcium", "blue")
      })
      
      output$Myoglobine <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Myoglobine", ],
                            "Myoglobine", "Myoglobine", "blue")
      })
      
      output$Cholesterol <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Cholesterol", ],
                            "Cholesterol", "Cholesterol", "blue")
      })
      
      output$HDL <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "HDL", ],
                            "HDL", "HDL", "blue")
      })
      
      output$LDL <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "LDL", ],
                            "LDL", "LDL", "blue")
      })
      
      output$Triglicerides <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Triglicerides", ],
                            "Triglicerides", "Triglicerides", "blue")
      })
      
      output$Glucose <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Glucose", ],
                            "Glucose", "Glucose", "blue")
      })
      
      output$WBC <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "WBC", ],
                            "WBC", "WBC", "blue")
      })
      
      output$Neutrophiles <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Neutrophiles", ],
                            "Neutrophiles", "Neutrophiles", "blue")
      })
      
      output$Lymphocytes <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Lymphocytes", ],
                            "Lymphocytes", "Lymphocytes", "blue")
      })
      
      
      output$Monocytes <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Monocytes", ],
                            "Monocytes", "Monocytes", "blue")
      })
      
      output$Eosinophile <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Eosinophile", ],
                            "Eosinophile", "Eosinophile", "blue")
      })
      
      output$Basophile <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Basophile", ],
                            "Basophile", "Basophile", "blue")
      })
      
      output$Plaquettes <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Plaquettes", ],
                            "Plaquettes", "Plaquettes", "blue")
      })
      
      output$RBC <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "RBC", ],
                            "RBC", "RBC", "blue")
      })
      
      output$Hemoglobine <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Hemoglobine", ],
                            "Hemoglobine", "Hemoglobine", "blue")
      })
      
      output$MCV <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "MCV", ],
                            "MCV", "MCV", "blue")
      })
      
      output$Hematocrite <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Hematocrite", ],
                            "Hematocrite", "Hematocrite", "blue")
      })
      
      output$MCH <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "MCH", ],
                            "MCH", "MCH", "blue")
      })
      
      output$MCHC <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "MCHC", ],
                            "MCHC", "MCHC", "blue")
      })
      
      output$Transferrine <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Transferrine", ],
                            "Transferrine", "Transferrine", "blue")
      })
      
      output$Ferritine <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Ferritine", ],
                            "Ferritine", "Ferritine", "blue")
      })
      
      output$Fer <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Fer", ],
                            "Fer", "Fer", "blue")
      })
      
      output$Sat_transferrine <- renderPlotly({
        create_plotly_chart(
          data_sujet[data_sujet$Variable == "Sat_transferrine", ],
          "Saturation en transferrine",
          "Saturation en transferrine",
          "blue"
        )
      })
      
      output$Testosterone <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Testosterone", ],
                            "Testosterone", "Testosterone", "blue")
      })
      
      output$Cortisol <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Cortisol", ],
                            "Cortisol", "Cortisol", "blue")
      })
      
      output$IL_6 <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "IL_6", ],
                            "IL_6", "IL_6", "blue")
      })
      
      ##### Partie vitamines
      output$Retinol <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Retinol", ],
                            "Retinol", "Retinol", "blue")
      })
      
      output$Beta_carotene <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Beta_carotene", ],
                            "Beta carotene", "Beta carotene", "blue")
      })
      
      output$Vit_E <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Vit_E", ],
                            "Vitamine E", "Vitamine E", "blue")
      })
      
      output$Vit_B6 <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Vit_B6", ],
                            "Vitamine B6", "Vitramine B6", "blue")
      })
      
      output$Vit_B12 <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Vit_B12", ],
                            "Vitamine B12", "Vitramine B12", "blue")
      })
      
      output$Vit_C <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$Variable == "Vit_C", ],
                            "Vitamine C", "Vitamine C", "blue")
      })
      
      output$e1_25_dihydroxyvitamine_D <- renderPlotly({
        create_plotly_chart(
          data_sujet[data_sujet$Variable == "1_25-dihydroxyvitamine_D", ],
          "1_25_dihydroxyvitamine_D",
          "1_25_dihydroxyvitamine_D",
          "blue"
        )
      })
      
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
