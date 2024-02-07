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
            filter(variable %in% c("Age", "Poids", "Masse_Grasse"),
                   Date %in% date_select) %>%
            arrange(desc(Date))
          
          # Renommer la colonne "Valeur" avec la date de la prise de valeur
          data_anthro <- data_anthro %>%
            pivot_wider(names_from = Date, values_from = Valeur)
        } else {
          # Si aucune date n'est sélectionnée, prendre toutes les dates
          data_anthro <- data_sujet %>%
            filter(variable %in% c("Age", "Poids", "Masse_Grasse")) %>%
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
  create_plotly_chart <- function(data,
                                  variable_name,
                                  title,
                                  highlight_color) {
    sujet_select <- input$Sujet
    data_sujet <- data[data$Sujet == sujet_select, ]
    
    # Charger les ranges depuis le fichier Excel
    ranges <- read_excel("data/Range_value.xlsx")
    
    # Filtrer les ranges en fonction de la variable sélectionnée
    selected_range <- ranges[ranges$variable == variable_name, ]
    
    # Ajouter un rectangle de couleur représentant la plage de valeurs
    rectangle <- list(
      type = "rect",
      x0 = min(data$Date),
      x1 = max(data$Date),
      y0 = selected_range$min,
      y1 = selected_range$max,
      fillcolor = "rgba(29, 255, 0, 0.5)",
      # Couleur verte avec opacité réduite
      line = list(width = 0),
      layer = "below"
    )
    
    # Ajouter une zone jaune si les valeurs sont dans les 5% de l'amplitude du range
    yellow_zone_sup <- list(
      type = "rect",
      x0 = min(data$Date),
      x1 = max(data$Date),
      y0 = selected_range$max,
      y1 = round(selected_range$max + (selected_range$max * 0.05),2),
      fillcolor = "rgba(255, 255, 0, 0.5)",
      # Couleur jaune avec opacité réduite
      line = list(width = 0),
      layer = "below"
    )
    
    
    
    # yellow_zone_inf <- list(
    #   type = "rect",
    #   x0 = min(data$Date),
    #   x1 = max(data$Date),
    #   y0 = selected_range$min,
    #   y1 = if(selected_range$min > 0) {
    #     selected_range$min - (selected_range$min * 0.05)
    #   } else {
    #     0
    #   },
    #   fillcolor = "rgba(255, 255, 0, 0.5)",
    #   line = list(width = 0),
    #   layer = "below"
    # )
    
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
        shapes = list(rectangle, yellow_zone_sup)  # Ajouter les zones jaunes et rouges
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
        create_plotly_chart(data_sujet[data_sujet$variable == "CMJ",],
                            "Hauteur du saut(cm)", "Sauts CMJ", "blue")
      })
      
      output$graph_FC_recup <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "FC_recup",],
                            "Fréquence cardiaque de récupération (bpm)",
                            "FC récup",
                            "blue")
      })
      
      output$graph_IFT30_15 <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "IFT30_15",],
                            "IFT30_15 (km/h)", "IFT30_15", "blue")
      })
      
      output$graph_SJ <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "SJ",],
                            "Hauteur du saut (cm)", "Sauts SJ", "blue")
      })
      
      output$graph_Azote_ureique <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Azote_ureique",],
                            "Azote_ureique", "Azote uréique", "blue")
      })
      
      output$graph_Magnesium <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Magnesium",],
                            "Magnesium", "Magnesium", "blue")
      })
      
      output$graph_Bilirubine <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Bilirubine",],
                            "Bilirubine", "Bilirubine", "blue")
      })
      
      
      output$Lactate_deshydrogenase <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Lactate_deshydrogenase",],
                            "Lactate_deshydrogenase",
                            "Lactate deshydrogenase",
                            "blue")
      })
      
      output$Creatine_kinase <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Creatine_kinase",],
                            "Creatine_kinase",
                            "Creatine kinase",
                            "blue")
      })
      
      output$Acide_Urique <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Acide_Urique",],
                            "Acide_Urique", "Acide Urique", "blue")
      })
      
      output$Proteine_C_reactive <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Proteine_C_reactive",],
                            "Proteine_C_reactive",
                            "Proteine C reactive",
                            "blue")
      })
      
      output$Sodium <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Sodium",],
                            "Sodium", "Sodium", "blue")
      })
      
      output$Potassium <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Potassium",],
                            "Potassium", "Potassium", "blue")
      })
      
      output$Calcium <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Calcium",],
                            "Calcium", "Calcium", "blue")
      })
      
      output$Myoglobine <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Myoglobine",],
                            "Myoglobine", "Myoglobine", "blue")
      })
      
      output$Cholesterol <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Cholesterol",],
                            "Cholesterol", "Cholesterol", "blue")
      })
      
      output$HDL <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "HDL",],
                            "HDL", "HDL", "blue")
      })
      
      output$LDL <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "LDL",],
                            "LDL", "LDL", "blue")
      })
      
      output$Triglicerides <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Triglicerides",],
                            "Triglicerides", "Triglicerides", "blue")
      })
      
      output$Glucose <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Glucose",],
                            "Glucose", "Glucose", "blue")
      })
      
      output$WBC <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "WBC",],
                            "WBC", "WBC", "blue")
      })
      
      output$Neutrophiles <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Neutrophiles",],
                            "Neutrophiles", "Neutrophiles", "blue")
      })
      
      output$Lymphocytes <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Lymphocytes",],
                            "Lymphocytes", "Lymphocytes", "blue")
      })
      
      
      output$Monocytes <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Monocytes",],
                            "Monocytes", "Monocytes", "blue")
      })
      
      output$Eosinophile <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Eosinophile",],
                            "Eosinophile", "Eosinophile", "blue")
      })
      
      output$Basophile <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Basophile",],
                            "Basophile", "Basophile", "blue")
      })
      
      output$Plaquettes <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Plaquettes",],
                            "Plaquettes", "Plaquettes", "blue")
      })
      
      output$RBC <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "RBC",],
                            "RBC", "RBC", "blue")
      })
      
      output$Hemoglobine <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Hemoglobine",],
                            "Hemoglobine", "Hemoglobine", "blue")
      })
      
      output$MCV <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "MCV",],
                            "MCV", "MCV", "blue")
      })
      
      output$Hematocrite <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Hematocrite",],
                            "Hematocrite", "Hematocrite", "blue")
      })
      
      output$MCH <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "MCH",],
                            "MCH", "MCH", "blue")
      })
      
      output$MCHC <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "MCHC",],
                            "MCHC", "MCHC", "blue")
      })
      
      output$Transferrine <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Transferrine",],
                            "Transferrine", "Transferrine", "blue")
      })
      
      output$Ferritine <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Ferritine",],
                            "Ferritine", "Ferritine", "blue")
      })
      
      output$Fer <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Fer",],
                            "Fer", "Fer", "blue")
      })
      
      output$Sat_transferrine <- renderPlotly({
        create_plotly_chart(
          data_sujet[data_sujet$variable == "Sat_transferrine",],
          "Sat_transferrine",
          "Saturation en transferrine",
          "blue"
        )
      })
      
      output$Testosterone <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Testosterone",],
                            "Testosterone", "Testosterone", "blue")
      })
      
      output$Cortisol <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Cortisol",],
                            "Cortisol", "Cortisol", "blue")
      })
      
      output$IL_6 <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "IL_6",],
                            "IL_6", "IL_6", "blue")
      })
      
      ##### Partie vitamines
      output$Retinol <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Retinol",],
                            "Retinol", "Retinol", "blue")
      })
      
      output$Beta_carotene <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Beta_carotene",],
                            "Beta_carotene", "Beta carotene", "blue")
      })
      
      output$Vit_E <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Vit_E",],
                            "Vitamine E", "Vitamine E", "blue")
      })
      
      output$Vit_B6 <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Vit_B6",],
                            "Vitamine B6", "Vitramine B6", "blue")
      })
      
      output$Vit_B12 <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Vit_B12",],
                            "Vitamine B12", "Vitramine B12", "blue")
      })
      
      output$Vit_C <- renderPlotly({
        create_plotly_chart(data_sujet[data_sujet$variable == "Vit_C",],
                            "Vitamine C", "Vitamine C", "blue")
      })
      
      output$e1_25_dihydroxyvitamine_D <- renderPlotly({
        create_plotly_chart(
          data_sujet[data_sujet$variable == "1_25-dihydroxyvitamine_D",],
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
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste("rapport-", input$Sujet, "-", format(Sys.Date(), "%Y-%m-%d"), ".pdf")
    },
    content = function(file) {
      # Assurez-vous que les noms des variables correspondent à ceux de votre application
      sujet_selected <- input$Sujet
      
      data_joined <- left_join(data_num, range_value, by = "variable") %>%
        filter(Sujet == sujet_selected) %>% # Filtrer par le sujet sélectionné
        mutate(Ecart = case_when(
          Valeur < min ~ min - Valeur,
          Valeur > max ~ Valeur - max,
          TRUE ~ 0
        ))
      
      data_sorted <- data_joined %>%
        arrange(Sujet, variable, Date)
      
      # Calcul de First_Value et Last_Value pour chaque Sujet et variable, en s'assurant qu'ils sont hors des normes
      value_comparison <- data_sorted %>%
        group_by(Sujet, variable) %>%
        summarise(First_Value = first(Valeur),
                  Last_Value = last(Valeur),
                  Min = first(min),
                  Max = first(max)) %>%
        ungroup() %>%
        mutate(Outside_Norms_First = First_Value < Min | First_Value > Max,
               Outside_Norms_Last = Last_Value < Min | Last_Value > Max) %>%
        filter(Outside_Norms_First & Outside_Norms_Last)  # S'assurer que First et Last Values sont hors des normes
      
      # Calcul des distances par rapport aux normes pour First_Value et Last_Value
      value_comparison <- value_comparison %>%
        mutate(
          # Calcul de la proximité initiale et finale par rapport aux normes
          Proximite_Initiale = pmin(abs(First_Value - Min), abs(First_Value - Max)),
          Proximite_Finale = pmin(abs(Last_Value - Min), abs(Last_Value - Max)),
          # Détermination du rapprochement ou de l'éloignement
          Rapprochement = Proximite_Finale < Proximite_Initiale,
          Eloignement = Proximite_Finale > Proximite_Initiale
        ) %>%
        # S'assurer d'inclure seulement les lignes où les valeurs initiales et finales sont hors des normes
        filter((First_Value < Min | First_Value > Max) & (Last_Value < Min | Last_Value > Max))
      
      
      # Filtrer pour obtenir les variables qui se sont rapprochées des normes
      variables_approaching_norms <- filter(value_comparison, Rapprochement) %>%
        select(Sujet, Variable = variable, First_Value, Last_Value, Min, Max)
      
      # Filtrer pour obtenir les variables qui se sont éloignées des normes
      variables_receding_from_norms <- filter(value_comparison, Eloignement) %>%
        select(Sujet, Variable = variable, First_Value, Last_Value, Min, Max)
      
      
      # Utiliser `params` pour passer les données filtrées à R Markdown
      rmarkdown::render("/Users/mathieubourgeois/Documents/GitHub/Monitoring_app/mon_rapport.Rmd",
                        output_file = file,
                        params = list(variables_approaching_norms = variables_approaching_norms,
                                      variables_receding_from_norms = variables_receding_from_norms,
                                      sujet = sujet_selected))  # Ajoutez cette ligne
      
    }
  )
  
  
  ########### Observe event ################################################
  #Observe event selection de l'athlete
  observeEvent(input$Sujet, {
    updateSelectInput(session,
                      "selectDate",
                      "Choisir une date : ",
                      choices = data_num$Date[data_num$Sujet == input$Sujet])
  })
}