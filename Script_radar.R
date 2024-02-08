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
library(googlesheets4)
library(tidyr)
library(fmsb)
library(viridisLite)

#Import data + traitement
gs4_deauth()
sheet_radar <- "https://docs.google.com/spreadsheets/d/1OaH5HMtmgiAYGv35wNGT1t31qL3dNLObJWrgS8wElMk/edit#gid=0"
data_radar <- read_sheet(sheet_radar)

#Traitement 
## Date
data_radar$Date <- format(data_radar$Date, "%e/%m/%Y")

#Data num (sans <,>)
data_radar <- copy(data)
data_radar$Valeur <- gsub(",", ".", data_radar$Valeur)
data_radar$Valeur <- gsub("[<>]", "", data_radar$Valeur)
data_radar$Valeur <- as.numeric(data_radar$Valeur)
data_radar$Valeur <- round(data_radar$Valeur, 3)
data_radar$Date <- as.Date(data_radar$Date, format = "%d/%m/%Y")


A_juin2023 <- data |>
  filter(Sujet == "A", Date == "30/06/2023") |> 
  select(Max, Min, Valeur, Valeur_max, Valeur_min)

A_juin2023 <- as.data.frame(t(A_juin2023))

# Utiliser la première ligne comme noms de colonnes
colnames <- c("Azote_ureique", "Magnesium",
              "Bilirubine", "Lactate_deshydrogenase",
              "Creatine_kinase", "Acide_Urique",
              "Proteine_C_reactive", "Sodium", "Potassium",
              "Calcium", "Myoglobine", "Cholesterol",
              "HDL", "LDL", "Triglicerides",
              "Glucose", "WBC", "Neutrophiles",
              "Lymphocytes", "Monocytes", "Eosinophile",
              "Basophile", "Plaquettes", "RBC",
              "Hemoglobine", "MCV", "Hematocrite",
              "MCH", "MCHC", "Transferrine",
              "Ferritine", "Fer", "Sat_transferrine",
              "Testosterone", "Cortisol", "IL_6",
              "Retinol", "Beta_carotene", "Vit_E",
              "Vit_B6", "Vit_B12", "Vit_C",
              "1_25-dihydroxyvitamine_D")

colnames(A_juin2023) <- colnames

A_juin2023 <- data.frame(lapply(A_juin2023, function(x) as.numeric(as.character(trimws(x)))))

#-----------------------------------------------------------------

A_juin2023_serum_sang <- A_juin2023[, 1:16]

A_juin2023_analyse_sang <- A_juin2023[, 17:29]

A_juin2023_hemato_fer <- A_juin2023[, 30:33]

A_juin2023_hormones <- A_juin2023[, 34:36]

A_juin2023_vitamines <- A_juin2023[, 37:43]



# Définir la disposition des sous-graphiques
par(mfrow = c(2, 3))


# Créer le graphique radar pour A_juin2023_serum_sang
radarchart(A_juin2023_serum_sang, title = "Serum sang", axistype = 1, seg = 5, pcol = c("blue", "red", "green"), plwd = c(1, 1, 1), pfcol = c(alpha("blue", 0.3), alpha("red", 0.3), alpha("green", 0.3)), plty = 1)

# Créer le graphique radar pour A_juin2023_analyse_sang
radarchart(A_juin2023_analyse_sang, title = "Analyse sang", axistype = 1, seg = 5, pcol = c("blue", "red", "green"), plwd = c(1, 1, 1), pfcol = c(alpha("blue", 0.3), alpha("red", 0.3), alpha("green", 0.3)), plty = 1)

# Créer le graphique radar pour A_juin2023_hemato_fer
radarchart(A_juin2023_hemato_fer, title = "Hemato fer", axistype = 1, seg = 5, pcol = c("blue", "red", "green"), plwd = c(1, 1, 1), pfcol = c(alpha("blue", 0.3), alpha("red", 0.3), alpha("green", 0.3)), plty = 1)

# Créer le graphique radar pour A_juin2023_hormones
radarchart(A_juin2023_hormones, title = "Hormones", axistype = 1, seg = 5, pcol = c("blue", "red", "green"), plwd = c(1, 1, 1), pfcol = c(alpha("blue", 0.3), alpha("red", 0.3), alpha("green", 0.3)), plty = 1)

# Créer le graphique radar pour A_juin2023_vitamines
radarchart(A_juin2023_vitamines, title = "Vitamines", axistype = 1, seg = 5, pcol = c("blue", "red", "green"), plwd = c(1, 1, 1), pfcol = c(alpha("blue", 0.3), alpha("red", 0.3), alpha("green", 0.3)), plty = 1)

# Ajouter une légende
legend("bottomright", legend = c("Valeur", "Valeur_max", "Valeur_min"), fill = c("blue", "red", "green"))
