library(readxl)
library(dplyr)


# Lire les données
data_num <- read_sheet(sheet_tidy)

sheet_range <- "https://docs.google.com/spreadsheets/d/1ykAvHxprWWsUDyPJX5gTMTYV6vPJUm4dX-kc68UgiDw/edit?usp=sharing"
range_value <- read_sheet(sheet_range)

data_num$Valeur <- gsub(",", ".", data_num$Valeur)
data_num$Valeur <- gsub("[<>]", "", data_num$Valeur)
data_num$Valeur <- as.numeric(data_num$Valeur)
data_num$Valeur <- round(data_num$Valeur, 3)
data_num$Date <- as.Date(data_num$Date, format = "%d/%m/%Y")

# Joindre les dataframes pour comparer les valeurs
data_joined <- left_join(data_num, range_value, by = "Variable")

# Identifier les valeurs en dehors des plages normatives
values_out_of_range <- data_joined %>%
  filter(Valeur < min | Valeur > max) %>%
  filter(Sujet=='A') %>%
  select(Sujet, Date, Categorie, Variable, Valeur, min, max, Unite)

### 

data_joined <- left_join(data_num, range_value, by = "Variable") %>%
  filter(Sujet == 'A') %>% # Filtrer par le sujet sélectionné
  mutate(Ecart = case_when(
    Valeur < min ~ min - Valeur,
    Valeur > max ~ Valeur - max,
    TRUE ~ 0
  ))

data_sorted <- data_joined %>%
  arrange(Sujet, Variable, Date)

# Calcul de First_Value et Last_Value pour chaque Sujet et Variable, en s'assurant qu'ils sont hors des normes
value_comparison <- data_sorted %>%
  group_by(Sujet, Variable) %>%
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


# Joindre value_comparison avec range_value pour récupérer 'Unite'
value_comparison_with_unit <- value_comparison %>%
  left_join(range_value %>% select(Variable, Unite), by = "Variable")

# Assurez-vous que la jointure a réussi en vérifiant si 'Unite' est maintenant disponible
# puis continuez avec la création des tableaux ajustés

# Filtrer pour obtenir les Variables qui se sont rapprochées des normes
Variables_approaching_norms <- value_comparison_with_unit %>%
  filter(Rapprochement) %>%
  mutate(Norme = paste(Min, "à", Max, Unite)) %>%
  select(Variable = Variable, `Avant-dernière prise de sang` = First_Value, `Dernière prise de sang` = Last_Value, Norme)

# Filtrer pour obtenir les Variables qui se sont éloignées des normes
Variables_receding_from_norms <- value_comparison_with_unit %>%
  filter(Eloignement) %>%
  mutate(Norme = paste(Min, "à", Max, Unite)) %>%
  select(Variable = Variable, `Avant-dernière prise de sang` = First_Value, `Dernière prise de sang` = Last_Value, Norme)



# Filtrer les valeurs hors normes pour le sujet 'A'
values_out_of_range2 <- data_joined %>%
  filter(Valeur < min | Valeur > max, Sujet == 'A') %>%
  arrange(desc(Date)) %>%
  group_by(Variable) %>%
  slice(1) %>%
  ungroup() %>%
  select(Sujet, Date, Categorie, Variable, Valeur, min, max, Unite)


# Concaténer Variables_approaching_norms et Variables_receding_from_norms
combined_vars <- bind_rows(
  Variables_approaching_norms %>% select(Variable),
  Variables_receding_from_norms %>% select(Variable)
)

# Exclure les variables déjà présentes dans combined_vars de values_out_of_range
values_out_of_range_filtered <- values_out_of_range %>%
  anti_join(combined_vars, by = "Variable")


# Identifier la dernière date pour le sujet 'A'
last_date <- max(data_num$Date[data_num$Sujet == 'A'])

values_to_check <- values_out_of_range_filtered %>%
  filter(Sujet == 'A', Date == last_date, Valeur < min | Valeur > max) %>%
  select(Sujet, Date, Categorie, Variable, Valeur, min, max, Unite)




################# RadarChart

sheet_radar <- "https://docs.google.com/spreadsheets/d/1OaH5HMtmgiAYGv35wNGT1t31qL3dNLObJWrgS8wElMk/edit#gid=0"
data_radar_2 <- read_sheet(sheet_radar)

#Traitement 
## Date
data_radar_2$Date <- format(data_radar_2$Date, "%e/%m/%Y")

#Data num (sans <,>)
data_radar <- copy(data_radar_2)
data_radar$Valeur <- gsub(",", ".", data_radar$Valeur)
data_radar$Valeur <- gsub("[<>]", "", data_radar$Valeur)
data_radar$Valeur <- as.numeric(data_radar$Valeur)
data_radar$Valeur <- round(data_radar$Valeur, 3)
data_radar$Date <- as.Date(data_radar$Date, format = "%d/%m/%Y")

radar_data <- function(sujet, date) {
  A_juin2023 <- data_radar |>
    filter(Sujet == sujet, Date == date) |>
    select(Max, Min, Valeur, Valeur_max, Valeur_min)
  A_juin2023 <- as.data.frame(t(A_juin2023))
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
  return(A_juin2023)
}

# Définition de la fonction plot_radar_graphs
plot_radar_graphs <- function(data_list, titles_list) {
  par(mfrow = c(2, 3))  # Définir la disposition des sous-graphiques
  
  for (i in seq_along(data_list)) {
    radarchart(data_list[[i]], title = titles_list[i], axistype = 1, seg = 5, 
               pcol = c("blue", "red", "green"), plwd = c(1, 1, 1), 
               pfcol = c(alpha("blue", 0.3), alpha("red", 0.3), alpha("green", 0.3)), 
               plty = 1)
  }
  
  # Ajouter une légende
  legend("bottomright", legend = c("Valeur", "Valeur_max", "Valeur_min"), 
         fill = c("blue", "red", "green"))
}

