library(readxl)
library(dplyr)

# Lire les données
data_num <- read_excel("data/data_tidy.xlsx")
range_value <- read_excel("data/Range_value.xlsx")

data_num$Valeur <- gsub(",", ".", data_num$Valeur)
data_num$Valeur <- gsub("[<>]", "", data_num$Valeur)
data_num$Valeur <- as.numeric(data_num$Valeur)
data_num$Valeur <- round(data_num$Valeur, 3)
data_num$Date <- as.Date(data_num$Date, format = "%d/%m/%Y")

# Joindre les dataframes pour comparer les valeurs
data_joined <- left_join(data_num, range_value, by = "variable")

# Identifier les valeurs en dehors des plages normatives
values_out_of_range <- data_joined %>%
  filter(Valeur < min | Valeur > max) %>%
  filter(Sujet=='A') %>%
  select(Sujet, Date, Categorie, variable, Valeur, min, max, unite)

# Afficher ou sauvegarder les résultats
print(values_out_of_range)

### 

# Joindre les dataframes pour comparer les valeurs
data_joined <- left_join(data_num, range_value, by = "variable") %>%
  mutate(Ecart = case_when(
    Valeur < min ~ min - Valeur,
    Valeur > max ~ Valeur - max,
    TRUE ~ 0  # Pas d'écart si la valeur est dans les normes
  ))

# Trier par Sujet, variable et Date
data_sorted <- data_joined %>%
  arrange(Sujet, variable, Date)

##
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
value_comparison2 <- value_comparison %>%
  mutate(CloserToMin_First = abs(First_Value - Min),
         CloserToMax_First = abs(Max - First_Value),
         CloserToMin_Last = abs(Last_Value - Min),
         CloserToMax_Last = abs(Max - Last_Value),
         Rapprochement = (CloserToMin_Last < CloserToMin_First | CloserToMax_Last < CloserToMax_First),
         Eloignement = (CloserToMin_Last > CloserToMin_First | CloserToMax_Last > CloserToMax_First))

# Filtrer pour obtenir les variables qui se sont rapprochées des normes
variables_approaching_norms <- filter(value_comparison, Rapprochement) %>%
  select(Sujet, Variable = variable, First_Value, Last_Value, Min, Max)

# Filtrer pour obtenir les variables qui se sont éloignées des normes
variables_receding_from_norms <- filter(value_comparison, Eloignement) %>%
  select(Sujet, Variable = variable, First_Value, Last_Value, Min, Max)

# Afficher ou sauvegarder les résultats
print("Variables s'étant rapprochées des normes :")
print(variables_approaching_norms)

print("Variables s'étant éloignées des normes :")
print(variables_receding_from_norms)
