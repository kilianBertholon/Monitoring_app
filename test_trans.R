data <- read_excel("data/Données_Suivi_Bio_Etudiant.xlsx", col_names  = FALSE, na="")

library(dplyr)

#Supprimer la colonne d'unité
data <- data %>%
  select(-2)


#Mettre la ligne sujet en référence 
reference <- data[1,] |> select(-1)
date <- data[2, ] |> select(-1)

data_prov <- data[-c(1:2), ]

new_data <- data.frame()

for (i in 1:ncol(reference)) {
  new_data <- data.frame(Sujet = rep(reference[, i], nrow(data_prov)),
                         Date = rep(date[, i], nrow(data_prov)))

}
