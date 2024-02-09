
################################# Range ######################################

# Lire les données
sheet_range <- "https://docs.google.com/spreadsheets/d/1ykAvHxprWWsUDyPJX5gTMTYV6vPJUm4dX-kc68UgiDw/edit?usp=sharing"
range_value <- read_sheet(sheet_range)

data_num <- read_sheet(sheet_tidy)


data_num$Valeur <- gsub(",", ".", data_num$Valeur)
data_num$Valeur <- gsub("[<>]", "", data_num$Valeur)
data_num$Valeur <- as.numeric(data_num$Valeur)
data_num$Valeur <- round(data_num$Valeur, 3)
data_num$Date <- as.Date(data_num$Date, format = "%d/%m/%Y")

# Joindre les dataframes pour comparer les valeurs
data_joined <- left_join(data_num, range_value, by = "Variable")




################################ Radar ######################################
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


############################# Tidy #########################################
#Import data + traitement
gs4_deauth()
sheet_tidy <- "https://docs.google.com/spreadsheets/d/1jty0iGJY-FLLkEQHKyRe12qRD9MLZzEQd2TXjclGemM/edit#gid=0"
data <- read_sheet(sheet_tidy)

#Traitement 
## Date
data$Date <- format(data$Date, "%e/%m/%Y")

#Data num (sans <,>)
data_num <- copy(data)
data_num$Valeur <- gsub(",", ".", data_num$Valeur)
data_num$Valeur <- gsub("[<>]", "", data_num$Valeur)
data_num$Valeur <- as.numeric(data_num$Valeur)
data_num$Valeur <- round(data_num$Valeur, 3)
data_num$Date <- as.Date(data_num$Date, format = "%d/%m/%Y")