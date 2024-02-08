gs4_auth(email = "kilian.bertholon02@gmail.com", cache = ".secrets")




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

Categorie <- unique(data_num$Categorie)[-c(1, 2)]


#Variable de sélection 
Sujet <- unique(data_num$Sujet)
date <- unique(data_num$Date)

Variables <- unique(data$Variable)

lire_tableau <- function() {
  datatable(
    data,
    editable = TRUE,
    filter = list(position = 'top', clear = TRUE),
    options = list(
      pageLength = 50,
      lengthMenu = c(50, 100, 150, 200),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#5e5a58', 'color': '#fff'});",
        "}"
      )
    )
  )
}
