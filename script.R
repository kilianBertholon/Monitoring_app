#Import data + traitement
data <- read_excel("/Users/mathieubourgeois/Documents/GitHub/Monitoring_app/data/data_tidy.xlsx")

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
