source(file = "Import_data.R")

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
