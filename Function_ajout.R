ajout_utili <- function(file_path) {
  tryCatch({
    # Charger les données du fichier fourni par l'utilisateur
    file_extension <- tools::file_ext(file_path)
    
    if (file_extension %in% c("xlsx", "xls")) {
      data_ext <- read_excel(file_path)
    } else if (file_extension == "csv") {
      data_ext <- read_csv2(file_path)
    } else if (file_extension == "ods") {
      data_ext <- read_ods(file_path)
    } else {
      stop("Type de fichier non supporté")
    }
    
    # Générer un nom de fichier temporaire
    tmp_file <- tempfile(fileext = ".csv")
    
    # Écrire le dataframe dans le fichier temporaire
    write.csv(data_ext, tmp_file, row.names = FALSE)
    
    tmp_file_path <- normalizePath(tmp_file)
    
    
    return(tmp_file)
  }, error = function(e) {
    message("Une erreur s'est produite : ", e$message)
    return(NULL)
  })
}