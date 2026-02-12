# ==============================================================================
# FONCTIONS UTILITAIRES
# ==============================================================================
# 
# Ici, je regroupe les fonctions utilitaires pour la validation,
# le formatage et les opérations communes sur les données.
#
# ==============================================================================

#' Vérifier si un fichier existe et afficher un message d'erreur si nécessaire
#'
#' @param file_path Chemin vers le fichier
#' @param file_name Nom du fichier (pour le message d'erreur)
#' @return TRUE si le fichier existe, arrête l'exécution sinon
check_file_exists <- function(file_path, file_name) {
  if (!file.exists(file_path)) {
    stop(paste("Le fichier", file_name, "n'existe pas à l'emplacement :", file_path))
  }
  return(TRUE)
}

#' Convertir une colonne en format date si elle existe
#'
#' @param data Dataframe à modifier
#' @param col_name Nom de la colonne à convertir
#' @return Dataframe avec la colonne convertie en date
convert_to_date <- function(data, col_name) {
  if (col_name %in% names(data)) {
    # J'essaie de convertir en date, en gérant différents formats
    data[[col_name]] <- as.Date(data[[col_name]], tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d"))
  }
  return(data)
}

#' Vérifier la présence de colonnes essentielles dans un dataframe
#'
#' @param data Dataframe à vérifier
#' @param required_cols Vecteur de noms de colonnes requises
#' @param data_name Nom du dataset (pour le message d'erreur)
#' @return TRUE si toutes les colonnes sont présentes
check_required_columns <- function(data, required_cols, data_name) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    warning(paste("Colonnes manquantes dans", data_name, ":", paste(missing_cols, collapse = ", ")))
    return(FALSE)
  }
  return(TRUE)
}

#' Formater une valeur numérique pour l'affichage
#'
#' @param value Valeur à formater
#' @param digits Nombre de décimales
#' @return Valeur formatée en chaîne de caractères
format_number <- function(value, digits = 2) {
  if (is.null(value) || is.na(value)) {
    return("N/A")
  }
  return(format(round(value, digits), nsmall = digits))
}

#' Formater une valeur de pourcentage
#'
#' @param value Valeur à formater (entre 0 et 1)
#' @param digits Nombre de décimales
#' @return Valeur formatée en pourcentage
format_percentage <- function(value, digits = 1) {
  if (is.null(value) || is.na(value)) {
    return("N/A")
  }
  return(paste0(format_number(value * 100, digits), "%"))
}
