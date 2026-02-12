# ==============================================================================
# DÉTECTION D'AMORCES ADN DANS LES SÉQUENCES
# ==============================================================================
# 
# Ce fichier contient les fonctions pour détecter des amorces spécifiques
# dans les séquences ADN, basé sur la logique du projet GitHub.
# Tolérance de 2 erreurs par occurrence pour gérer les variations.
#
# ==============================================================================

# Charger les bibliothèques nécessaires
if (!requireNamespace("stringr", quietly = TRUE)) {
  # Si stringr n'est pas disponible, utiliser les fonctions de base R
  # Les fonctions utilisent uniquement strsplit qui est dans base R
}

#' Détecter le nombre d'occurrences d'une amorce dans une séquence ADN
#'
#' @param adn_seq Séquence ADN (chaîne de caractères)
#' @param primer Amorce à rechercher (chaîne de caractères)
#' @param tolerance Nombre d'erreurs tolérées par occurrence (défaut: 2)
#' @return Nombre d'occurrences détectées
#' @export
count_primer <- function(adn_seq, primer, tolerance = 2) {
  # Ici, je détecte le nombre d'occurrences d'une amorce dans une séquence ADN
  # avec une tolérance aux erreurs (mutations, variations)
  
  # Gérer les valeurs manquantes
  if (is.na(adn_seq) || is.null(adn_seq)) {
    return(0)
  }
  
  # Convertir en minuscules et en chaîne de caractères
  adn_seq <- tolower(as.character(adn_seq))
  
  # Vérifier que la séquence est assez longue
  if (nchar(adn_seq) < nchar(primer)) {
    return(0)
  }
  
  # Convertir la séquence et l'amorce en vecteurs de caractères
  adn_vec <- strsplit(adn_seq, "")[[1]]
  primer_vec <- strsplit(primer, "")[[1]]
  
  primer_length <- length(primer_vec)
  nb_occurrences <- 0
  
  # Parcourir la séquence avec une fenêtre glissante
  for (i in 1:(length(adn_vec) - primer_length + 1)) {
    # Extraire la sous-séquence de la longueur de l'amorce
    sub_seq <- adn_vec[i:(i + primer_length - 1)]
    
    # Compter le nombre d'erreurs (caractères différents)
    errors <- sum(sub_seq != primer_vec)
    
    # Si le nombre d'erreurs est inférieur ou égal à la tolérance,
    # on considère qu'on a trouvé une occurrence
    if (errors <= tolerance) {
      nb_occurrences <- nb_occurrences + 1
    }
  }
  
  return(nb_occurrences)
}

#' Détecter toutes les amorces dans une séquence ADN
#'
#' @param adn_seq Séquence ADN (chaîne de caractères)
#' @param primers Vecteur d'amorces à rechercher
#' @param tolerance Nombre d'erreurs tolérées par occurrence (défaut: 2)
#' @return Dataframe avec une colonne par amorce contenant le nombre d'occurrences
#' @export
detect_all_primers <- function(adn_seq, primers, tolerance = 2) {
  # Ici, je détecte toutes les amorces dans une séquence ADN
  # et retourne un dataframe avec le nombre d'occurrences pour chaque amorce
  
  results <- data.frame()
  
  for (primer in primers) {
    col_name <- paste0("nb_", primer)
    count <- count_primer(adn_seq, primer, tolerance)
    results[[col_name]] <- count
  }
  
  return(results)
}

#' Détecter toutes les amorces dans un dataframe de séquences
#'
#' @param data Dataframe contenant une colonne `sequence_adn`
#' @param primers Vecteur d'amorces à rechercher (défaut: amorces du projet)
#' @param tolerance Nombre d'erreurs tolérées par occurrence (défaut: 2)
#' @return Dataframe avec les colonnes originales + colonnes `nb_<amorce>` pour chaque amorce
#' @export
detect_primers_in_dataframe <- function(data, primers = NULL, tolerance = 2) {
  # Ici, je détecte toutes les amorces dans toutes les séquences d'un dataframe
  # et j'ajoute les résultats comme nouvelles colonnes
  
  # Amorces par défaut du projet GitHub
  if (is.null(primers)) {
    primers <- c(
      "gggccc",
      "acctcca",
      "tttttta",
      "gggacggg",
      "atatatat",
      "gtacacgt"
    )
  }
  
  # Vérifier que la colonne sequence_adn existe
  if (!"sequence_adn" %in% names(data)) {
    warning("La colonne 'sequence_adn' n'existe pas dans les données.")
    return(data)
  }
  
  # Créer un dataframe de résultats avec les colonnes d'amorces
  results <- data
  
  # Pour chaque amorce, créer une colonne avec le nombre d'occurrences
  # Optimisation : utiliser vapply au lieu de sapply pour de meilleures performances
  for (primer in primers) {
    col_name <- paste0("nb_", primer)
    
    # Appliquer count_primer à toutes les séquences avec gestion d'erreur
    # vapply est plus rapide que sapply car il pré-alloue le vecteur de résultats
    results[[col_name]] <- vapply(
      data$sequence_adn,
      function(seq) {
        tryCatch({
          count_primer(seq, primer, tolerance)
        }, error = function(e) {
          warning("Erreur lors de la détection de l'amorce ", primer, " : ", e$message)
          return(0)
        })
      },
      FUN.VALUE = integer(1)
    )
  }
  
  return(results)
}

#' Obtenir la liste des amorces par défaut du projet
#'
#' @return Vecteur de chaînes de caractères avec les amorces
#' @export
get_default_primers <- function() {
  # Ici, je retourne la liste des amorces utilisées dans le projet GitHub
  c(
    "gggccc",
    "acctcca",
    "tttttta",
    "gggacggg",
    "atatatat",
    "gtacacgt"
  )
}

#' Calculer les statistiques des amorces détectées
#'
#' @param data Dataframe avec colonnes `nb_<amorce>`
#' @param primers Vecteur d'amorces (optionnel, pour filtrer les colonnes)
#' @return Dataframe avec statistiques par amorce (total, moyenne, médiane, etc.)
#' @export
calculate_primer_stats <- function(data, primers = NULL) {
  # Ici, je calcule les statistiques pour chaque amorce détectée
  
  if (is.null(primers)) {
    primers <- get_default_primers()
  }
  
  # Filtrer les colonnes d'amorces
  primer_cols <- paste0("nb_", primers)
  primer_cols <- primer_cols[primer_cols %in% names(data)]
  
  if (length(primer_cols) == 0) {
    return(data.frame())
  }
  
  # Calculer les statistiques pour chaque amorce
  stats_list <- lapply(primer_cols, function(col) {
    values <- data[[col]]
    data.frame(
      amorce = gsub("nb_", "", col),
      total_occurrences = sum(values, na.rm = TRUE),
      moyenne = mean(values, na.rm = TRUE),
      mediane = median(values, na.rm = TRUE),
      ecart_type = sd(values, na.rm = TRUE),
      min = min(values, na.rm = TRUE),
      max = max(values, na.rm = TRUE),
      presence_pct = mean(values > 0, na.rm = TRUE) * 100,
      stringsAsFactors = FALSE
    )
  })
  
  # Combiner tous les résultats
  stats_df <- do.call(rbind, stats_list)
  
  return(stats_df)
}
