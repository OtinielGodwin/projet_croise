# ==============================================================================
# ANALYSE TEMPORELLE DES DONNÉES
# ==============================================================================
# 
# Fonctions pour analyser la dimension temporelle des données,
# notamment les séries temporelles par pays et trimestre.
#
# ==============================================================================

#' Préparer les données pour l'analyse temporelle
#'
#' @param data Dataframe avec colonnes temporelles
#' @param date_col Nom de la colonne de date/trimestre (défaut: "trimestre")
#' @param value_col Nom de la colonne de valeur à analyser
#' @param country_col Nom de la colonne pays (défaut: "pays")
#' @param country Filtrer par pays spécifique (NULL pour tous)
#' @return Dataframe préparé pour analyse temporelle
#' @export
prepare_temporal_data <- function(data, date_col = "trimestre", value_col, country_col = "pays", country = NULL) {
  # Ici, je prépare les données pour l'analyse temporelle
  # en groupant par date/trimestre et pays si nécessaire
  
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }
  
  # Vérifier que les colonnes existent
  if (!date_col %in% names(data)) {
    warning(paste("La colonne", date_col, "n'existe pas dans les données."))
    return(data.frame())
  }
  
  if (!value_col %in% names(data)) {
    warning(paste("La colonne", value_col, "n'existe pas dans les données."))
    return(data.frame())
  }
  
  # Filtrer par pays si spécifié
  if (!is.null(country) && country_col %in% names(data)) {
    data <- data[data[[country_col]] == country, ]
  }
  
  # Créer une colonne de date ordonnée si c'est un trimestre
  if (date_col == "trimestre" || all(grepl("^Q[1-4]", data[[date_col]], ignore.case = TRUE))) {
    # Extraire année et trimestre pour créer une date ordonnée
    if ("annee" %in% names(data)) {
      data$date_ordered <- paste0(data$annee, "-", data[[date_col]])
    } else {
      data$date_ordered <- data[[date_col]]
    }
  } else {
    data$date_ordered <- data[[date_col]]
  }
  
  # Grouper par date et pays (si pays spécifié)
  if (!is.null(country) && country_col %in% names(data)) {
    temporal_data <- data %>%
      group_by(.data[[date_col]], .data[[country_col]]) %>%
      summarise(
        valeur = mean(.data[[value_col]], na.rm = TRUE),
        count = n(),
        .groups = 'drop'
      ) %>%
      arrange(.data[[date_col]])
  } else if (country_col %in% names(data)) {
    # Grouper par date et pays
    temporal_data <- data %>%
      group_by(.data[[date_col]], .data[[country_col]]) %>%
      summarise(
        valeur = mean(.data[[value_col]], na.rm = TRUE),
        count = n(),
        .groups = 'drop'
      ) %>%
      arrange(.data[[date_col]], .data[[country_col]])
  } else {
    # Grouper seulement par date
    temporal_data <- data %>%
      group_by(.data[[date_col]]) %>%
      summarise(
        valeur = mean(.data[[value_col]], na.rm = TRUE),
        count = n(),
        .groups = 'drop'
      ) %>%
      arrange(.data[[date_col]])
  }
  
  return(temporal_data)
}

#' Calculer les tendances temporelles
#'
#' @param temporal_data Dataframe préparé avec colonnes date et valeur
#' @param date_col Nom de la colonne de date
#' @param value_col Nom de la colonne de valeur
#' @return Liste avec tendance (croissance/décroissance) et pente
#' @export
calculate_trends <- function(temporal_data, date_col = "trimestre", value_col = "valeur") {
  # Ici, je calcule les tendances temporelles (croissance ou décroissance)
  
  if (is.null(temporal_data) || nrow(temporal_data) < 2) {
    return(list(trend = "insuffisant", slope = 0, p_value = NA))
  }
  
  # Créer un index numérique pour la régression
  temporal_data$index <- 1:nrow(temporal_data)
  
  # Régression linéaire simple pour détecter la tendance
  model <- lm(temporal_data[[value_col]] ~ temporal_data$index, data = temporal_data)
  
  slope <- coef(model)[2]
  p_value <- summary(model)$coefficients[2, 4]
  
  # Déterminer la tendance
  if (p_value < 0.05) {
    if (slope > 0) {
      trend <- "croissance"
    } else {
      trend <- "décroissance"
    }
  } else {
    trend <- "stable"
  }
  
  return(list(
    trend = trend,
    slope = as.numeric(slope),
    p_value = p_value,
    r_squared = summary(model)$r.squared
  ))
}

#' Créer une série temporelle
#'
#' @param data Dataframe avec colonnes temporelles
#' @param date_col Nom de la colonne de date
#' @param value_col Nom de la colonne de valeur
#' @param frequency Fréquence (4 pour trimestriel, 12 pour mensuel)
#' @return Objet ts (time series)
#' @export
create_time_series <- function(data, date_col = "trimestre", value_col, frequency = 4) {
  # Ici, je crée un objet série temporelle pour analyses avancées
  
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }
  
  # Trier par date
  data_sorted <- data %>%
    arrange(.data[[date_col]])
  
  # Extraire les valeurs
  values <- data_sorted[[value_col]]
  values <- values[!is.na(values)]
  
  if (length(values) == 0) {
    return(NULL)
  }
  
  # Créer la série temporelle
  ts_data <- ts(values, frequency = frequency)
  
  return(ts_data)
}

#' Calculer les statistiques par trimestre
#'
#' @param data Dataframe avec colonne trimestre
#' @param value_col Nom de la colonne de valeur
#' @param country_col Nom de la colonne pays (optionnel)
#' @return Dataframe avec statistiques par trimestre
#' @export
calculate_quarterly_stats <- function(data, value_col, country_col = NULL) {
  # Ici, je calcule les statistiques (moyenne, médiane, etc.) par trimestre
  
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }
  
  if (!"trimestre" %in% names(data)) {
    warning("La colonne 'trimestre' n'existe pas.")
    return(data.frame())
  }
  
  if (!value_col %in% names(data)) {
    warning(paste("La colonne", value_col, "n'existe pas."))
    return(data.frame())
  }
  
  # Grouper par trimestre (et pays si spécifié)
  if (!is.null(country_col) && country_col %in% names(data)) {
    stats <- data %>%
      group_by(trimestre, .data[[country_col]]) %>%
      summarise(
        moyenne = mean(.data[[value_col]], na.rm = TRUE),
        mediane = median(.data[[value_col]], na.rm = TRUE),
        ecart_type = sd(.data[[value_col]], na.rm = TRUE),
        min = min(.data[[value_col]], na.rm = TRUE),
        max = max(.data[[value_col]], na.rm = TRUE),
        count = n(),
        .groups = 'drop'
      ) %>%
      arrange(trimestre, .data[[country_col]])
  } else {
    stats <- data %>%
      group_by(trimestre) %>%
      summarise(
        moyenne = mean(.data[[value_col]], na.rm = TRUE),
        mediane = median(.data[[value_col]], na.rm = TRUE),
        ecart_type = sd(.data[[value_col]], na.rm = TRUE),
        min = min(.data[[value_col]], na.rm = TRUE),
        max = max(.data[[value_col]], na.rm = TRUE),
        count = n(),
        .groups = 'drop'
      ) %>%
      arrange(trimestre)
  }
  
  return(stats)
}

#' Comparer deux trimestres
#'
#' @param data Dataframe avec colonne trimestre
#' @param value_col Nom de la colonne de valeur
#' @param quarter1 Premier trimestre à comparer (ex: "Q1")
#' @param quarter2 Deuxième trimestre à comparer (ex: "Q2")
#' @return Liste avec statistiques de comparaison
#' @export
compare_quarters <- function(data, value_col, quarter1, quarter2) {
  # Ici, je compare deux trimestres pour voir l'évolution
  
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }
  
  if (!"trimestre" %in% names(data)) {
    return(NULL)
  }
  
  # Filtrer les données pour chaque trimestre
  q1_data <- data[data$trimestre == quarter1, value_col]
  q2_data <- data[data$trimestre == quarter2, value_col]
  
  q1_data <- q1_data[!is.na(q1_data)]
  q2_data <- q2_data[!is.na(q2_data)]
  
  if (length(q1_data) == 0 || length(q2_data) == 0) {
    return(NULL)
  }
  
  # Calculer les statistiques
  q1_mean <- mean(q1_data)
  q2_mean <- mean(q2_data)
  
  # Test statistique (t-test)
  test_result <- tryCatch({
    t.test(q1_data, q2_data)
  }, error = function(e) {
    return(NULL)
  })
  
  # Calculer la différence relative
  diff_abs <- q2_mean - q1_mean
  diff_pct <- (diff_abs / q1_mean) * 100
  
  return(list(
    quarter1 = quarter1,
    quarter2 = quarter2,
    q1_mean = q1_mean,
    q2_mean = q2_mean,
    diff_abs = diff_abs,
    diff_pct = diff_pct,
    p_value = if (!is.null(test_result)) test_result$p.value else NA,
    significant = if (!is.null(test_result) && test_result$p.value < 0.05) TRUE else FALSE
  ))
}
