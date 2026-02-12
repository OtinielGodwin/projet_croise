# ==============================================================================
# MODÈLE AUTORÉGRESSIF (AR) POUR PRÉDICTION TEMPORELLE
# ==============================================================================
# 
# Fonctions pour ajuster un modèle AR(1) et prédire les cas trimestriels
# selon la demande d'Arthur Loone : "prédire le nombre de cas d'un trimestre
# sur l'autre, pays par pays ou au niveau de l'europe"
#
# ==============================================================================

#' Calculer le nombre de cas par trimestre
#'
#' @param data Dataframe avec colonnes temporelles
#' @param id_col Nom de la colonne d'identifiant (défaut: "id_global")
#' @param date_col Nom de la colonne de date/trimestre (défaut: "trimestre")
#' @param country_col Nom de la colonne pays (défaut: "pays")
#' @param country Filtrer par pays spécifique (NULL pour tous = niveau européen)
#' @return Dataframe avec nombre de cas par trimestre
#' @export
calculate_quarterly_cases <- function(data, id_col = "id_global", date_col = "trimestre", country_col = "pays", country = NULL) {
  # Ici, je calcule le nombre de cas (IDs uniques) par trimestre
  # Pour le niveau européen, je compte tous les cas sans distinction de pays
  
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }
  
  # Vérifier que les colonnes existent
  if (!id_col %in% names(data)) {
    warning(paste("La colonne", id_col, "n'existe pas dans les données."))
    return(data.frame())
  }
  
  if (!date_col %in% names(data)) {
    warning(paste("La colonne", date_col, "n'existe pas dans les données."))
    return(data.frame())
  }
  
  # Filtrer par pays si spécifié
  if (!is.null(country) && country_col %in% names(data)) {
    data <- data[data[[country_col]] == country, ]
  }
  
  # Ajouter année si disponible pour trier correctement
  if ("annee" %in% names(data)) {
    # Créer une colonne de tri
    data$date_sort <- paste0(data$annee, "-", data[[date_col]])
    
    # Compter les cas uniques par trimestre
    cases <- data %>%
      group_by(.data[[date_col]], annee) %>%
      summarise(
        cas = n_distinct(.data[[id_col]], na.rm = TRUE),
        date_sort = first(date_sort),
        .groups = 'drop'
      ) %>%
      arrange(annee, .data[[date_col]])
  } else {
    # Compter les cas uniques par trimestre sans année
    cases <- data %>%
      group_by(.data[[date_col]]) %>%
      summarise(
        cas = n_distinct(.data[[id_col]], na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(.data[[date_col]])
  }
  
  return(cases)
}

#' Ajuster un modèle AR(1)
#'
#' @param time_series Vecteur numérique avec les valeurs temporelles
#' @return Liste avec le modèle et les statistiques
#' @export
fit_ar_model <- function(time_series) {
  # Ici, j'ajuste un modèle AR(1) : cas_t = α + β * cas_{t-1} + ε
  # Le modèle prédit le trimestre suivant basé sur le trimestre précédent
  
  if (is.null(time_series) || length(time_series) < 2) {
    return(NULL)
  }
  
  # Retirer les valeurs manquantes
  time_series <- time_series[!is.na(time_series)]
  
  if (length(time_series) < 2) {
    return(NULL)
  }
  
  # Créer les variables décalées pour AR(1)
  # cas_t dépend de cas_{t-1}
  n <- length(time_series)
  
  if (n < 2) {
    return(NULL)
  }
  
  # Variables : y_t = cas_t, x_t = cas_{t-1}
  y <- time_series[2:n]  # Valeurs actuelles
  x <- time_series[1:(n-1)]  # Valeurs précédentes
  
  # Ajuster le modèle linéaire : y = α + β*x + ε
  model <- lm(y ~ x)
  
  # Extraire les coefficients
  alpha <- coef(model)[1]  # Constante
  beta <- coef(model)[2]   # Coefficient autorégressif
  
  # Statistiques du modèle
  summary_model <- summary(model)
  r_squared <- summary_model$r.squared
  p_value <- summary_model$coefficients[2, 4]
  
  # Prédictions sur les données d'entraînement
  fitted_values <- fitted(model)
  
  # Résidus
  residuals <- residuals(model)
  
  return(list(
    model = model,
    alpha = as.numeric(alpha),
    beta = as.numeric(beta),
    r_squared = r_squared,
    p_value = p_value,
    fitted_values = fitted_values,
    residuals = residuals,
    actual_values = y,
    lagged_values = x
  ))
}

#' Prédire le trimestre suivant
#'
#' @param ar_result Résultat de fit_ar_model()
#' @param last_value Dernière valeur observée (cas du trimestre précédent)
#' @return Valeur prédite pour le trimestre suivant
#' @export
predict_next_quarter <- function(ar_result, last_value) {
  # Ici, je prédit le trimestre suivant avec le modèle AR(1)
  # Formule : cas_t = α + β * cas_{t-1}
  
  if (is.null(ar_result)) {
    return(NA)
  }
  
  if (is.na(last_value)) {
    return(NA)
  }
  
  # Prédiction : cas_t = α + β * cas_{t-1}
  prediction <- ar_result$alpha + ar_result$beta * last_value
  
  return(prediction)
}

#' Calculer les métriques de performance
#'
#' @param actual Valeurs réelles
#' @param predicted Valeurs prédites
#' @return Liste avec métriques (RMSE, MAE, R², MAPE)
#' @export
calculate_ar_metrics <- function(actual, predicted) {
  # Ici, je calcule les métriques de performance du modèle AR
  
  if (is.null(actual) || is.null(predicted)) {
    return(NULL)
  }
  
  # Retirer les valeurs manquantes
  valid <- !is.na(actual) & !is.na(predicted)
  actual <- actual[valid]
  predicted <- predicted[valid]
  
  if (length(actual) == 0 || length(predicted) == 0) {
    return(NULL)
  }
  
  # RMSE (Root Mean Squared Error)
  rmse <- sqrt(mean((actual - predicted)^2))
  
  # MAE (Mean Absolute Error)
  mae <- mean(abs(actual - predicted))
  
  # R² (Coefficient de détermination)
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  
  # MAPE (Mean Absolute Percentage Error)
  mape <- mean(abs((actual - predicted) / actual)) * 100
  
  return(list(
    rmse = rmse,
    mae = mae,
    r_squared = r_squared,
    mape = mape
  ))
}

#' Valider le modèle sur de nouvelles données
#'
#' @param model Résultat de fit_ar_model()
#' @param new_data Nouvelles données pour validation
#' @return Liste avec métriques de validation
#' @export
validate_ar_model <- function(model, new_data) {
  # Ici, je valide le modèle AR sur de nouvelles données
  # comme demandé par Arthur Loone : "vous pourrez tester votre modèle sur ces nouvelles données"
  
  if (is.null(model) || is.null(new_data)) {
    return(NULL)
  }
  
  if (length(new_data) < 2) {
    return(NULL)
  }
  
  # Retirer les valeurs manquantes
  new_data <- new_data[!is.na(new_data)]
  
  if (length(new_data) < 2) {
    return(NULL)
  }
  
  # Prédire avec le modèle
  predictions <- c()
  actual_values <- c()
  
  # Pour chaque valeur, prédire la suivante
  for (i in 1:(length(new_data) - 1)) {
    pred <- predict_next_quarter(model, new_data[i])
    predictions <- c(predictions, pred)
    actual_values <- c(actual_values, new_data[i + 1])
  }
  
  # Calculer les métriques
  metrics <- calculate_ar_metrics(actual_values, predictions)
  
  return(list(
    predictions = predictions,
    actual_values = actual_values,
    metrics = metrics
  ))
}
