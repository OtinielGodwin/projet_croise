# ==============================================================================
# MODULE MODÈLES SUPERVISÉS - RÉGRESSION
# ==============================================================================
# 
# Module Shiny pour la prédiction de marqueurs du cancer
# à partir des variables génétiques et comportementales
# Méthodes : régression linéaire multiple, LASSO, Ridge
#
# ==============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(glmnet)

# À ce niveau, je charge theme_shiny.R avant theme_ggplot.R car
# theme_ggplot.R utilise la fonction get_theme_colors() définie dans theme_shiny.R
source("R/theme_shiny.R")
source("R/theme_ggplot.R")
source("R/constants.R")

# ==============================================================================
# UI DU MODULE
# ==============================================================================

#' Interface utilisateur du module régression
#'
#' @param id Identifiant unique du module
#' @return Interface utilisateur du module
mod_regression_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # Titre de la page
    div(
      class = "container-fluid",
      style = "margin-bottom: 2rem;",
      h1(
        style = "color: #1E5A8E; margin-bottom: 0.5rem;",
        icon("chart-line", style = "margin-right: 0.5rem;"),
        "Modèles Supervisés - Prédiction des Marqueurs"
      ),
      p(
        style = "color: #6C757D; font-size: 1.1rem;",
        "Prédiction des marqueurs du cancer à partir des variables génétiques et comportementales"
      ),
      div(
        style = "background-color: #E7F3FF; border-left: 4px solid #1E5A8E; padding: 1rem; margin-top: 1rem; border-radius: 0.375rem;",
        p(
          style = "margin: 0; color: #0C5460; font-size: 0.95rem;",
          icon("info-circle", style = "margin-right: 0.5rem;"),
          tags$strong("Méthode :"),
          " Les modèles supervisés apprennent à prédire une variable cible à partir",
          " de variables explicatives. Trois méthodes sont disponibles : régression linéaire",
          " multiple (simple et interprétable), LASSO (sélection de variables),",
          " et Ridge (régularisation)."
        )
      )
    ),
    
    # Panneau de configuration
    div(
      class = "container-fluid",
      style = "margin-bottom: 2rem;",
      
      div(
        class = "card",
        
        div(
          class = "card-header",
          style = "background-color: #F8F9FA; border-bottom: 2px solid #1E5A8E;",
          h3(
            style = "margin: 0; color: #1E5A8E; font-size: 1.25rem;",
            icon("cog", style = "margin-right: 0.5rem;"),
            "Configuration du modèle"
          )
        ),
        
        div(
          class = "card-body",
          style = "padding: 1.5rem;",
          
          fluidRow(
            # Sélection du dataset
            column(
              width = 3,
              selectInput(
                ns("dataset"),
                label = tags$strong("Source de données"),
                choices = list(
                  "Table analytique complète" = "analytics",
                  "Données génétiques" = "genetic",
                  "Données comportementales" = "behavioral",
                  "Marqueurs du cancer" = "cancer_markers"
                ),
                selected = "analytics"
              )
            ),
            
            # Variable à prédire
            column(
              width = 3,
              uiOutput(ns("target_variable_selector"))
            ),
            
            # Méthode de régression
            column(
              width = 3,
              selectInput(
                ns("regression_method"),
                label = tags$strong("Méthode de régression"),
                choices = list(
                  "Régression linéaire multiple" = "lm",
                  "LASSO" = "lasso",
                  "Ridge" = "ridge"
                ),
                selected = "lm"
              )
            ),
            
            # Paramètre lambda pour régularisation
            column(
              width = 3,
              uiOutput(ns("lambda_selector"))
            )
          ),
          
          # Sélection des variables explicatives
          fluidRow(
            column(
              width = 12,
              style = "margin-top: 1rem;",
              uiOutput(ns("predictor_variables_selector"))
            )
          ),
          
          # Filtres
          fluidRow(
            column(
              width = 6,
              style = "margin-top: 1rem;",
              uiOutput(ns("country_selector"))
            ),
            column(
              width = 6,
              style = "margin-top: 1rem;",
              uiOutput(ns("date_range_selector"))
            )
          )
        )
      )
    ),
    
    # Métriques du modèle
    div(
      class = "container-fluid",
      style = "margin-bottom: 2rem;",
      
      div(
        class = "card",
        
        div(
          class = "card-header",
          style = "background-color: #F8F9FA; border-bottom: 2px solid #1E5A8E;",
          h3(
            style = "margin: 0; color: #1E5A8E; font-size: 1.25rem;",
            icon("calculator", style = "margin-right: 0.5rem;"),
            "Métriques du Modèle"
          )
        ),
        
        div(
          class = "card-body",
          style = "padding: 1.5rem;",
          
          DT::dataTableOutput(ns("metrics_table"), width = "100%")
        )
      )
    ),
    
    # Coefficients et importance
    div(
      class = "container-fluid",
      style = "margin-bottom: 2rem;",
      
      fluidRow(
        # Tableau des coefficients
        column(
          width = 6,
          div(
            class = "card",
            style = "height: 100%;",
            
            div(
              class = "card-header",
              style = "background-color: #F8F9FA; border-bottom: 2px solid #1E5A8E;",
              h3(
                style = "margin: 0; color: #1E5A8E; font-size: 1.25rem;",
                icon("table", style = "margin-right: 0.5rem;"),
                "Coefficients du Modèle"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              DT::dataTableOutput(ns("coefficients_table"), width = "100%")
            )
          )
        ),
        
        # Graphique d'importance
        column(
          width = 6,
          div(
            class = "card",
            style = "height: 100%;",
            
            div(
              class = "card-header",
              style = "background-color: #F8F9FA; border-bottom: 2px solid #1E5A8E;",
              h3(
                style = "margin: 0; color: #1E5A8E; font-size: 1.25rem;",
                icon("chart-bar", style = "margin-right: 0.5rem;"),
                "Importance des Variables"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              plotOutput(ns("importance_plot"), height = "400px")
            )
          )
        )
      )
    ),
    
    # Visualisations
    div(
      class = "container-fluid",
      
      fluidRow(
        # Prédictions vs Observations
        column(
          width = 6,
          div(
            class = "card",
            style = "height: 100%;",
            
            div(
              class = "card-header",
              style = "background-color: #F8F9FA; border-bottom: 2px solid #1E5A8E;",
              h3(
                style = "margin: 0; color: #1E5A8E; font-size: 1.25rem;",
                icon("chart-scatter", style = "margin-right: 0.5rem;"),
                "Prédictions vs Observations"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              plotOutput(ns("predictions_plot"), height = "400px")
            )
          )
        ),
        
        # Résidus
        column(
          width = 6,
          div(
            class = "card",
            style = "height: 100%;",
            
            div(
              class = "card-header",
              style = "background-color: #F8F9FA; border-bottom: 2px solid #1E5A8E;",
              h3(
                style = "margin: 0; color: #1E5A8E; font-size: 1.25rem;",
                icon("chart-area", style = "margin-right: 0.5rem;"),
                "Analyse des Résidus"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              plotOutput(ns("residuals_plot"), height = "400px")
            )
          )
        )
      )
    )
  )
}

# ==============================================================================
# SERVER DU MODULE
# ==============================================================================

#' Logique serveur du module régression
#'
#' @param id Identifiant unique du module
#' @param genetic_data Données génétiques filtrées (réactif)
#' @param smoking_data Données de tabagisme filtrées (réactif)
#' @param alcohol_data Données d'alcool filtrées (réactif)
#' @param cancer_markers_data Données des marqueurs filtrées (réactif)
#' @param analytics_data Données analytiques filtrées (réactif)
#' @param available_countries Liste des pays disponibles (réactif)
#' @param available_date_range Plage de dates disponible (réactif)
mod_regression_server <- function(
  id,
  genetic_data,
  smoking_data,
  alcohol_data,
  cancer_markers_data,
  analytics_data,
  available_countries,
  available_date_range
) {
  moduleServer(id, function(input, output, session) {
    
    # ==========================================================================
    # SÉLECTION DES DONNÉES SELON LE DATASET CHOISI
    # ==========================================================================
    
    # Ici, je récupère les données selon le dataset sélectionné par l'utilisateur.
    # Les données proviennent des CSV chargés au démarrage de l'application.
    selected_data <- reactive({
      switch(
        input$dataset,
        "genetic" = genetic_data(),
        "behavioral" = {
          merged <- merge(smoking_data(), alcohol_data(), by = COL_PATIENT_ID, all = TRUE)
          merged
        },
        "cancer_markers" = cancer_markers_data(),
        "analytics" = analytics_data()
      )
    })
    
    # ==========================================================================
    # IDENTIFICATION DES VARIABLES DISPONIBLES
    # ==========================================================================
    
    # À ce niveau, je détecte automatiquement toutes les variables quantitatives
    # disponibles dans le dataset sélectionné. Ces variables peuvent servir
    # soit de variable cible (à prédire) soit de variables explicatives.
    all_numeric_variables <- reactive({
      data <- selected_data()
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      numeric_cols <- sapply(data, is.numeric)
      numeric_cols <- names(data)[numeric_cols]
      
      exclude_cols <- c(COL_PATIENT_ID, COL_GENE_ID, COL_DATE, COL_YEAR)
      numeric_cols <- setdiff(numeric_cols, exclude_cols)
      
      return(numeric_cols)
    })
    
    # ==========================================================================
    # INTERFACE DYNAMIQUE POUR LA VARIABLE À PRÉDIRE
    # ==========================================================================
    
    output$target_variable_selector <- renderUI({
      ns <- session$ns
      vars <- all_numeric_variables()
      
      if (is.null(vars) || length(vars) == 0) {
        return(
          selectInput(
            ns("target_variable"),
            label = tags$strong("Variable à prédire"),
            choices = list("Aucune variable disponible" = ""),
            selected = ""
          )
        )
      }
      
      # Je privilégie les marqueurs du cancer si disponibles
      preferred_vars <- vars[grepl("ca15|marker|marqueur", vars, ignore.case = TRUE)]
      if (length(preferred_vars) > 0) {
        selected_var <- preferred_vars[1]
      } else {
        selected_var <- vars[1]
      }
      
      choices_list <- as.list(vars)
      names(choices_list) <- vars
      
      selectInput(
        ns("target_variable"),
        label = tags$strong("Variable à prédire"),
        choices = choices_list,
        selected = selected_var
      )
    })
    
    # ==========================================================================
    # INTERFACE DYNAMIQUE POUR LES VARIABLES EXPLICATIVES
    # ==========================================================================
    
    output$predictor_variables_selector <- renderUI({
      ns <- session$ns
      vars <- all_numeric_variables()
      target <- input$target_variable
      
      if (is.null(vars) || length(vars) == 0) {
        return(
          p(
            style = "color: #DC3545;",
            "Aucune variable quantitative disponible."
          )
        )
      }
      
      # Je retire la variable cible de la liste des prédicteurs
      predictor_vars <- setdiff(vars, target)
      
      if (length(predictor_vars) == 0) {
        return(
          p(
            style = "color: #DC3545;",
            "Aucune variable explicative disponible."
          )
        )
      }
      
      choices_list <- as.list(predictor_vars)
      names(choices_list) <- predictor_vars
      
      checkboxGroupInput(
        ns("predictor_variables"),
        label = tags$strong("Variables explicatives (sélectionnez au moins 1)"),
        choices = choices_list,
        selected = if (length(predictor_vars) <= 10) predictor_vars else predictor_vars[1:min(10, length(predictor_vars))]
      )
    })
    
    # ==========================================================================
    # INTERFACE DYNAMIQUE POUR LE PARAMÈTRE LAMBDA
    # ==========================================================================
    
    output$lambda_selector <- renderUI({
      ns <- session$ns
      
      if (input$regression_method %in% c("lasso", "ridge")) {
        sliderInput(
          ns("lambda"),
          label = tags$strong("Lambda (régularisation)"),
          min = 0.001,
          max = 1,
          value = 0.1,
          step = 0.001
        )
      } else {
        return(NULL)
      }
    })
    
    # ==========================================================================
    # INTERFACE DYNAMIQUE POUR LES FILTRES
    # ==========================================================================
    
    output$country_selector <- renderUI({
      ns <- session$ns
      countries <- available_countries()
      
      if (is.null(countries) || length(countries) == 0) {
        return(NULL)
      }
      
      selectInput(
        ns("country_filter"),
        label = tags$strong("Pays"),
        choices = c("Tous" = "", countries),
        selected = ""
      )
    })
    
    output$date_range_selector <- renderUI({
      ns <- session$ns
      date_range <- available_date_range()
      
      if (is.null(date_range)) {
        return(NULL)
      }
      
      dateRangeInput(
        ns("date_range"),
        label = tags$strong("Période"),
        start = date_range$start_date,
        end = date_range$end_date,
        min = date_range$start_date,
        max = date_range$end_date,
        language = "fr",
        separator = " à "
      )
    })
    
    # ==========================================================================
    # DONNÉES FILTRÉES SELON LES CRITÈRES
    # ==========================================================================
    
    # Ici, je filtre les données selon les critères sélectionnés
    filtered_data <- reactive({
      data <- selected_data()
      
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      # Je filtre par pays si sélectionné
      if (!is.null(input$country_filter) && input$country_filter != "") {
        if (COL_COUNTRY %in% names(data)) {
          data <- data[data[[COL_COUNTRY]] == input$country_filter, ]
        }
      }
      
      # Je filtre par période si sélectionnée
      if (!is.null(input$date_range) && length(input$date_range) == 2) {
        date_col <- NULL
        if (COL_DATE %in% names(data)) {
          date_col <- COL_DATE
        } else if (COL_PERIOD %in% names(data)) {
          date_col <- COL_PERIOD
        }
        
        if (!is.null(date_col)) {
          start_date <- as.Date(input$date_range[1])
          end_date <- as.Date(input$date_range[2])
          data <- data[data[[date_col]] >= start_date & data[[date_col]] <= end_date, ]
        }
      }
      
      return(data)
    })
    
    # ==========================================================================
    # PRÉPARATION DES DONNÉES POUR LA RÉGRESSION
    # ==========================================================================
    
    # Ici, je prépare les données avec la variable cible (à prédire) et les
    # variables explicatives sélectionnées. Les valeurs manquantes sont exclues
    # car les modèles nécessitent des données complètes.
    regression_data <- reactive({
      data <- filtered_data()
      target <- input$target_variable
      predictors <- input$predictor_variables
      
      if (is.null(data) || is.null(target) || target == "" || 
          is.null(predictors) || length(predictors) == 0) {
        return(NULL)
      }
      
      # Je vérifie que toutes les variables existent
      all_vars <- c(target, predictors)
      all_vars <- all_vars[all_vars %in% names(data)]
      
      if (length(all_vars) < 2) {
        return(NULL)
      }
      
      # Je sélectionne les variables nécessaires
      reg_data <- data[, all_vars, drop = FALSE]
      
      # Je retire les lignes avec des valeurs manquantes
      reg_data <- reg_data[complete.cases(reg_data), ]
      
      if (nrow(reg_data) < 2) {
        return(NULL)
      }
      
      return(reg_data)
    })
    
    # ==========================================================================
    # CALCUL DU MODÈLE DE RÉGRESSION
    # ==========================================================================
    
    # Ici, je calcule le modèle selon la méthode choisie :
    # - Régression linéaire multiple (lm) : simple et interprétable
    # - LASSO/Ridge : avec régularisation pour éviter le sur-apprentissage
    regression_model <- reactive({
      data <- regression_data()
      target <- input$target_variable
      method <- input$regression_method
      
      if (is.null(data) || is.null(target) || !target %in% names(data)) {
        return(NULL)
      }
      
      predictors <- setdiff(names(data), target)
      
      if (length(predictors) == 0) {
        return(NULL)
      }
      
      # Je prépare les données
      X <- as.matrix(data[, predictors, drop = FALSE])
      y <- data[[target]]
      
      if (method == "lm") {
        # Je calcule la régression linéaire multiple avec la fonction lm().
        # Cette méthode est simple, interprétable et ne nécessite pas de régularisation.
        formula_str <- paste(target, "~", paste(predictors, collapse = " + "))
        model <- lm(as.formula(formula_str), data = data)
        
        return(list(
          model = model,
          method = "lm",
          predictions = predict(model),
          residuals = residuals(model),
          coefficients = coef(model)
        ))
      } else if (method %in% c("lasso", "ridge")) {
        # Je calcule la régression avec régularisation (LASSO ou Ridge).
        # La régularisation pénalise les coefficients pour éviter le sur-apprentissage.
        # alpha = 1 pour LASSO (sélection de variables), alpha = 0 pour Ridge (réduction).
        alpha <- ifelse(method == "lasso", 1, 0)
        lambda <- input$lambda
        
        if (is.null(lambda)) {
          lambda <- 0.1
        }
        
        # Je standardise les données pour glmnet car la régularisation est sensible
        # à l'échelle des variables. Je garde les moyennes et écarts-types pour
        # reconvertir les résultats à l'échelle originale.
        X_scaled <- scale(X)
        y_mean <- mean(y)
        y_sd <- sd(y)
        y_scaled <- (y - y_mean) / y_sd
        
        # Je calcule le modèle avec glmnet. standardize = FALSE car on a déjà standardisé.
        model <- glmnet(X_scaled, y_scaled, alpha = alpha, lambda = lambda, standardize = FALSE)
        
        # Je fais les prédictions sur les données standardisées
        predictions_scaled <- predict(model, newx = X_scaled, s = lambda)
        
        # Je reconvertis les prédictions à l'échelle originale pour faciliter
        # l'interprétation des résultats.
        predictions <- as.numeric(predictions_scaled) * y_sd + y_mean
        
        # Je calcule les résidus (différence entre valeurs observées et prédites)
        residuals <- y - predictions
        
        # Je récupère les coefficients sur l'échelle standardisée
        coefficients_scaled <- as.numeric(coef(model, s = lambda))
        
        # Je convertis les coefficients à l'échelle originale pour permettre
        # une interprétation directe dans les unités des variables originales.
        X_means <- attr(X_scaled, "scaled:center")
        X_sds <- attr(X_scaled, "scaled:scale")
        
        coefficients <- coefficients_scaled
        coefficients[-1] <- coefficients_scaled[-1] * (y_sd / X_sds)
        coefficients[1] <- coefficients_scaled[1] * y_sd + y_mean - sum(coefficients_scaled[-1] * X_means * y_sd / X_sds)
        
        names(coefficients) <- c("(Intercept)", predictors)
        
        return(list(
          model = model,
          method = method,
          predictions = predictions,
          residuals = residuals,
          coefficients = coefficients,
          lambda = lambda
        ))
      }
      
      return(NULL)
    })
    
    # ==========================================================================
    # CALCUL DES MÉTRIQUES
    # ==========================================================================
    
    # Ici, je calcule les métriques de performance du modèle (R², RMSE, MAE, MSE)
    # pour évaluer la qualité des prédictions.
    model_metrics <- reactive({
      model_result <- regression_model()
      data <- regression_data()
      target <- input$target_variable
      
      if (is.null(model_result) || is.null(data) || is.null(target)) {
        return(NULL)
      }
      
      y_true <- data[[target]]
      y_pred <- model_result$predictions
      residuals <- model_result$residuals
      
      # Je calcule les métriques
      mse <- mean(residuals^2)
      rmse <- sqrt(mse)
      mae <- mean(abs(residuals))
      
      # R²
      ss_res <- sum(residuals^2)
      ss_tot <- sum((y_true - mean(y_true))^2)
      r_squared <- 1 - (ss_res / ss_tot)
      
      metrics_df <- data.frame(
        Métrique = c("R²", "RMSE", "MAE", "MSE"),
        Valeur = c(
          round(r_squared, 4),
          round(rmse, 4),
          round(mae, 4),
          round(mse, 4)
        ),
        stringsAsFactors = FALSE
      )
      
      return(metrics_df)
    })
    
    # ==========================================================================
    # AFFICHAGE DES MÉTRIQUES
    # ==========================================================================
    
    output$metrics_table <- DT::renderDataTable({
      metrics <- model_metrics()
      
      if (is.null(metrics)) {
        return(data.frame(
          Message = "Configurez le modèle pour voir les métriques."
        ))
      }
      
      DT::datatable(
        metrics,
        options = list(
          pageLength = 10,
          dom = 't',
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
        ),
        rownames = FALSE,
        class = "display nowrap"
      ) %>%
        DT::formatStyle(
          "Métrique",
          fontWeight = "bold",
          color = "#1E5A8E"
        )
    })
    
    # ==========================================================================
    # AFFICHAGE DES COEFFICIENTS
    # ==========================================================================
    
    output$coefficients_table <- DT::renderDataTable({
      model_result <- regression_model()
      
      if (is.null(model_result)) {
        return(data.frame(
          Message = "Configurez le modèle pour voir les coefficients."
        ))
      }
      
      coefficients <- model_result$coefficients
      
      # Je crée un dataframe avec les coefficients
      coef_df <- data.frame(
        Variable = names(coefficients),
        Coefficient = round(coefficients, 4),
        stringsAsFactors = FALSE
      )
      
      # Je trie par valeur absolue pour voir les plus importants
      coef_df <- coef_df[order(abs(coef_df$Coefficient), decreasing = TRUE), ]
      
      DT::datatable(
        coef_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 't',
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
        ),
        rownames = FALSE,
        class = "display nowrap"
      ) %>%
        DT::formatStyle(
          "Variable",
          fontWeight = "bold",
          color = "#1E5A8E"
        ) %>%
        DT::formatStyle(
          "Coefficient",
          backgroundColor = DT::styleInterval(
            c(-0.5, 0, 0.5),
            c("#DC3545", "#FFFFFF", "#28A745", "#1E5A8E")
          )
        )
    })
    
    # ==========================================================================
    # GRAPHIQUE D'IMPORTANCE DES VARIABLES
    # ==========================================================================
    
    # Ici, je crée un graphique montrant l'importance de chaque variable explicative
    # basée sur la valeur absolue des coefficients. Cela permet d'identifier les
    # variables les plus influentes dans la prédiction.
    output$importance_plot <- renderPlot({
      model_result <- regression_model()
      
      if (is.null(model_result)) {
        return(NULL)
      }
      
      coefficients <- model_result$coefficients
      
      # Je retire l'intercept
      coef_no_intercept <- coefficients[names(coefficients) != "(Intercept)"]
      
      if (length(coef_no_intercept) == 0) {
        return(NULL)
      }
      
      # Je crée un dataframe pour le graphique
      importance_df <- data.frame(
        Variable = names(coef_no_intercept),
        Importance = abs(coef_no_intercept),
        Coefficient = coef_no_intercept,
        stringsAsFactors = FALSE
      )
      
      # Je trie par importance
      importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
      
      # Je limite aux 15 premières variables pour la lisibilité
      if (nrow(importance_df) > 15) {
        importance_df <- importance_df[1:15, ]
      }
      
      # Je crée le graphique
      colors <- get_theme_colors()
      
      p <- ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance, fill = Coefficient > 0)) +
        geom_bar(stat = "identity", alpha = 0.7) +
        coord_flip() +
        scale_fill_manual(
          values = c("#DC3545", "#1E5A8E"),
          labels = c("Négatif", "Positif"),
          name = "Signe"
        ) +
        theme_academique() +
        labs(
          title = "Importance des Variables",
          subtitle = "Valeur absolue des coefficients",
          x = "Variable",
          y = "Importance (|coefficient|)"
        ) +
        theme(
          plot.title = element_text(color = colors$text_primary, hjust = 0.5),
          plot.subtitle = element_text(color = colors$text_secondary, hjust = 0.5)
        )
      
      return(p)
    })
    
    # ==========================================================================
    # GRAPHIQUE PRÉDICTIONS VS OBSERVATIONS
    # ==========================================================================
    
    # Ici, je crée un graphique comparant les valeurs prédites aux valeurs observées.
    # Plus les points sont proches de la ligne rouge (y=x), meilleure est la prédiction.
    output$predictions_plot <- renderPlot({
      model_result <- regression_model()
      data <- regression_data()
      target <- input$target_variable
      
      if (is.null(model_result) || is.null(data) || is.null(target)) {
        return(NULL)
      }
      
      y_true <- data[[target]]
      y_pred <- model_result$predictions
      
      # Je crée le graphique
      colors <- get_theme_colors()
      
      p <- ggplot(data.frame(Observations = y_true, Prédictions = y_pred), 
                  aes(x = Observations, y = Prédictions)) +
        geom_point(color = colors$chart_blue, alpha = 0.6, size = 2) +
        geom_abline(intercept = 0, slope = 1, color = colors$danger, linetype = "dashed", size = 1) +
        theme_academique() +
        labs(
          title = "Prédictions vs Observations",
          subtitle = "La ligne rouge représente une prédiction parfaite",
          x = "Valeurs observées",
          y = "Valeurs prédites"
        ) +
        theme(
          plot.title = element_text(color = colors$text_primary, hjust = 0.5),
          plot.subtitle = element_text(color = colors$text_secondary, hjust = 0.5)
        )
      
      return(p)
    })
    
    # ==========================================================================
    # GRAPHIQUE DES RÉSIDUS
    # ==========================================================================
    
    # Ici, je crée un graphique des résidus pour vérifier les hypothèses du modèle.
    # Les résidus doivent être distribués aléatoirement autour de zéro sans pattern.
    output$residuals_plot <- renderPlot({
      model_result <- regression_model()
      data <- regression_data()
      target <- input$target_variable
      
      if (is.null(model_result) || is.null(data) || is.null(target)) {
        return(NULL)
      }
      
      y_true <- data[[target]]
      residuals <- model_result$residuals
      
      # Je crée le graphique
      colors <- get_theme_colors()
      
      p <- ggplot(data.frame(Observations = y_true, Résidus = residuals), 
                  aes(x = Observations, y = Résidus)) +
        geom_point(color = colors$chart_blue, alpha = 0.6, size = 2) +
        geom_hline(yintercept = 0, color = colors$danger, linetype = "dashed", size = 1) +
        theme_academique() +
        labs(
          title = "Analyse des Résidus",
          subtitle = "Les résidus doivent être distribués aléatoirement autour de 0",
          x = "Valeurs observées",
          y = "Résidus"
        ) +
        theme(
          plot.title = element_text(color = colors$text_primary, hjust = 0.5),
          plot.subtitle = element_text(color = colors$text_secondary, hjust = 0.5)
        )
      
      return(p)
    })
    
  })
}
