# ==============================================================================
# MODULE PRÉDICTION AUTORÉGRESSIVE (AR)
# ==============================================================================
# 
# Module Shiny pour prédire le nombre de cas trimestriels avec un modèle AR(1),
# selon la demande d'Arthur Loone : "prédire le nombre de cas d'un trimestre
# sur l'autre, pays par pays ou au niveau de l'europe"
# 
# Fonctionnalités :
# - Modèle AR(1) : cas_t = α + β * cas_{t-1} + ε
# - Prédiction par pays
# - Prédiction au niveau européen
# - Validation sur nouvelles données
# - Métriques de performance
#
# ==============================================================================

library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)

# Charger les fonctions du modèle AR
source("R/ar_model.R", local = TRUE)

# Charger les thèmes
source("R/theme_shiny.R")
source("R/theme_ggplot.R")

# ==============================================================================
# UI DU MODULE
# ==============================================================================

#' Interface utilisateur du module de prédiction AR
#'
#' @param id Identifiant unique du module
#' @return Interface utilisateur du module
mod_ar_prediction_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Activer shinyjs pour ce module
    useShinyjs(),
    
    # Titre de la page
    div(
      class = "container-fluid",
      style = "margin-bottom: 2rem;",
      h1(
        style = "color: #1E5A8E; margin-bottom: 0.5rem;",
        icon("chart-area", style = "margin-right: 0.5rem;"),
        "Prédiction Temporelle - Modèle Autorégressif"
      ),
      p(
        style = "color: #6C757D; font-size: 1.1rem;",
        "Prédiction du nombre de cas au trimestre suivant basée sur le trimestre précédent"
      ),
      
      # Explication du modèle
      div(
        style = "padding: 1.5rem; background-color: #E7F3FF; border-left: 4px solid #1E5A8E; border-radius: 0.375rem; margin-top: 1rem;",
        h4(
          style = "color: #1E5A8E; margin-bottom: 1rem;",
          icon("info-circle", style = "margin-right: 0.5rem;"),
          "Modèle AR(1)"
        ),
        p(
          style = "color: #0C5460; margin-bottom: 0.5rem;",
          "Le modèle autorégressif d'ordre 1 (AR(1)) prédit le nombre de cas",
          " au trimestre suivant en fonction du nombre de cas au trimestre précédent."
        ),
        p(
          style = "color: #0C5460; margin: 0;",
          tags$strong("Formule :"), " cas_t = α + β × cas_{t-1} + ε",
          " où cas_t est le nombre de cas au trimestre t et cas_{t-1} au trimestre précédent."
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
            "Configuration du Modèle"
          )
        ),
        
        div(
          class = "card-body",
          style = "padding: 1.5rem;",
          
          fluidRow(
            # Sélection du pays
            column(
              width = 6,
              uiOutput(ns("country_selector"))
            ),
            
            # Bouton de lancement
            column(
              width = 6,
              div(
                style = "margin-top: 1.75rem;",
                actionButton(
                  ns("run_prediction"),
                  label = "Lancer la prédiction",
                  icon = icon("play"),
                  class = "btn-primary",
                  style = "width: 100%;"
                ),
                # Indicateur de chargement
                div(
                  id = ns("loading_indicator"),
                  style = "display: none; margin-top: 0.5rem; text-align: center;",
                  tags$span(
                    icon("spinner", class = "fa-spin"),
                    " Calcul en cours..."
                  )
                )
              )
            )
          ),
          
          # Message d'information
          uiOutput(ns("prediction_info"))
        )
      )
    ),
    
    # Résultats
    uiOutput(ns("results_panel"))
  )
}

# ==============================================================================
# SERVER DU MODULE
# ==============================================================================

#' Logique serveur du module de prédiction AR
#'
#' @param id Identifiant unique du module
#' @param imported_data Données importées (réactif)
#' @return Rien (effets de bord : affichage des résultats)
mod_ar_prediction_server <- function(id, imported_data) {
  moduleServer(id, function(input, output, session) {
    
    # Définir le namespace
    ns <- session$ns
    
    # ==========================================================================
    # SÉLECTION DU PAYS
    # ==========================================================================
    
    output$country_selector <- renderUI({
      data <- imported_data()
      
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      # Chercher une colonne pays
      country_cols <- c("pays", "country", "nom_pays", "pays_code")
      country_col <- NULL
      
      for (col in country_cols) {
        if (col %in% names(data)) {
          country_col <- col
          break
        }
      }
      
      if (is.null(country_col)) {
        return(
          selectInput(
            ns("selected_country"),
            label = tags$strong("Niveau d'analyse"),
            choices = list("Europe (tous les pays)" = "all"),
            selected = "all"
          )
        )
      }
      
      # Obtenir les pays uniques
      countries <- unique(data[[country_col]])
      countries <- countries[!is.na(countries)]
      countries <- sort(countries)
      
      choices_list <- list("Europe (tous les pays)" = "all")
      for (country in countries) {
        choices_list[[as.character(country)]] <- country
      }
      
      selectInput(
        ns("selected_country"),
        label = tags$strong("Niveau d'analyse"),
        choices = choices_list,
        selected = "all"
      )
    })
    
    # ==========================================================================
    # CALCUL DES CAS TRIMESTRIELS
    # ==========================================================================
    
    quarterly_cases <- eventReactive(input$run_prediction, {
      data <- isolate(imported_data())
      country <- isolate(input$selected_country)
      
      # Afficher l'indicateur de chargement
      shinyjs::show("loading_indicator")
      
      on.exit({
        shinyjs::hide("loading_indicator")
      })
      
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      # Déterminer la colonne pays
      country_cols <- c("pays", "country", "nom_pays", "pays_code")
      country_col <- NULL
      
      for (col in country_cols) {
        if (col %in% names(data)) {
          country_col <- col
          break
        }
      }
      
      # Calculer les cas trimestriels
      tryCatch({
        if (country == "all" || is.null(country)) {
          # Niveau européen
          cases <- calculate_quarterly_cases(
            data,
            id_col = "id_global",
            date_col = "trimestre",
            country_col = country_col,
            country = NULL
          )
        } else {
          # Par pays
          cases <- calculate_quarterly_cases(
            data,
            id_col = "id_global",
            date_col = "trimestre",
            country_col = country_col,
            country = country
          )
        }
        
        return(cases)
      }, error = function(e) {
        warning("Erreur lors du calcul des cas trimestriels : ", e$message)
        return(NULL)
      })
    })
    
    # ==========================================================================
    # AJUSTEMENT DU MODÈLE AR
    # ==========================================================================
    
    ar_model <- reactive({
      cases <- quarterly_cases()
      
      if (is.null(cases) || nrow(cases) < 2) {
        return(NULL)
      }
      
      # Extraire les valeurs de cas
      cas_values <- cases$cas
      
      # Ajuster le modèle AR(1)
      tryCatch({
        model <- fit_ar_model(cas_values)
        return(model)
      }, error = function(e) {
        warning("Erreur lors de l'ajustement du modèle AR : ", e$message)
        return(NULL)
      })
    })
    
    # ==========================================================================
    # PRÉDICTION
    # ==========================================================================
    
    predictions <- reactive({
      cases <- quarterly_cases()
      model <- ar_model()
      
      if (is.null(cases) || is.null(model)) {
        return(NULL)
      }
      
      # Prédire pour chaque trimestre (basé sur le précédent)
      predicted_values <- c()
      actual_values <- cases$cas
      
      for (i in 2:length(actual_values)) {
        pred <- predict_next_quarter(model, actual_values[i - 1])
        predicted_values <- c(predicted_values, pred)
      }
      
      # Créer un dataframe avec prédictions et valeurs réelles
      result <- data.frame(
        trimestre = cases$trimestre[2:nrow(cases)],
        actual = actual_values[2:length(actual_values)],
        predicted = predicted_values,
        stringsAsFactors = FALSE
      )
      
      # Calculer les erreurs
      result$error <- result$actual - result$predicted
      result$abs_error <- abs(result$error)
      result$pct_error <- (result$error / result$actual) * 100
      
      return(result)
    })
    
    # ==========================================================================
    # MÉTRIQUES DE PERFORMANCE
    # ==========================================================================
    
    metrics <- reactive({
      pred <- predictions()
      
      if (is.null(pred)) {
        return(NULL)
      }
      
      # Calculer les métriques
      metrics_result <- calculate_ar_metrics(pred$actual, pred$predicted)
      
      return(metrics_result)
    })
    
    # ==========================================================================
    # INFORMATIONS SUR LA PRÉDICTION
    # ==========================================================================
    
    output$prediction_info <- renderUI({
      data <- imported_data()
      cases <- quarterly_cases()
      model <- ar_model()
      
      if (is.null(data) || nrow(data) == 0) {
        return(
          div(
            style = "padding: 1rem; background-color: #FFF3CD; border-left: 4px solid #FFC107; border-radius: 0.375rem; margin-top: 1rem;",
            p(
              style = "margin: 0; color: #856404;",
              icon("info-circle", style = "margin-right: 0.5rem;"),
              "Importez d'abord un fichier CSV avec des données temporelles (colonne 'trimestre')."
            )
          )
        )
      }
      
      if (!is.null(cases) && nrow(cases) > 0 && !is.null(model)) {
        return(
          div(
            style = "padding: 1rem; background-color: #D4EDDA; border-left: 4px solid #28A745; border-radius: 0.375rem; margin-top: 1rem;",
            p(
              style = "margin: 0; color: #155724;",
              icon("check-circle", style = "margin-right: 0.5rem;"),
              tags$strong("Modèle ajusté !"),
              paste0(" α = ", format(model$alpha, digits = 3), 
                     ", β = ", format(model$beta, digits = 3),
                     ", R² = ", format(model$r_squared, digits = 3))
            )
          )
        )
      }
      
      return(NULL)
    })
    
    # ==========================================================================
    # PANEL DE RÉSULTATS
    # ==========================================================================
    
    output$results_panel <- renderUI({
      cases <- quarterly_cases()
      model <- ar_model()
      pred <- predictions()
      metrics_result <- metrics()
      
      if (is.null(cases) || is.null(model) || is.null(pred)) {
        return(NULL)
      }
      
      tagList(
        # Métriques de performance
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
                icon("chart-bar", style = "margin-right: 0.5rem;"),
                "Métriques de Performance"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              fluidRow(
                column(
                  3,
                  div(
                    style = "padding: 1rem; background-color: #F8F9FA; border-left: 4px solid #1E5A8E; border-radius: 0.375rem;",
                    h4(style = "color: #1E5A8E; margin-bottom: 0.5rem;", "RMSE"),
                    p(style = "font-size: 1.5rem; font-weight: 700; color: #2C3E50; margin: 0;", format(metrics_result$rmse, digits = 2))
                  )
                ),
                column(
                  3,
                  div(
                    style = "padding: 1rem; background-color: #F8F9FA; border-left: 4px solid #17A2B8; border-radius: 0.375rem;",
                    h4(style = "color: #1E5A8E; margin-bottom: 0.5rem;", "MAE"),
                    p(style = "font-size: 1.5rem; font-weight: 700; color: #2C3E50; margin: 0;", format(metrics_result$mae, digits = 2))
                  )
                ),
                column(
                  3,
                  div(
                    style = "padding: 1rem; background-color: #F8F9FA; border-left: 4px solid #28A745; border-radius: 0.375rem;",
                    h4(style = "color: #1E5A8E; margin-bottom: 0.5rem;", "R²"),
                    p(style = "font-size: 1.5rem; font-weight: 700; color: #2C3E50; margin: 0;", format(metrics_result$r_squared, digits = 3))
                  )
                ),
                column(
                  3,
                  div(
                    style = "padding: 1rem; background-color: #F8F9FA; border-left: 4px solid #FFC107; border-radius: 0.375rem;",
                    h4(style = "color: #1E5A8E; margin-bottom: 0.5rem;", "MAPE"),
                    p(style = "font-size: 1.5rem; font-weight: 700; color: #2C3E50; margin: 0;", paste0(format(metrics_result$mape, digits = 2), "%"))
                  )
                )
              )
            )
          )
        ),
        
        # Graphique prédiction vs réalité
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
                icon("chart-line", style = "margin-right: 0.5rem;"),
                "Prédiction vs Réalité"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              plotlyOutput(ns("prediction_plot"), height = "500px")
            )
          )
        ),
        
        # Tableau des prédictions
        div(
          class = "container-fluid",
          
          div(
            class = "card",
            
            div(
              class = "card-header",
              style = "background-color: #F8F9FA; border-bottom: 2px solid #1E5A8E;",
              h3(
                style = "margin: 0; color: #1E5A8E; font-size: 1.25rem;",
                icon("table", style = "margin-right: 0.5rem;"),
                "Détails des Prédictions"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              DT::dataTableOutput(ns("predictions_table"))
            )
          )
        )
      )
    })
    
    # ==========================================================================
    # GRAPHIQUE PRÉDICTION VS RÉALITÉ
    # ==========================================================================
    
    output$prediction_plot <- renderPlotly({
      pred <- predictions()
      
      if (is.null(pred)) {
        return(plotly_empty())
      }
      
      tryCatch({
        # Préparer les données pour le graphique
        plot_data <- pred %>%
          mutate(trimestre_num = 1:nrow(.)) %>%
          pivot_longer(cols = c(actual, predicted), names_to = "type", values_to = "cas")
        
        # Créer le graphique
        p <- ggplot(plot_data, aes(x = trimestre_num, y = cas, color = type, linetype = type)) +
          geom_line(linewidth = 1.2) +
          geom_point(size = 2) +
          scale_color_manual(
            values = c("actual" = "#1E5A8E", "predicted" = "#DC3545"),
            labels = c("actual" = "Réel", "predicted" = "Prédit")
          ) +
          scale_linetype_manual(
            values = c("actual" = "solid", "predicted" = "dashed"),
            labels = c("actual" = "Réel", "predicted" = "Prédit")
          ) +
          scale_x_continuous(
            breaks = unique(plot_data$trimestre_num),
            labels = unique(pred$trimestre[order(pred$trimestre)])
          ) +
          theme_academique() +
          labs(
            title = "Prédiction vs Réalité - Modèle AR(1)",
            x = "Trimestre",
            y = "Nombre de cas",
            color = "Type",
            linetype = "Type"
          ) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(p, tooltip = c("x", "y", "type")) %>%
          config(
            displayModeBar = TRUE, 
            displaylogo = FALSE, 
            modeBarButtonsToRemove = c("pan2d", "lasso2d", "select2d", "autoScale2d", "resetScale2d"),
            toImageButtonOptions = list(format = "png", width = 800, height = 600, scale = 1),
            doubleClick = "reset",
            sendData = FALSE
          )
      }, error = function(e) {
        warning("Erreur lors de la création du graphique : ", e$message)
        return(plotly_empty())
      })
    })
    
    # ==========================================================================
    # TABLEAU DES PRÉDICTIONS
    # ==========================================================================
    
    output$predictions_table <- DT::renderDataTable({
      pred <- predictions()
      
      if (is.null(pred)) {
        return(data.frame())
      }
      
      # Préparer le tableau
      table_data <- pred %>%
        select(trimestre, actual, predicted, error, abs_error, pct_error) %>%
        rename(
          "Trimestre" = trimestre,
          "Réel" = actual,
          "Prédit" = predicted,
          "Erreur" = error,
          "Erreur Absolue" = abs_error,
          "Erreur %" = pct_error
        )
      
      DT::datatable(
        table_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("Réel", "Prédit", "Erreur", "Erreur Absolue", "Erreur %"), digits = 2)
    })
  })
}
