# ==============================================================================
# MODULE ANALYSE TEMPORELLE
# ==============================================================================
# 
# Module Shiny pour analyser la dimension temporelle des données,
# notamment les séries temporelles par pays et trimestre.
# 
# Fonctionnalités :
# - Séries temporelles des marqueurs (CA 15-3) par pays
# - Séries temporelles des amorces détectées
# - Comparaison trimestre par trimestre
# - Détection de tendances
#
# ==============================================================================

library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)

# Charger les fonctions d'analyse temporelle
source("R/time_series_analysis.R", local = TRUE)

# Charger les thèmes
source("R/theme_shiny.R")
source("R/theme_ggplot.R")

# ==============================================================================
# UI DU MODULE
# ==============================================================================

#' Interface utilisateur du module d'analyse temporelle
#'
#' @param id Identifiant unique du module
#' @return Interface utilisateur du module
mod_temporal_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Activer shinyjs si nécessaire (pour futures améliorations)
    useShinyjs(),
    
    # Titre de la page
    div(
      class = "container-fluid",
      style = "margin-bottom: 2rem;",
      h1(
        style = "color: #1E5A8E; margin-bottom: 0.5rem;",
        icon("clock", style = "margin-right: 0.5rem;"),
        "Analyse Temporelle"
      ),
      p(
        style = "color: #6C757D; font-size: 1.1rem;",
        "Analyse de l'évolution temporelle des données par trimestre et par pays"
      ),
      
      # Explication
      div(
        style = "padding: 1.5rem; background-color: #E7F3FF; border-left: 4px solid #1E5A8E; border-radius: 0.375rem; margin-top: 1rem;",
        h4(
          style = "color: #1E5A8E; margin-bottom: 1rem;",
          icon("info-circle", style = "margin-right: 0.5rem;"),
          "Dimension Temporelle"
        ),
        p(
          style = "color: #0C5460; margin: 0;",
          "Ce module permet d'analyser l'évolution des variables dans le temps,",
          " de détecter des tendances (croissance/décroissance) et de comparer",
          " les trimestres entre eux."
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
            "Configuration de l'Analyse"
          )
        ),
        
        div(
          class = "card-body",
          style = "padding: 1.5rem;",
          
          fluidRow(
            # Sélection de la variable
            column(
              width = 4,
              uiOutput(ns("variable_selector"))
            ),
            
            # Sélection du pays
            column(
              width = 4,
              uiOutput(ns("country_selector"))
            ),
            
            # Type de graphique
            column(
              width = 4,
              selectInput(
                ns("plot_type"),
                label = tags$strong("Type de graphique"),
                choices = list(
                  "Ligne temporelle" = "line",
                  "Barres" = "bar",
                  "Heatmap" = "heatmap"
                ),
                selected = "line"
              )
            )
          ),
          
          # Message d'information
          uiOutput(ns("temporal_info"))
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

#' Logique serveur du module d'analyse temporelle
#'
#' @param id Identifiant unique du module
#' @param imported_data Données importées (réactif)
#' @return Rien (effets de bord : affichage des résultats)
mod_temporal_server <- function(id, imported_data) {
  moduleServer(id, function(input, output, session) {
    
    # Définir le namespace
    ns <- session$ns
    
    # ==========================================================================
    # SÉLECTION DE LA VARIABLE
    # ==========================================================================
    
    output$variable_selector <- renderUI({
      data <- imported_data()
      
      if (is.null(data) || nrow(data) == 0) {
        return(
          selectInput(
            ns("selected_variable"),
            label = tags$strong("Variable à analyser"),
            choices = list("Aucune donnée" = ""),
            selected = ""
          )
        )
      }
      
      # Filtrer les variables numériques
      numeric_vars <- sapply(data, is.numeric)
      numeric_vars <- names(data)[numeric_vars]
      
      # Exclure les colonnes d'identifiant et de date
      exclude_cols <- c("id_global", "id", "annee", "year")
      numeric_vars <- setdiff(numeric_vars, exclude_cols)
      
      if (length(numeric_vars) == 0) {
        return(
          selectInput(
            ns("selected_variable"),
            label = tags$strong("Variable à analyser"),
            choices = list("Aucune variable numérique" = ""),
            selected = ""
          )
        )
      }
      
      selectInput(
        ns("selected_variable"),
        label = tags$strong("Variable à analyser"),
        choices = numeric_vars,
        selected = numeric_vars[1]
      )
    })
    
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
            label = tags$strong("Pays"),
            choices = list("Tous les pays" = "all"),
            selected = "all"
          )
        )
      }
      
      # Obtenir les pays uniques
      countries <- unique(data[[country_col]])
      countries <- countries[!is.na(countries)]
      countries <- sort(countries)
      
      choices_list <- list("Tous les pays" = "all")
      for (country in countries) {
        choices_list[[as.character(country)]] <- country
      }
      
      selectInput(
        ns("selected_country"),
        label = tags$strong("Pays"),
        choices = choices_list,
        selected = "all"
      )
    })
    
    # ==========================================================================
    # PRÉPARATION DES DONNÉES TEMPORELLES
    # ==========================================================================
    
    temporal_data <- reactive({
      data <- imported_data()
      variable <- input$selected_variable
      country <- input$selected_country
      
      if (is.null(data) || nrow(data) == 0 || is.null(variable) || variable == "") {
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
      
      # Préparer les données temporelles
      if (!is.null(country_col) && country != "all" && !is.null(country)) {
        prepared <- prepare_temporal_data(
          data,
          date_col = "trimestre",
          value_col = variable,
          country_col = country_col,
          country = country
        )
      } else {
        prepared <- prepare_temporal_data(
          data,
          date_col = "trimestre",
          value_col = variable,
          country_col = country_col,
          country = NULL
        )
      }
      
      return(prepared)
    })
    
    # ==========================================================================
    # CALCUL DES TENDANCES
    # ==========================================================================
    
    trends <- reactive({
      data <- temporal_data()
      
      if (is.null(data) || nrow(data) < 2) {
        return(NULL)
      }
      
      # Calculer les tendances
      trends_result <- calculate_trends(data, date_col = "trimestre", value_col = "valeur")
      
      return(trends_result)
    })
    
    # ==========================================================================
    # INFORMATIONS SUR L'ANALYSE
    # ==========================================================================
    
    output$temporal_info <- renderUI({
      data <- imported_data()
      temporal <- temporal_data()
      trend <- trends()
      
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
      
      if (!is.null(temporal) && nrow(temporal) > 0 && !is.null(trend)) {
        trend_icon <- if (trend$trend == "croissance") "arrow-up" else if (trend$trend == "décroissance") "arrow-down" else "minus"
        trend_color <- if (trend$trend == "croissance") "#28A745" else if (trend$trend == "décroissance") "#DC3545" else "#6C757D"
        
        return(
          div(
            style = "padding: 1rem; background-color: #D4EDDA; border-left: 4px solid #28A745; border-radius: 0.375rem; margin-top: 1rem;",
            p(
              style = "margin: 0; color: #155724;",
              icon("check-circle", style = "margin-right: 0.5rem;"),
              tags$strong("Tendance détectée :"), " ",
              icon(trend_icon, style = paste0("color: ", trend_color, "; margin-right: 0.5rem;")),
              trend$trend,
              if (!is.na(trend$p_value) && trend$p_value < 0.05) {
                paste0(" (p = ", format(trend$p_value, digits = 3), ", significatif)")
              } else if (!is.na(trend$p_value)) {
                paste0(" (p = ", format(trend$p_value, digits = 3), ", non significatif)")
              }
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
      temporal <- temporal_data()
      plot_type <- input$plot_type
      
      if (is.null(temporal) || nrow(temporal) == 0) {
        return(NULL)
      }
      
      tagList(
        # Graphique temporel
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
                "Évolution Temporelle"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              plotlyOutput(ns("temporal_plot"), height = "500px")
            )
          )
        ),
        
        # Statistiques par trimestre
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
                "Statistiques par Trimestre"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              DT::dataTableOutput(ns("quarterly_stats_table"))
            )
          )
        )
      )
    })
    
    # ==========================================================================
    # GRAPHIQUE TEMPOREL
    # ==========================================================================
    
    output$temporal_plot <- renderPlotly({
      temporal <- temporal_data()
      plot_type <- input$plot_type
      variable <- input$selected_variable
      
      if (is.null(temporal) || nrow(temporal) == 0) {
        return(plotly_empty())
      }
      
      tryCatch({
        if (plot_type == "line") {
          # Graphique de ligne temporelle
          if ("pays" %in% names(temporal)) {
            p <- ggplot(temporal, aes(x = trimestre, y = valeur, color = pays, group = pays)) +
              geom_line(linewidth = 1.2) +
              geom_point(size = 2) +
              theme_academique() +
              labs(
                title = paste("Évolution de", variable, "dans le temps"),
                x = "Trimestre",
                y = variable,
                color = "Pays"
              ) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          } else {
            p <- ggplot(temporal, aes(x = trimestre, y = valeur, group = 1)) +
              geom_line(linewidth = 1.2, color = "#1E5A8E") +
              geom_point(size = 2, color = "#1E5A8E") +
              theme_academique() +
              labs(
                title = paste("Évolution de", variable, "dans le temps"),
                x = "Trimestre",
                y = variable
              ) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          }
        } else if (plot_type == "bar") {
          # Graphique en barres
          if ("pays" %in% names(temporal)) {
            p <- ggplot(temporal, aes(x = trimestre, y = valeur, fill = pays)) +
              geom_bar(stat = "identity", position = "dodge") +
              theme_academique() +
              labs(
                title = paste("Évolution de", variable, "par trimestre"),
                x = "Trimestre",
                y = variable,
                fill = "Pays"
              ) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          } else {
            p <- ggplot(temporal, aes(x = trimestre, y = valeur)) +
              geom_bar(stat = "identity", fill = "#1E5A8E") +
              theme_academique() +
              labs(
                title = paste("Évolution de", variable, "par trimestre"),
                x = "Trimestre",
                y = variable
              ) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          }
        } else {
          # Heatmap
          if ("pays" %in% names(temporal)) {
            p <- ggplot(temporal, aes(x = trimestre, y = pays, fill = valeur)) +
              geom_tile(color = "white", linewidth = 0.5) +
              scale_fill_gradient(low = "white", high = "steelblue", name = variable) +
              theme_academique() +
              labs(
                title = paste("Heatmap de", variable, "par pays et trimestre"),
                x = "Trimestre",
                y = "Pays"
              ) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          } else {
            return(plotly_empty())
          }
        }
        
        ggplotly(p, tooltip = c("x", "y", "fill", "color")) %>%
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
    # TABLEAU DES STATISTIQUES PAR TRIMESTRE
    # ==========================================================================
    
    output$quarterly_stats_table <- DT::renderDataTable({
      temporal <- temporal_data()
      
      if (is.null(temporal) || nrow(temporal) == 0) {
        return(data.frame())
      }
      
      DT::datatable(
        temporal,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = "valeur", digits = 2)
    })
  })
}
