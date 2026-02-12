# ==============================================================================
# MODULE ANALYSE DES AMORCES ADN
# ==============================================================================
# 
# Module Shiny pour détecter et analyser les amorces ADN dans les séquences,
# basé sur la logique du projet GitHub (algo_test.Rmd).
# 
# Fonctionnalités :
# - Détection d'amorces avec tolérance aux erreurs
# - Visualisations : barres totales, heatmap par pays, pourcentage de présence
# - Statistiques par amorce et par pays
#
# ==============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
library(shinyjs)

# Charger les fonctions de détection d'amorces
source("R/detect_primers.R", local = TRUE)

# Charger les thèmes
source("R/theme_shiny.R")
source("R/theme_ggplot.R")

# ==============================================================================
# UI DU MODULE
# ==============================================================================

#' Interface utilisateur du module d'analyse des amorces ADN
#'
#' @param id Identifiant unique du module
#' @return Interface utilisateur du module
mod_amorces_adn_ui <- function(id) {
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
        icon("dna", style = "margin-right: 0.5rem;"),
        "Analyse des Amorces ADN"
      ),
      p(
        style = "color: #6C757D; font-size: 1.1rem;",
        "Détection et analyse des amorces génétiques dans les séquences ADN avec tolérance aux mutations"
      ),
      
      # Explication de la méthode
      div(
        style = "padding: 1.5rem; background-color: #E7F3FF; border-left: 4px solid #1E5A8E; border-radius: 0.375rem; margin-top: 1rem;",
        h4(
          style = "color: #1E5A8E; margin-bottom: 1rem;",
          icon("info-circle", style = "margin-right: 0.5rem;"),
          "Méthode de Détection"
        ),
        p(
          style = "color: #0C5460; margin-bottom: 0.5rem;",
          "Ce module détecte des amorces spécifiques dans les séquences ADN en utilisant une",
          " fenêtre glissante avec une tolérance de 2 erreurs par occurrence.",
          " Cela permet de détecter les amorces même en présence de mutations ou de variations."
        ),
        p(
          style = "color: #0C5460; margin: 0;",
          tags$strong("Amorces recherchées :"), " gggccc, acctcca, tttttta, gggacggg, atatatat, gtacacgt"
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
            # Sélection de la colonne de séquence ADN
            column(
              width = 6,
              uiOutput(ns("sequence_column_selector"))
            ),
            
            # Tolérance aux erreurs
            column(
              width = 3,
              numericInput(
                ns("tolerance"),
                label = tags$strong("Tolérance aux erreurs"),
                value = 2,
                min = 0,
                max = 5,
                step = 1
              ),
              p(
                style = "font-size: 0.85rem; color: #6C757D; margin-top: -0.5rem;",
                "Nombre d'erreurs tolérées par occurrence"
              )
            ),
            
            # Bouton de lancement avec indicateur de chargement
            column(
              width = 3,
              div(
                style = "margin-top: 1.75rem;",
                actionButton(
                  ns("run_detection"),
                  label = "Lancer la détection",
                  icon = icon("play"),
                  class = "btn-primary",
                  style = "width: 100%;"
                ),
                # Indicateur de chargement (masqué par défaut)
                div(
                  id = ns("loading_indicator"),
                  style = "display: none; margin-top: 0.5rem; text-align: center;",
                  tags$span(
                    icon("spinner", class = "fa-spin"),
                    " Traitement en cours..."
                  )
                )
              )
            )
          ),
          
          # Message d'information
          uiOutput(ns("detection_info"))
        )
      )
    ),
    
    # Résultats de la détection
    uiOutput(ns("results_panel"))
  )
}

# ==============================================================================
# SERVER DU MODULE
# ==============================================================================

#' Logique serveur du module d'analyse des amorces ADN
#'
#' @param id Identifiant unique du module
#' @param imported_data Données importées (réactif)
#' @return Rien (effets de bord : affichage des résultats)
mod_amorces_adn_server <- function(id, imported_data) {
  moduleServer(id, function(input, output, session) {
    
    # Définir le namespace
    ns <- session$ns
    
    # ==========================================================================
    # SÉLECTION DE LA COLONNE DE SÉQUENCE ADN
    # ==========================================================================
    
    output$sequence_column_selector <- renderUI({
      data <- imported_data()
      
      if (is.null(data) || nrow(data) == 0) {
        return(
          selectInput(
            ns("sequence_column"),
            label = tags$strong("Colonne de séquence ADN"),
            choices = list("Aucune donnée" = ""),
            selected = ""
          )
        )
      }
      
      # Chercher les colonnes qui pourraient contenir des séquences ADN
      possible_cols <- c("sequence_adn", "sequence", "adn", "dna", "seq")
      sequence_cols <- c()
      
      for (col in names(data)) {
        col_lower <- tolower(col)
        if (any(sapply(possible_cols, function(x) grepl(x, col_lower, fixed = TRUE)))) {
          sequence_cols <- c(sequence_cols, col)
        }
      }
      
      # Si aucune colonne spécifique trouvée, proposer toutes les colonnes texte
      if (length(sequence_cols) == 0) {
        text_cols <- sapply(data, function(x) is.character(x) || is.factor(x))
        sequence_cols <- names(data)[text_cols]
      }
      
      # Si toujours rien, proposer toutes les colonnes
      if (length(sequence_cols) == 0) {
        sequence_cols <- names(data)
      }
      
      selectInput(
        ns("sequence_column"),
        label = tags$strong("Colonne de séquence ADN"),
        choices = sequence_cols,
        selected = if ("sequence_adn" %in% sequence_cols) "sequence_adn" else sequence_cols[1]
      )
    })
    
    # ==========================================================================
    # DÉTECTION DES AMORCES
    # ==========================================================================
    
    # Ici, je détecte les amorces dans les séquences ADN quand l'utilisateur
    # clique sur le bouton "Lancer la détection"
    # J'utilise isolate() pour éviter les recalculs inutiles
    detection_results <- eventReactive(input$run_detection, {
      # Afficher l'indicateur de chargement
      shinyjs::show("loading_indicator")
      
      # J'isole les valeurs pour éviter les dépendances réactives inutiles
      data <- isolate(imported_data())
      
      on.exit({
        # Masquer l'indicateur de chargement à la fin
        shinyjs::hide("loading_indicator")
      })
      
      if (is.null(data) || nrow(data) == 0) {
        warning("⚠️ Aucune donnée disponible pour la détection")
        return(NULL)
      }
      
      sequence_col <- isolate(input$sequence_column)
      tolerance <- isolate(input$tolerance)
      
      if (is.null(sequence_col) || sequence_col == "" || !sequence_col %in% names(data)) {
        warning("⚠️ Colonne de séquence invalide : ", sequence_col)
        return(NULL)
      }
      
      if (is.null(tolerance)) tolerance <- 2
      
      # Vérifier que la colonne contient des données valides
      sequences <- data[[sequence_col]]
      valid_sequences <- sequences[!is.na(sequences) & sequences != "" & nchar(as.character(sequences)) > 0]
      
      if (length(valid_sequences) == 0) {
        warning("⚠️ Aucune séquence valide trouvée dans la colonne : ", sequence_col)
        return(NULL)
      }
      
      # Créer un dataframe temporaire avec la colonne sequence_adn
      temp_data <- data.frame(data, stringsAsFactors = FALSE)
      temp_data$sequence_adn <- temp_data[[sequence_col]]
      
      # Détecter toutes les amorces avec gestion d'erreur
      tryCatch({
        results <- detect_primers_in_dataframe(
          temp_data,
          primers = get_default_primers(),
          tolerance = tolerance
        )
        
        # Vérifier que les résultats contiennent des colonnes d'amorces
        primer_cols <- paste0("nb_", get_default_primers())
        has_primer_cols <- any(primer_cols %in% names(results))
        
        if (!has_primer_cols || is.null(results) || nrow(results) == 0) {
          warning("⚠️ Aucune amorce détectée dans les séquences")
          return(NULL)
        }
        
        message("✅ Détection terminée : ", nrow(results), " séquences analysées")
        return(results)
      }, error = function(e) {
        warning("❌ Erreur lors de la détection des amorces : ", e$message)
        return(NULL)
      })
    })
    
    # ==========================================================================
    # INFORMATIONS SUR LA DÉTECTION
    # ==========================================================================
    
    output$detection_info <- renderUI({
      # J'isole les valeurs pour éviter les recalculs inutiles
      data <- isolate(imported_data())
      results <- isolate(detection_results())
      
      if (is.null(data) || nrow(data) == 0) {
        return(
          div(
            style = "padding: 1rem; background-color: #FFF3CD; border-left: 4px solid #FFC107; border-radius: 0.375rem; margin-top: 1rem;",
            p(
              style = "margin: 0; color: #856404;",
              icon("info-circle", style = "margin-right: 0.5rem;"),
              "Importez d'abord un fichier CSV contenant des séquences ADN."
            )
          )
        )
      }
      
      # Vérifier si la détection a été lancée
      if (input$run_detection > 0) {
        if (!is.null(results)) {
          return(
            div(
              style = "padding: 1rem; background-color: #D4EDDA; border-left: 4px solid #28A745; border-radius: 0.375rem; margin-top: 1rem;",
              p(
                style = "margin: 0; color: #155724;",
                icon("check-circle", style = "margin-right: 0.5rem;"),
                tags$strong("Détection terminée !"),
                paste0(" ", nrow(results), " séquences analysées.")
              )
            )
          )
        } else {
          return(
            div(
              style = "padding: 1rem; background-color: #F8D7DA; border-left: 4px solid #DC3545; border-radius: 0.375rem; margin-top: 1rem;",
              p(
                style = "margin: 0; color: #721C24;",
                icon("exclamation-triangle", style = "margin-right: 0.5rem;"),
                "Erreur lors de la détection. Vérifiez que la colonne sélectionnée contient des séquences ADN valides."
              )
            )
          )
        }
      }
      
      return(NULL)
    })
    
    # ==========================================================================
    # PANEL DE RÉSULTATS
    # ==========================================================================
    
    output$results_panel <- renderUI({
      results <- detection_results()
      
      if (is.null(results)) {
        return(NULL)
      }
      
      tagList(
        # Statistiques globales par amorce
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
                "Statistiques par Amorce"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              # Graphique des occurrences totales
              plotlyOutput(ns("plot_total_occurrences"), height = "400px"),
              
              br(),
              
              # Graphique du pourcentage de présence
              plotlyOutput(ns("plot_presence_pct"), height = "400px"),
              
              br(),
              
              # Tableau des statistiques
              DT::dataTableOutput(ns("table_stats"))
            )
          )
        ),
        
        # Heatmap par pays (si colonne pays disponible)
        uiOutput(ns("heatmap_panel")),
        
        # Tableau des résultats détaillés
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
                "Résultats Détaillés"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              DT::dataTableOutput(ns("table_results"))
            )
          )
        )
      )
    })
    
    # ==========================================================================
    # GRAPHIQUE DES OCCURRENCES TOTALES
    # ==========================================================================
    
    output$plot_total_occurrences <- renderPlotly({
      results <- detection_results()
      
      if (is.null(results)) {
        return(plotly_empty())
      }
      
      # J'isole les calculs pour éviter les recalculs inutiles
      tryCatch({
        # Préparer les données au format long
        primers <- get_default_primers()
        primer_cols <- paste0("nb_", primers)
        primer_cols <- primer_cols[primer_cols %in% names(results)]
        
        if (length(primer_cols) == 0) {
          return(plotly_empty())
        }
        
        results_long <- results %>%
          select(all_of(primer_cols)) %>%
          pivot_longer(
            cols = everything(),
            names_to = "primer",
            values_to = "count"
          ) %>%
          mutate(primer = gsub("nb_", "", primer))
        
        # Vérifier que la colonne primer existe avant de faire group_by
        if (!"primer" %in% names(results_long) || nrow(results_long) == 0) {
          return(plotly_empty())
        }
        
        results_long <- results_long %>%
          group_by(primer) %>%
          summarise(total = sum(count, na.rm = TRUE), .groups = 'drop') %>%
          arrange(desc(total))
        
        # Créer le graphique avec optimisation plotly
        p <- ggplot(results_long, aes(x = reorder(primer, -total), y = total, fill = primer)) +
          geom_bar(stat = "identity") +
          theme_academique() +
          labs(
            title = "Nombre Total d'Occurrences par Amorce",
            x = "Amorce",
            y = "Total d'Occurrences",
            fill = "Amorce"
          ) +
          theme(legend.position = "none")
        
        # Optimisation plotly : désactiver certaines fonctionnalités pour améliorer les performances
        ggplotly(p, tooltip = c("x", "y")) %>%
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
    # GRAPHIQUE DU POURCENTAGE DE PRÉSENCE
    # ==========================================================================
    
    output$plot_presence_pct <- renderPlotly({
      results <- detection_results()
      
      if (is.null(results)) {
        return(plotly_empty())
      }
      
      tryCatch({
        # Préparer les données
        primers <- get_default_primers()
        primer_cols <- paste0("nb_", primers)
        primer_cols <- primer_cols[primer_cols %in% names(results)]
        
        if (length(primer_cols) == 0) {
          return(plotly_empty())
        }
        
        results_long <- results %>%
          select(all_of(primer_cols)) %>%
          pivot_longer(
            cols = everything(),
            names_to = "primer",
            values_to = "count"
          ) %>%
          mutate(primer = gsub("nb_", "", primer))
        
        # Vérifier que la colonne primer existe avant de faire group_by
        if (!"primer" %in% names(results_long) || nrow(results_long) == 0) {
          return(plotly_empty())
        }
        
        results_long <- results_long %>%
          group_by(primer) %>%
          summarise(
            presence_pct = mean(count > 0, na.rm = TRUE) * 100,
            .groups = 'drop'
          ) %>%
          arrange(desc(presence_pct))
        
        # Créer le graphique
        p <- ggplot(results_long, aes(x = reorder(primer, -presence_pct), y = presence_pct, fill = primer)) +
          geom_bar(stat = "identity") +
          theme_academique() +
          labs(
            title = "Pourcentage de Séquences Contenant l'Amorce",
            x = "Amorce",
            y = "% de Séquences",
            fill = "Amorce"
          ) +
          ylim(0, 100) +
          theme(legend.position = "none")
        
        # Optimisation plotly
        ggplotly(p, tooltip = c("x", "y")) %>%
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
    # TABLEAU DES STATISTIQUES
    # ==========================================================================
    
    output$table_stats <- DT::renderDataTable({
      results <- detection_results()
      
      if (is.null(results)) {
        return(data.frame())
      }
      
      stats <- calculate_primer_stats(results)
      
      if (nrow(stats) == 0) {
        return(data.frame())
      }
      
      DT::datatable(
        stats,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
        ),
        rownames = FALSE,
        colnames = c(
          "Amorce",
          "Total Occurrences",
          "Moyenne",
          "Médiane",
          "Écart-Type",
          "Minimum",
          "Maximum",
          "% Présence"
        )
      ) %>%
        DT::formatRound(columns = c("moyenne", "mediane", "ecart_type"), digits = 2) %>%
        DT::formatRound(columns = "presence_pct", digits = 1)
    })
    
    # ==========================================================================
    # HEATMAP PAR PAYS
    # ==========================================================================
    
    output$heatmap_panel <- renderUI({
      results <- detection_results()
      
      if (is.null(results)) {
        return(NULL)
      }
      
      # Vérifier si une colonne pays existe
      country_cols <- c("pays", "country", "nom_pays", "pays_code")
      country_col <- NULL
      
      for (col in country_cols) {
        if (col %in% names(results)) {
          country_col <- col
          break
        }
      }
      
      if (is.null(country_col)) {
        return(NULL)
      }
      
      # Vérifier qu'il y a assez de pays
      unique_countries <- unique(results[[country_col]])
      unique_countries <- unique_countries[!is.na(unique_countries)]
      
      if (length(unique_countries) == 0) {
        return(NULL)
      }
      
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
              icon("map", style = "margin-right: 0.5rem;"),
              "Occurrences Moyennes par Pays et par Amorce"
            )
          ),
          
          div(
            class = "card-body",
            style = "padding: 1.5rem;",
            
            plotlyOutput(ns("plot_heatmap_country"), height = "600px")
          )
        )
      )
    })
    
    output$plot_heatmap_country <- renderPlotly({
      results <- detection_results()
      
      if (is.null(results)) {
        return(plotly_empty())
      }
      
      tryCatch({
        # Trouver la colonne pays
        country_cols <- c("pays", "country", "nom_pays", "pays_code")
        country_col <- NULL
        
        for (col in country_cols) {
          if (col %in% names(results)) {
            country_col <- col
            break
          }
        }
        
        if (is.null(country_col)) {
          return(plotly_empty())
        }
        
        # Préparer les données pour la heatmap
        primers <- get_default_primers()
        primer_cols <- paste0("nb_", primers)
        primer_cols <- primer_cols[primer_cols %in% names(results)]
        
        if (length(primer_cols) == 0) {
          return(plotly_empty())
        }
        
        # Vérifier que la colonne pays existe et contient des valeurs
        if (!country_col %in% names(results)) {
          return(plotly_empty())
        }
        
        # Filtrer les valeurs NA pour le pays
        results_country <- results %>%
          filter(!is.na(.data[[country_col]]), .data[[country_col]] != "")
        
        if (nrow(results_country) == 0) {
          return(plotly_empty())
        }
        
        # Top 10 pays par nombre de séquences
        top_countries <- results_country %>%
          group_by(.data[[country_col]]) %>%
          summarise(n = n(), .groups = 'drop') %>%
          arrange(desc(n)) %>%
          head(10) %>%
          pull(.data[[country_col]])
        
        if (length(top_countries) == 0) {
          return(plotly_empty())
        }
        
        # Calculer les moyennes par pays et par amorce
        heatmap_data <- results_country %>%
          filter(.data[[country_col]] %in% top_countries) %>%
          select(all_of(c(country_col, primer_cols))) %>%
          pivot_longer(
            cols = all_of(primer_cols),
            names_to = "primer",
            values_to = "count"
          ) %>%
          mutate(primer = gsub("nb_", "", primer))
        
        # Vérifier que les colonnes nécessaires existent avant group_by
        if (!"primer" %in% names(heatmap_data) || !country_col %in% names(heatmap_data) || nrow(heatmap_data) == 0) {
          return(plotly_empty())
        }
        
        heatmap_data <- heatmap_data %>%
          group_by(.data[[country_col]], primer) %>%
          summarise(mean_count = mean(count, na.rm = TRUE), .groups = 'drop')
        
        # Créer la heatmap avec nom de colonne dynamique
        p <- ggplot(heatmap_data, aes_string(x = "primer", y = country_col, fill = "mean_count")) +
          geom_tile(color = "white", linewidth = 0.5) +
          scale_fill_gradient(low = "white", high = "steelblue", name = "Moyenne") +
          theme_academique() +
          labs(
            title = "Occurrences Moyennes par Pays et par Amorce (Top 10 Pays)",
            x = "Amorce",
            y = "Pays"
          ) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Optimisation plotly
        ggplotly(p, tooltip = c("x", "y", "fill")) %>%
          config(
            displayModeBar = TRUE, 
            displaylogo = FALSE, 
            modeBarButtonsToRemove = c("pan2d", "lasso2d", "select2d", "autoScale2d", "resetScale2d"),
            toImageButtonOptions = list(format = "png", width = 800, height = 600, scale = 1),
            doubleClick = "reset",
            sendData = FALSE
          )
      }, error = function(e) {
        warning("Erreur lors de la création de la heatmap : ", e$message)
        return(plotly_empty())
      })
    })
    
    # ==========================================================================
    # TABLEAU DES RÉSULTATS DÉTAILLÉS
    # ==========================================================================
    
    output$table_results <- DT::renderDataTable({
      results <- detection_results()
      
      if (is.null(results)) {
        return(data.frame())
      }
      
      DT::datatable(
        results,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "500px",
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
        ),
        rownames = FALSE
      )
    })
  })
}
