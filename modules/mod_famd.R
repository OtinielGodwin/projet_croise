# ==============================================================================
# MODULE FACTOR ANALYSIS OF MIXED DATA (FAMD)
# ==============================================================================
# 
# Module Shiny pour l'analyse factorielle de données mixtes
# Utilise FactoMineR pour le calcul et factoextra pour la visualisation
# 
# QUAND UTILISER LA FAMD :
# - Vous avez des variables qualitatives ET quantitatives
# - Vous voulez analyser les relations entre tous les types de variables
# - Vous voulez réduire la dimensionnalité de données mixtes
#
# ==============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(FactoMineR)
library(factoextra)

# À ce niveau, je charge theme_shiny.R avant theme_ggplot.R car
# theme_ggplot.R utilise la fonction get_theme_colors() définie dans theme_shiny.R
source("R/theme_shiny.R")
source("R/theme_ggplot.R")
source("R/constants.R")

# ==============================================================================
# UI DU MODULE
# ==============================================================================

#' Interface utilisateur du module FAMD
#'
#' @param id Identifiant unique du module
#' @return Interface utilisateur du module
mod_famd_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # Titre de la page
    div(
      class = "container-fluid",
      style = "margin-bottom: 2rem;",
      h1(
        style = "color: #1E5A8E; margin-bottom: 0.5rem;",
        icon("network-wired", style = "margin-right: 0.5rem;"),
        "Factor Analysis of Mixed Data (FAMD)"
      ),
      p(
        style = "color: #6C757D; font-size: 1.1rem;",
        "Analyse factorielle pour données mixtes (qualitatives et quantitatives)"
      ),
      div(
        style = "background-color: #E7F3FF; border-left: 4px solid #17A2B8; padding: 1rem; margin-top: 1rem; border-radius: 0.375rem;",
        p(
          style = "margin: 0; color: #0C5460; font-size: 0.95rem;",
          icon("info-circle", style = "margin-right: 0.5rem;"),
          tags$strong("Quand utiliser la FAMD :"),
          " Cette méthode est adaptée lorsque vous avez ",
          tags$strong("des variables qualitatives ET quantitatives"),
          " et que vous souhaitez analyser leurs relations simultanément."
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
            "Configuration de l'analyse"
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
            
            # Sélection des axes
            column(
              width = 3,
              fluidRow(
                column(6, numericInput(ns("axis1"), label = tags$strong("Axe 1"), value = 1, min = 1, max = 10, step = 1)),
                column(6, numericInput(ns("axis2"), label = tags$strong("Axe 2"), value = 2, min = 1, max = 10, step = 1))
              )
            ),
            
            # Filtre par pays
            column(
              width = 3,
              uiOutput(ns("country_selector"))
            ),
            
            # Filtre par période
            column(
              width = 3,
              uiOutput(ns("date_range_selector"))
            )
          ),
          
          # Sélection des variables
          fluidRow(
            column(
              width = 6,
              style = "margin-top: 1rem;",
              uiOutput(ns("quantitative_variables_selector"))
            ),
            column(
              width = 6,
              style = "margin-top: 1rem;",
              uiOutput(ns("qualitative_variables_selector"))
            )
          )
        )
      )
    ),
    
    # Interprétation des axes
    div(
      class = "container-fluid",
      style = "margin-bottom: 2rem;",
      
      fluidRow(
        # Graphique des valeurs propres
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
                icon("chart-line", style = "margin-right: 0.5rem;"),
                "Valeurs Propres"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              plotOutput(ns("eigenvalues_plot"), height = "400px")
            )
          )
        ),
        
        # Tableau des valeurs propres
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
                "Interprétation des Axes"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              DT::dataTableOutput(ns("eigenvalues_table"), width = "100%")
            )
          )
        )
      )
    ),
    
    # Projection des individus
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
            icon("users", style = "margin-right: 0.5rem;"),
            "Projection des Individus"
          )
        ),
        
        div(
          class = "card-body",
          style = "padding: 1.5rem;",
          
          plotOutput(ns("individuals_plot"), height = "500px")
        )
      )
    ),
    
    # Visualisation des variables
    div(
      class = "container-fluid",
      
      fluidRow(
        # Variables quantitatives
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
                "Variables Quantitatives"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              plotOutput(ns("quantitative_variables_plot"), height = "500px")
            )
          )
        ),
        
        # Variables qualitatives
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
                icon("sitemap", style = "margin-right: 0.5rem;"),
                "Variables Qualitatives"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              plotOutput(ns("qualitative_variables_plot"), height = "500px")
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

#' Logique serveur du module FAMD
#'
#' @param id Identifiant unique du module
#' @param genetic_data Données génétiques filtrées (réactif)
#' @param smoking_data Données de tabagisme filtrées (réactif)
#' @param alcohol_data Données d'alcool filtrées (réactif)
#' @param cancer_markers_data Données des marqueurs filtrées (réactif)
#' @param analytics_data Données analytiques filtrées (réactif)
#' @param available_countries Liste des pays disponibles (réactif)
#' @param available_date_range Plage de dates disponible (réactif)
mod_famd_server <- function(
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
    # IDENTIFICATION DES VARIABLES QUANTITATIVES ET QUALITATIVES
    # ==========================================================================
    
    # À ce niveau, je sépare les variables quantitatives et qualitatives.
    # La FAMD nécessite les deux types de variables pour analyser leurs relations simultanément.
    quantitative_variables <- reactive({
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
    
    # Ici, je trouve toutes les variables qualitatives
    qualitative_variables <- reactive({
      data <- selected_data()
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      qual_cols <- sapply(data, function(x) is.character(x) || is.factor(x))
      qual_cols <- names(data)[qual_cols]
      
      exclude_cols <- c(COL_PATIENT_ID, COL_GENE_ID, COL_DATE, COL_YEAR)
      qual_cols <- setdiff(qual_cols, exclude_cols)
      
      return(qual_cols)
    })
    
    # ==========================================================================
    # INTERFACE DYNAMIQUE POUR LA SÉLECTION DES VARIABLES
    # ==========================================================================
    
    output$quantitative_variables_selector <- renderUI({
      ns <- session$ns
      vars <- quantitative_variables()
      
      if (is.null(vars) || length(vars) == 0) {
        return(
          p(
            style = "color: #6C757D; font-size: 0.9rem;",
            "Aucune variable quantitative disponible."
          )
        )
      }
      
      choices_list <- as.list(vars)
      names(choices_list) <- vars
      
      checkboxGroupInput(
        ns("selected_quantitative_vars"),
        label = tags$strong("Variables quantitatives"),
        choices = choices_list,
        selected = if (length(vars) <= 5) vars else vars[1:min(5, length(vars))]
      )
    })
    
    output$qualitative_variables_selector <- renderUI({
      ns <- session$ns
      vars <- qualitative_variables()
      
      if (is.null(vars) || length(vars) == 0) {
        return(
          p(
            style = "color: #6C757D; font-size: 0.9rem;",
            "Aucune variable qualitative disponible."
          )
        )
      }
      
      choices_list <- as.list(vars)
      names(choices_list) <- vars
      
      checkboxGroupInput(
        ns("selected_qualitative_vars"),
        label = tags$strong("Variables qualitatives"),
        choices = choices_list,
        selected = if (length(vars) <= 5) vars else vars[1:min(5, length(vars))]
      )
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
    # PRÉPARATION DES DONNÉES POUR LA FAMD
    # ==========================================================================
    
    # Ici, je prépare les données avec les variables quantitatives et qualitatives
    # sélectionnées. La FAMD peut fonctionner avec seulement un type de variable,
    # mais l'idéal est d'avoir les deux types pour analyser leurs relations.
    famd_data <- reactive({
      data <- filtered_data()
      quant_vars <- input$selected_quantitative_vars
      qual_vars <- input$selected_qualitative_vars
      
      if (is.null(data) || (is.null(quant_vars) && is.null(qual_vars))) {
        return(NULL)
      }
      
      # Je vérifie qu'au moins une variable de chaque type est sélectionnée
      if (length(quant_vars) == 0 && length(qual_vars) == 0) {
        return(NULL)
      }
      
      # Je sélectionne les variables choisies
      selected_vars <- c(quant_vars, qual_vars)
      selected_vars <- selected_vars[selected_vars %in% names(data)]
      
      if (length(selected_vars) == 0) {
        return(NULL)
      }
      
      famd_data <- data[, selected_vars, drop = FALSE]
      
      # Je retire les lignes avec trop de valeurs manquantes
      # Pour la FAMD, je garde les lignes avec au moins 50% de données complètes
      complete_rows <- rowSums(!is.na(famd_data)) >= ncol(famd_data) * 0.5
      famd_data <- famd_data[complete_rows, ]
      
      if (nrow(famd_data) < 2) {
        return(NULL)
      }
      
      return(famd_data)
    })
    
    # ==========================================================================
    # CALCUL DE LA FAMD
    # ==========================================================================
    
    # Ici, je calcule la FAMD avec FactoMineR. Cette méthode combine les avantages
    # de l'ACP (pour les variables quantitatives) et de l'ACM (pour les variables
    # qualitatives) pour analyser simultanément les deux types de variables.
    
    # Ici, je calcule la FAMD avec FactoMineR. Cette méthode combine les avantages
    # de l'ACP (pour les variables quantitatives) et de l'ACM (pour les variables
    # qualitatives) pour analyser simultanément les deux types de variables.
    famd_result <- reactive({
      data <- famd_data()
      quant_vars <- input$selected_quantitative_vars
      qual_vars <- input$selected_qualitative_vars
      
      if (is.null(data) || nrow(data) < 2) {
        return(NULL)
      }
      
      # Je détermine quelles colonnes sont quantitatives et qualitatives
      quant_cols <- quant_vars[quant_vars %in% names(data)]
      qual_cols <- qual_vars[qual_vars %in% names(data)]
      
      # Je calcule la FAMD avec FactoMineR
      # graph = FALSE pour éviter l'affichage automatique
      famd <- FAMD(data, graph = FALSE, ncp = 10)
      
      return(famd)
    })
    
    # ==========================================================================
    # GRAPHIQUE DES VALEURS PROPRES
    # ==========================================================================
    
    # Ici, je crée le graphique des valeurs propres (scree plot) qui permet
    # d'identifier le nombre d'axes à retenir pour l'interprétation de la FAMD.
    output$eigenvalues_plot <- renderPlot({
      famd <- famd_result()
      
      if (is.null(famd)) {
        return(NULL)
      }
      
      # Je crée le graphique des valeurs propres avec factoextra
      p <- fviz_eig(famd, addlabels = TRUE, ylim = c(0, 50))
      
      # J'applique le thème académique
      colors <- get_theme_colors()
      p <- p + 
        theme_academique() +
        labs(
          title = "Valeurs propres de la FAMD",
          subtitle = "Pourcentage de variance expliquée par chaque axe",
          x = "Composante principale",
          y = "Pourcentage de variance expliquée (%)"
        ) +
        theme(
          plot.title = element_text(color = colors$text_primary, hjust = 0.5),
          plot.subtitle = element_text(color = colors$text_secondary, hjust = 0.5)
        )
      
      return(p)
    })
    
    # ==========================================================================
    # TABLEAU DES VALEURS PROPRES
    # ==========================================================================
    
    output$eigenvalues_table <- DT::renderDataTable({
      famd <- famd_result()
      
      if (is.null(famd)) {
        return(data.frame(
          Message = "Sélectionnez au moins une variable quantitative et une variable qualitative pour effectuer la FAMD."
        ))
      }
      
      # Je récupère les valeurs propres et le pourcentage de variance avec gestion d'erreur
      eig <- tryCatch({
        eig_result <- get_eigenvalue(famd)
        if (is.null(eig_result)) {
          return(NULL)
        }
        if (is.atomic(eig_result) && !is.null(names(eig_result))) {
          warning("⚠️ get_eigenvalue retourne un vecteur atomique")
          return(NULL)
        }
        if (is.data.frame(eig_result) || is.matrix(eig_result)) {
          if (!"eigenvalue" %in% names(eig_result) && !"eigenvalue" %in% colnames(eig_result)) {
            warning("⚠️ Colonne 'eigenvalue' non trouvée")
            return(NULL)
          }
          return(eig_result)
        }
        return(NULL)
      }, error = function(e) {
        warning("❌ Erreur lors de la récupération des valeurs propres : ", e$message)
        return(NULL)
      })
      
      if (is.null(eig)) {
        return(data.frame(
          Message = "Erreur lors du calcul des valeurs propres."
        ))
      }
      
      n_axes <- nrow(eig)
      if (is.null(n_axes) || n_axes == 0) {
        return(data.frame(
          Message = "Aucune valeur propre disponible."
        ))
      }
      
      # Je crée un dataframe avec l'interprétation
      eig_df <- data.frame(
        Axe = rownames(eig),
        "Valeur propre" = if ("eigenvalue" %in% names(eig)) round(eig$eigenvalue, 3) else round(eig[, "eigenvalue"], 3),
        "Pourcentage de variance" = if ("variance.percent" %in% names(eig)) round(eig$variance.percent, 2) else round(eig[, "variance.percent"], 2),
        "Pourcentage cumulé" = if ("cumulative.variance.percent" %in% names(eig)) round(eig$cumulative.variance.percent, 2) else round(eig[, "cumulative.variance.percent"], 2),
        check.names = FALSE
      )
      
      DT::datatable(
        eig_df,
        options = list(
          pageLength = 10,
          dom = 't',
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
        ),
        rownames = FALSE,
        class = "display nowrap"
      ) %>%
        DT::formatStyle(
          "Axe",
          fontWeight = "bold",
          color = "#1E5A8E"
        )
    })
    
    # ==========================================================================
    # PROJECTION DES INDIVIDUS
    # ==========================================================================
    
    # Ici, je crée la projection des individus sur les axes sélectionnés.
    # Cette visualisation permet d'identifier les groupes d'individus similaires
    # en tenant compte à la fois des variables quantitatives et qualitatives.
    output$individuals_plot <- renderPlot({
      famd <- famd_result()
      axis1 <- input$axis1
      axis2 <- input$axis2
      
      if (is.null(famd)) {
        return(NULL)
      }
      
      # Je vérifie la structure de l'objet FAMD
      # Parfois famd$ind peut être directement une matrice ou un vecteur atomique
      if (is.null(famd$ind)) {
        return(NULL)
      }
      
      # Je récupère les coordonnées des individus
      ind_coord <- if (is.list(famd$ind) && !is.null(famd$ind$coord)) {
        famd$ind$coord
      } else if (is.matrix(famd$ind) || is.data.frame(famd$ind)) {
        famd$ind
      } else {
        return(NULL)
      }
      
      # Je vérifie que les axes demandés existent
      n_axes <- ncol(ind_coord)
      if (is.null(n_axes) || axis1 > n_axes || axis2 > n_axes || axis1 < 1 || axis2 < 1) {
        return(NULL)
      }
      
      # Je crée le graphique de projection des individus avec factoextra
      p <- fviz_famd_ind(
        famd,
        axes = c(axis1, axis2),
        geom = "point",
        col.ind = "#1E5A8E",
        pointshape = 19,
        pointsize = 2,
        alpha.ind = 0.6
      )
      
      # J'applique le thème académique
      colors <- get_theme_colors()
      p <- p + 
        theme_academique() +
        labs(
          title = paste("Projection des individus sur les axes", axis1, "et", axis2),
          subtitle = paste(
            "Axe", axis1, ":", round(get_eigenvalue(famd)$variance.percent[axis1], 2), "% de variance | ",
            "Axe", axis2, ":", round(get_eigenvalue(famd)$variance.percent[axis2], 2), "% de variance"
          )
        ) +
        theme(
          plot.title = element_text(color = colors$text_primary, hjust = 0.5),
          plot.subtitle = element_text(color = colors$text_secondary, hjust = 0.5)
        )
      
      return(p)
    })
    
    # ==========================================================================
    # VISUALISATION DES VARIABLES QUANTITATIVES
    # ==========================================================================
    
    # Ici, je crée la visualisation des variables quantitatives dans l'espace factoriel.
    # Cette représentation montre comment les variables quantitatives contribuent
    # aux axes principaux et leurs relations mutuelles.
    output$quantitative_variables_plot <- renderPlot({
      famd <- famd_result()
      axis1 <- input$axis1
      axis2 <- input$axis2
      
      if (is.null(famd)) {
        return(NULL)
      }
      
      # Je vérifie la structure de l'objet FAMD pour les variables quantitatives
      if (is.null(famd$quanti.var)) {
        return(NULL)
      }
      
      # Je récupère les coordonnées des variables quantitatives
      quanti_coord <- if (is.list(famd$quanti.var) && !is.null(famd$quanti.var$coord)) {
        famd$quanti.var$coord
      } else if (is.matrix(famd$quanti.var) || is.data.frame(famd$quanti.var)) {
        famd$quanti.var
      } else {
        return(NULL)
      }
      
      # Je vérifie que les axes demandés existent
      n_axes <- ncol(quanti_coord)
      if (is.null(n_axes) || axis1 > n_axes || axis2 > n_axes || axis1 < 1 || axis2 < 1) {
        return(NULL)
      }
      
      # Je crée le graphique des variables quantitatives avec factoextra
      p <- fviz_famd_var(
        famd,
        choice = "quanti.var",
        axes = c(axis1, axis2),
        col.var = "contrib",
        gradient.cols = c("#FFFFFF", "#4A7BA7", "#1E5A8E"),
        repel = TRUE
      )
      
      # J'applique le thème académique
      colors <- get_theme_colors()
      p <- p + 
        theme_academique() +
        labs(
          title = paste("Variables quantitatives - Axes", axis1, "et", axis2),
          subtitle = "Couleur = contribution à l'axe",
          color = "Contribution"
        ) +
        theme(
          plot.title = element_text(color = colors$text_primary, hjust = 0.5),
          plot.subtitle = element_text(color = colors$text_secondary, hjust = 0.5)
        )
      
      return(p)
    })
    
    # ==========================================================================
    # VISUALISATION DES VARIABLES QUALITATIVES
    # ==========================================================================
    
    # Ici, je crée la visualisation des variables qualitatives dans l'espace factoriel.
    # Cette représentation montre comment les modalités des variables qualitatives
    # contribuent aux axes principaux et leurs associations.
    output$qualitative_variables_plot <- renderPlot({
      famd <- famd_result()
      axis1 <- input$axis1
      axis2 <- input$axis2
      
      if (is.null(famd)) {
        return(NULL)
      }
      
      # Je vérifie la structure de l'objet FAMD pour les variables qualitatives
      if (is.null(famd$quali.var)) {
        return(NULL)
      }
      
      # Je récupère les coordonnées des variables qualitatives
      quali_coord <- if (is.list(famd$quali.var) && !is.null(famd$quali.var$coord)) {
        famd$quali.var$coord
      } else if (is.matrix(famd$quali.var) || is.data.frame(famd$quali.var)) {
        famd$quali.var
      } else {
        return(NULL)
      }
      
      # Je vérifie que les axes demandés existent
      n_axes <- ncol(quali_coord)
      if (is.null(n_axes) || n_axes < max(axis1, axis2)) {
        return(NULL)
      }
      
      # Je crée le graphique des variables qualitatives avec factoextra
      p <- fviz_famd_var(
        famd,
        choice = "quali.var",
        axes = c(axis1, axis2),
        col.var = "contrib",
        gradient.cols = c("#FFFFFF", "#DC3545", "#1E5A8E"),
        repel = TRUE
      )
      
      # J'applique le thème académique
      colors <- get_theme_colors()
      p <- p + 
        theme_academique() +
        labs(
          title = paste("Variables qualitatives - Axes", axis1, "et", axis2),
          subtitle = "Couleur = contribution à l'axe",
          color = "Contribution"
        ) +
        theme(
          plot.title = element_text(color = colors$text_primary, hjust = 0.5),
          plot.subtitle = element_text(color = colors$text_secondary, hjust = 0.5)
        )
      
      return(p)
    })
    
  })
}
