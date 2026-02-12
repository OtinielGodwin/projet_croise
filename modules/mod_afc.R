# ==============================================================================
# MODULE ANALYSE FACTORIELLE DES CORRESPONDANCES (AFC)
# ==============================================================================
# 
# Module Shiny pour l'analyse factorielle des correspondances
# Utilise FactoMineR pour le calcul et factoextra pour la visualisation
# 
# QUAND UTILISER L'AFC :
# - Vous avez deux variables qualitatives (catégorielles)
# - Vous voulez analyser les associations entre les catégories
# - Vous voulez visualiser les proximités entre modalités
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

#' Interface utilisateur du module AFC
#'
#' @param id Identifiant unique du module
#' @return Interface utilisateur du module
mod_afc_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # Titre de la page
    div(
      class = "container-fluid",
      style = "margin-bottom: 2rem;",
      h1(
        style = "color: #1E5A8E; margin-bottom: 0.5rem;",
        icon("sitemap", style = "margin-right: 0.5rem;"),
        "Analyse Factorielle des Correspondances (AFC)"
      ),
      p(
        style = "color: #6C757D; font-size: 1.1rem;",
        "Analyse des associations entre deux variables qualitatives"
      ),
      div(
        style = "background-color: #E7F3FF; border-left: 4px solid #17A2B8; padding: 1rem; margin-top: 1rem; border-radius: 0.375rem;",
        p(
          style = "margin: 0; color: #0C5460; font-size: 0.95rem;",
          icon("info-circle", style = "margin-right: 0.5rem;"),
          tags$strong("Quand utiliser l'AFC :"),
          " Cette méthode est adaptée lorsque vous avez ",
          tags$strong("deux variables qualitatives"),
          " et que vous souhaitez analyser les associations entre leurs catégories."
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
            
            # Sélection de la première variable qualitative
            column(
              width = 3,
              uiOutput(ns("variable1_selector"))
            ),
            
            # Sélection de la deuxième variable qualitative
            column(
              width = 3,
              uiOutput(ns("variable2_selector"))
            ),
            
            # Sélection des axes
            column(
              width = 3,
              fluidRow(
                column(6, numericInput(ns("axis1"), label = tags$strong("Axe 1"), value = 1, min = 1, max = 10, step = 1)),
                column(6, numericInput(ns("axis2"), label = tags$strong("Axe 2"), value = 2, min = 1, max = 10, step = 1))
              )
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
    
    # Tableau de contingence
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
            icon("table", style = "margin-right: 0.5rem;"),
            "Tableau de Contingence"
          )
        ),
        
        div(
          class = "card-body",
          style = "padding: 1.5rem;",
          
          DT::dataTableOutput(ns("contingency_table"), width = "100%")
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
    
    # Projection des modalités
    div(
      class = "container-fluid",
      
      div(
        class = "card",
        
        div(
          class = "card-header",
          style = "background-color: #F8F9FA; border-bottom: 2px solid #1E5A8E;",
          h3(
            style = "margin: 0; color: #1E5A8E; font-size: 1.25rem;",
            icon("project-diagram", style = "margin-right: 0.5rem;"),
            "Projection des Modalités"
          )
        ),
        
        div(
          class = "card-body",
          style = "padding: 1.5rem;",
          
          plotOutput(ns("categories_plot"), height = "500px")
        )
      )
    )
  )
}

# ==============================================================================
# SERVER DU MODULE
# ==============================================================================

#' Logique serveur du module AFC
#'
#' @param id Identifiant unique du module
#' @param genetic_data Données génétiques filtrées (réactif)
#' @param smoking_data Données de tabagisme filtrées (réactif)
#' @param alcohol_data Données d'alcool filtrées (réactif)
#' @param cancer_markers_data Données des marqueurs filtrées (réactif)
#' @param analytics_data Données analytiques filtrées (réactif)
#' @param available_countries Liste des pays disponibles (réactif)
#' @param available_date_range Plage de dates disponible (réactif)
mod_afc_server <- function(
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
    # IDENTIFICATION DES VARIABLES QUALITATIVES DISPONIBLES
    # ==========================================================================
    
    # À ce niveau, je détecte automatiquement toutes les variables qualitatives
    # (caractères ou facteurs) disponibles dans le dataset. L'AFC nécessite
    # exactement deux variables qualitatives pour analyser leurs associations.
    qualitative_variables <- reactive({
      data <- selected_data()
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      # Je filtre pour ne garder que les colonnes qualitatives (character ou factor)
      qual_cols <- sapply(data, function(x) is.character(x) || is.factor(x))
      qual_cols <- names(data)[qual_cols]
      
      # Je retire les colonnes d'identifiant et de date
      exclude_cols <- c(COL_PATIENT_ID, COL_GENE_ID, COL_DATE, COL_YEAR)
      qual_cols <- setdiff(qual_cols, exclude_cols)
      
      return(qual_cols)
    })
    
    # ==========================================================================
    # INTERFACE DYNAMIQUE POUR LA SÉLECTION DES VARIABLES
    # ==========================================================================
    
    output$variable1_selector <- renderUI({
      ns <- session$ns
      vars <- qualitative_variables()
      
      if (is.null(vars) || length(vars) == 0) {
        return(
          selectInput(
            ns("variable1"),
            label = tags$strong("Variable 1"),
            choices = list("Aucune variable disponible" = ""),
            selected = ""
          )
        )
      }
      
      choices_list <- as.list(vars)
      names(choices_list) <- vars
      
      selectInput(
        ns("variable1"),
        label = tags$strong("Première variable qualitative"),
        choices = choices_list,
        selected = if (length(vars) > 0) vars[1] else NULL
      )
    })
    
    output$variable2_selector <- renderUI({
      ns <- session$ns
      vars <- qualitative_variables()
      
      if (is.null(vars) || length(vars) == 0) {
        return(
          selectInput(
            ns("variable2"),
            label = tags$strong("Variable 2"),
            choices = list("Aucune variable disponible" = ""),
            selected = ""
          )
        )
      }
      
      choices_list <- as.list(vars)
      names(choices_list) <- vars
      
      # Je retire la variable 1 de la liste pour éviter de sélectionner la même
      vars_filtered <- setdiff(vars, input$variable1)
      
      selectInput(
        ns("variable2"),
        label = tags$strong("Deuxième variable qualitative"),
        choices = as.list(vars_filtered),
        selected = if (length(vars_filtered) > 0) vars_filtered[1] else NULL
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
    # PRÉPARATION DES DONNÉES POUR L'AFC
    # ==========================================================================
    
    # Ici, je crée le tableau de contingence croisant les deux variables qualitatives
    # sélectionnées. Ce tableau est la base de l'analyse AFC.
    contingency_data <- reactive({
      data <- filtered_data()
      var1 <- input$variable1
      var2 <- input$variable2
      
      if (is.null(data) || is.null(var1) || is.null(var2) || 
          var1 == "" || var2 == "" || 
          !var1 %in% names(data) || !var2 %in% names(data)) {
        return(NULL)
      }
      
      # Je crée le tableau de contingence
      contingency_table <- table(data[[var1]], data[[var2]])
      
      return(contingency_table)
    })
    
    # ==========================================================================
    # CALCUL DE L'AFC
    # ==========================================================================
    
    # Ici, je calcule l'AFC avec FactoMineR à partir du tableau de contingence.
    # L'AFC permet d'analyser les associations entre les catégories des deux variables.
    afc_result <- reactive({
      contingency_table <- contingency_data()
      
      if (is.null(contingency_table) || nrow(contingency_table) < 2 || ncol(contingency_table) < 2) {
        return(NULL)
      }
      
      # Je calcule l'AFC avec FactoMineR
      # graph = FALSE pour éviter l'affichage automatique
      afc <- CA(contingency_table, graph = FALSE)
      
      return(afc)
    })
    
    # ==========================================================================
    # AFFICHAGE DU TABLEAU DE CONTINGENCE
    # ==========================================================================
    
    output$contingency_table <- DT::renderDataTable({
      contingency_table <- contingency_data()
      
      if (is.null(contingency_table)) {
        return(data.frame(
          Message = "Sélectionnez deux variables qualitatives pour créer le tableau de contingence."
        ))
      }
      
      # Je convertis le tableau en dataframe pour l'affichage
      contingency_df <- as.data.frame.matrix(contingency_table)
      contingency_df <- cbind(Modalité = rownames(contingency_df), contingency_df)
      
      DT::datatable(
        contingency_df,
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
          "Modalité",
          fontWeight = "bold",
          color = "#1E5A8E"
        )
    })
    
    # ==========================================================================
    # GRAPHIQUE DES VALEURS PROPRES
    # ==========================================================================
    
    # Ici, je crée le graphique des valeurs propres (scree plot) qui permet
    # d'identifier le nombre d'axes à retenir pour l'interprétation de l'AFC.
    output$eigenvalues_plot <- renderPlot({
      afc <- afc_result()
      
      if (is.null(afc)) {
        return(NULL)
      }
      
      # Je crée le graphique des valeurs propres avec factoextra
      p <- fviz_eig(afc, addlabels = TRUE, ylim = c(0, 50))
      
      # J'applique le thème académique
      colors <- get_theme_colors()
      p <- p + 
        theme_academique() +
        labs(
          title = "Valeurs propres de l'AFC",
          subtitle = "Pourcentage de variance expliquée par chaque axe",
          x = "Axe",
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
      afc <- afc_result()
      
      if (is.null(afc)) {
        return(data.frame(
          Message = "Sélectionnez deux variables qualitatives pour effectuer l'AFC."
        ))
      }
      
      # Je récupère les valeurs propres et le pourcentage de variance avec gestion d'erreur
      eig <- tryCatch({
        eig_result <- get_eigenvalue(afc)
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
    # PROJECTION DES MODALITÉS
    # ==========================================================================
    
    # Ici, je crée la projection des modalités des deux variables qualitatives.
    # Cette visualisation permet d'identifier les associations entre catégories :
    # les modalités proches sont associées, les modalités opposées sont dissociées.
    output$categories_plot <- renderPlot({
      afc <- afc_result()
      axis1 <- input$axis1
      axis2 <- input$axis2
      
      if (is.null(afc)) {
        return(NULL)
      }
      
      # Je vérifie que les axes demandés existent
      n_axes <- ncol(afc$row$coord)
      if (axis1 > n_axes || axis2 > n_axes || axis1 < 1 || axis2 < 1) {
        return(NULL)
      }
      
      # Je crée le graphique de projection des modalités avec factoextra
      p <- fviz_ca_biplot(
        afc,
        axes = c(axis1, axis2),
        repel = TRUE,
        col.row = "#1E5A8E",
        col.col = "#DC3545"
      )
      
      # J'applique le thème académique
      colors <- get_theme_colors()
      p <- p + 
        theme_academique() +
        labs(
          title = paste("Projection des modalités sur les axes", axis1, "et", axis2),
          subtitle = paste(
            "Bleu = modalités de", input$variable1, "| ",
            "Rouge = modalités de", input$variable2
          ),
          caption = "Les modalités proches sont associées. Les modalités opposées sont dissociées."
        ) +
        theme(
          plot.title = element_text(color = colors$text_primary, hjust = 0.5),
          plot.subtitle = element_text(color = colors$text_secondary, hjust = 0.5),
          plot.caption = element_text(color = colors$text_muted, hjust = 0.5, size = 10)
        )
      
      return(p)
    })
    
  })
}
