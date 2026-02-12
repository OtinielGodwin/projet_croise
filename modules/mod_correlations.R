# ==============================================================================
# MODULE ANALYSE DES CORRÉLATIONS
# ==============================================================================
# 
# Module Shiny pour explorer les relations entre variables :
# - gènes
# - facteurs comportementaux
# - marqueurs du cancer
#
# ==============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)

# À ce niveau, je charge theme_shiny.R avant theme_ggplot.R car
# theme_ggplot.R utilise la fonction get_theme_colors() définie dans theme_shiny.R
source("R/theme_shiny.R")
source("R/theme_ggplot.R")
source("R/constants.R")

# ==============================================================================
# UI DU MODULE
# ==============================================================================

#' Interface utilisateur du module analyse des corrélations
#'
#' @param id Identifiant unique du module
#' @return Interface utilisateur du module
mod_correlations_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # Titre de la page
    div(
      class = "container-fluid",
      style = "margin-bottom: 2rem;",
      h1(
        style = "color: #1E5A8E; margin-bottom: 0.5rem;",
        icon("project-diagram", style = "margin-right: 0.5rem;"),
        "Analyse des Corrélations"
      ),
      p(
        style = "color: #6C757D; font-size: 1.1rem;",
        "Explorez les relations entre les variables génétiques, comportementales et les marqueurs du cancer"
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
            
            # Méthode de corrélation
            column(
              width = 3,
              selectInput(
                ns("correlation_method"),
                label = tags$strong("Méthode de corrélation"),
                choices = list(
                  "Pearson" = "pearson",
                  "Spearman" = "spearman"
                ),
                selected = "pearson"
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
              width = 12,
              style = "margin-top: 1rem;",
              uiOutput(ns("variables_selector"))
            )
          )
        )
      )
    ),
    
    # Matrice de corrélation (tableau)
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
            "Matrice de Corrélation"
          )
        ),
        
        div(
          class = "card-body",
          style = "padding: 1.5rem;",
          
          DT::dataTableOutput(ns("correlation_table"), width = "100%")
        )
      )
    ),
    
    # Heatmap de corrélation
    div(
      class = "container-fluid",
      
      div(
        class = "card",
        
        div(
          class = "card-header",
          style = "background-color: #F8F9FA; border-bottom: 2px solid #1E5A8E;",
          h3(
            style = "margin: 0; color: #1E5A8E; font-size: 1.25rem;",
            icon("th", style = "margin-right: 0.5rem;"),
            "Heatmap de Corrélation"
          )
        ),
        
        div(
          class = "card-body",
          style = "padding: 1.5rem;",
          
          plotOutput(ns("correlation_heatmap"), height = "600px")
        )
      )
    )
  )
}

# ==============================================================================
# SERVER DU MODULE
# ==============================================================================

#' Logique serveur du module analyse des corrélations
#'
#' @param id Identifiant unique du module
#' @param genetic_data Données génétiques filtrées (réactif)
#' @param smoking_data Données de tabagisme filtrées (réactif)
#' @param alcohol_data Données d'alcool filtrées (réactif)
#' @param cancer_markers_data Données des marqueurs filtrées (réactif)
#' @param analytics_data Données analytiques filtrées (réactif)
#' @param available_countries Liste des pays disponibles (réactif)
#' @param available_date_range Plage de dates disponible (réactif)
mod_correlations_server <- function(
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
          # Je fusionne les données comportementales si nécessaire
          merged <- merge(smoking_data(), alcohol_data(), by = COL_PATIENT_ID, all = TRUE)
          merged
        },
        "cancer_markers" = cancer_markers_data(),
        "analytics" = analytics_data()
      )
    })
    
    # ==========================================================================
    # IDENTIFICATION DES VARIABLES NUMÉRIQUES DISPONIBLES
    # ==========================================================================
    
    # Ici, je trouve toutes les variables numériques dans les données sélectionnées
    numeric_variables <- reactive({
      data <- selected_data()
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      # Je filtre pour ne garder que les colonnes numériques
      numeric_cols <- sapply(data, is.numeric)
      numeric_cols <- names(data)[numeric_cols]
      
      # Je retire les colonnes d'identifiant et de date
      exclude_cols <- c(COL_PATIENT_ID, COL_GENE_ID, COL_DATE, COL_YEAR)
      numeric_cols <- setdiff(numeric_cols, exclude_cols)
      
      return(numeric_cols)
    })
    
    # ==========================================================================
    # INTERFACE DYNAMIQUE POUR LA SÉLECTION DES VARIABLES
    # ==========================================================================
    
    output$variables_selector <- renderUI({
      ns <- session$ns
      vars <- numeric_variables()
      
      if (is.null(vars) || length(vars) == 0) {
        return(
          p(
            style = "color: #DC3545;",
            "Aucune variable numérique disponible dans ce dataset."
          )
        )
      }
      
      # Je crée une liste nommée avec des labels en français
      choices_list <- as.list(vars)
      names(choices_list) <- vars
      
      checkboxGroupInput(
        ns("selected_variables"),
        label = tags$strong("Variables à analyser (sélectionnez au moins 2)"),
        choices = choices_list,
        selected = if (length(vars) <= 10) vars else vars[1:min(10, length(vars))]
      )
    })
    
    # ==========================================================================
    # INTERFACE DYNAMIQUE POUR LE FILTRE PAR PAYS
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
    
    # ==========================================================================
    # INTERFACE DYNAMIQUE POUR LE FILTRE PAR PÉRIODE
    # ==========================================================================
    
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
    # DONNÉES POUR LE CALCUL DE CORRÉLATION
    # ==========================================================================
    
    # Ici, je prépare les données avec uniquement les variables sélectionnées
    correlation_data <- reactive({
      data <- filtered_data()
      vars <- input$selected_variables
      
      if (is.null(data) || is.null(vars) || length(vars) < 2) {
        return(NULL)
      }
      
      # Je vérifie que toutes les variables existent dans les données
      vars <- vars[vars %in% names(data)]
      
      if (length(vars) < 2) {
        return(NULL)
      }
      
      # Je sélectionne uniquement les variables choisies
      corr_data <- data[, vars, drop = FALSE]
      
      # Je retire les lignes avec des valeurs manquantes pour le calcul de corrélation
      corr_data <- corr_data[complete.cases(corr_data), ]
      
      return(corr_data)
    })
    
    # ==========================================================================
    # CALCUL DE LA MATRICE DE CORRÉLATION
    # ==========================================================================
    
    # Ici, je calcule la matrice de corrélation selon la méthode choisie
    # (Pearson pour relations linéaires, Spearman pour relations monotones).
    correlation_matrix <- reactive({
      data <- correlation_data()
      method <- input$correlation_method
      
      if (is.null(data) || nrow(data) < 2) {
        return(NULL)
      }
      
      # Je calcule la matrice de corrélation
      cor_matrix <- cor(data, method = method, use = "complete.obs")
      
      return(cor_matrix)
    })
    
    # ==========================================================================
    # AFFICHAGE DU TABLEAU DE CORRÉLATION
    # ==========================================================================
    
    output$correlation_table <- DT::renderDataTable({
      cor_matrix <- correlation_matrix()
      
      if (is.null(cor_matrix)) {
        return(data.frame(
          Message = "Sélectionnez au moins 2 variables numériques pour calculer les corrélations."
        ))
      }
      
      # Je convertis la matrice en dataframe pour l'affichage dans le tableau.
      # Les valeurs sont arrondies à 3 décimales pour la lisibilité.
      cor_df <- as.data.frame(cor_matrix)
      cor_df <- round(cor_df, 3)
      
      # J'ajoute les noms de lignes comme première colonne
      cor_df <- cbind(Variable = rownames(cor_df), cor_df)
      
      DT::datatable(
        cor_df,
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
          names(cor_df)[-1],
          backgroundColor = DT::styleInterval(
            c(-0.5, 0, 0.5),
            c("#DC3545", "#FFFFFF", "#28A745", "#1E5A8E")
          )
        )
    })
    
    # ==========================================================================
    # CRÉATION DE LA HEATMAP DE CORRÉLATION
    # ==========================================================================
    
    # Ici, je crée une visualisation claire de la matrice de corrélation sous
    # forme de heatmap pour faciliter l'identification des relations fortes.
    output$correlation_heatmap <- renderPlot({
      cor_matrix <- correlation_matrix()
      
      if (is.null(cor_matrix)) {
        # Je crée un graphique vide avec un message informatif
        colors <- get_theme_colors()
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Sélectionnez au moins 2 variables pour afficher la heatmap", 
                   size = 5, color = colors$text_secondary) +
          theme_void() +
          theme(plot.background = element_rect(fill = colors$bg_white))
        return(p)
      }
      
      # Je convertis la matrice en format long pour pouvoir créer la heatmap avec ggplot2.
      # Cette transformation permet de représenter chaque corrélation comme un point dans le graphique.
      cor_df <- as.data.frame(cor_matrix)
      cor_df$Var1 <- rownames(cor_df)
      
      cor_long <- pivot_longer(
        cor_df,
        cols = -Var1,
        names_to = "Var2",
        values_to = "Correlation"
      )
      
      # Je récupère les couleurs du thème
      colors <- get_theme_colors()
      
      # Je crée la heatmap avec ggplot2
      p <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
        geom_tile(color = "white", size = 0.5) +
        scale_fill_gradient2(
          low = "#DC3545",      # Rouge pour corrélations négatives
          mid = "#FFFFFF",      # Blanc pour corrélations nulles
          high = "#1E5A8E",      # Bleu pour corrélations positives
          midpoint = 0,
          limits = c(-1, 1),
          name = "Corrélation"
        ) +
        theme_academique() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_blank(),
          legend.position = "right",
          plot.title = element_text(hjust = 0.5)
        ) +
        labs(
          title = paste("Matrice de corrélation (méthode", 
                       ifelse(input$correlation_method == "pearson", "Pearson", "Spearman"), ")"),
          subtitle = paste("Nombre de variables :", nrow(cor_matrix)),
          fill = "Corrélation"
        )
      
      return(p)
    })
    
  })
}
