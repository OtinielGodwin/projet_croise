# ==============================================================================
# MODULE STATISTIQUES DESCRIPTIVES
# ==============================================================================
# 
# Module Shiny pour l'affichage des statistiques descriptives
# avec choix des variables, filtrage et visualisations.
#
# ==============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# À ce niveau, je charge theme_shiny.R avant theme_ggplot.R car
# theme_ggplot.R utilise la fonction get_theme_colors() définie dans theme_shiny.R
source("R/theme_shiny.R")
source("R/theme_ggplot.R")
source("R/constants.R")

# ==============================================================================
# UI DU MODULE
# ==============================================================================

#' Interface utilisateur du module statistiques descriptives
#'
#' @param id Identifiant unique du module
#' @return Interface utilisateur du module
mod_stats_descriptives_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # Titre de la page
    div(
      class = "container-fluid",
      style = "margin-bottom: 2rem;",
      h1(
        style = "color: #1E5A8E; margin-bottom: 0.5rem;",
        icon("chart-bar", style = "margin-right: 0.5rem;"),
        "Statistiques Descriptives"
      ),
      p(
        style = "color: #6C757D; font-size: 1.1rem;",
        "Explorez les distributions et les statistiques descriptives des variables"
      )
    ),
    
    # Panneau de filtres et sélection
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
            icon("filter", style = "margin-right: 0.5rem;"),
            "Filtres et Sélection"
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
            
            # Filtre par pays
            column(
              width = 4,
              uiOutput(ns("country_selector"))
            ),
            
            # Filtre par période
            column(
              width = 4,
              uiOutput(ns("date_range_selector"))
            )
          )
        )
      )
    ),
    
    # Statistiques descriptives
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
            "Statistiques Descriptives"
          )
        ),
        
        div(
          class = "card-body",
          style = "padding: 1.5rem;",
          
          # Tableau des statistiques
          DT::dataTableOutput(ns("stats_table"), width = "100%")
        )
      )
    ),
    
    # Graphiques de distribution
    div(
      class = "container-fluid",
      
      fluidRow(
        # Histogramme
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
                "Distribution (Histogramme)"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              plotOutput(ns("histogram"), height = "400px")
            )
          )
        ),
        
        # Boxplot
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
                icon("chart-box", style = "margin-right: 0.5rem;"),
                "Distribution (Boxplot)"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              plotOutput(ns("boxplot"), height = "400px")
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

#' Logique serveur du module statistiques descriptives
#'
#' @param id Identifiant unique du module
#' @param genetic_data Données génétiques filtrées (réactif)
#' @param smoking_data Données de tabagisme filtrées (réactif)
#' @param alcohol_data Données d'alcool filtrées (réactif)
#' @param cancer_markers_data Données des marqueurs filtrées (réactif)
#' @param analytics_data Données analytiques filtrées (réactif)
#' @param available_countries Liste des pays disponibles (réactif)
#' @param available_date_range Plage de dates disponible (réactif)
mod_stats_descriptives_server <- function(
  id,
  imported_data,
  available_countries,
  available_date_range
) {
  moduleServer(id, function(input, output, session) {
    
    # ==========================================================================
    # DONNÉES PRINCIPALES (db.csv chargé directement)
    # ==========================================================================
    
    # Ici, j'utilise directement les données de db.csv sans filtre de source
    # Les données sont déjà chargées et disponibles via imported_data
    base_data <- reactive({
      data <- imported_data()
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      return(data)
    })
    
    # ==========================================================================
    # IDENTIFICATION DES VARIABLES QUANTITATIVES DISPONIBLES
    # ==========================================================================
    
    # À ce niveau, je détecte toutes les variables quantitatives disponibles
    # en excluant explicitement "pays" et "nom_pays" comme demandé
    numeric_variables <- reactive({
      data <- base_data()
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      # Je filtre pour ne garder que les colonnes numériques
      numeric_cols <- sapply(data, is.numeric)
      numeric_cols <- names(data)[numeric_cols]
      
      # Je retire les colonnes d'identifiant, de date et explicitement "pays" et "nom_pays"
      exclude_cols <- c(
        COL_PATIENT_ID, COL_GENE_ID, COL_DATE, COL_YEAR, 
        "id_global", "id", "patient_id", "gene_id", 
        "annee", "year", "trimestre",
        "pays", "nom_pays"  # Exclusion explicite comme demandé
      )
      numeric_cols <- setdiff(numeric_cols, exclude_cols)
      
      # Si aucune variable numérique n'est disponible, je retourne NULL
      if (length(numeric_cols) == 0) {
        return(NULL)
      }
      
      return(numeric_cols)
    })
    
    # ==========================================================================
    # INTERFACE DYNAMIQUE POUR LA SÉLECTION DE VARIABLE
    # ==========================================================================
    
    output$variable_selector <- renderUI({
      ns <- session$ns
      vars <- numeric_variables()
      
      if (is.null(vars) || length(vars) == 0) {
        return(
          selectInput(
            ns("variable"),
            label = tags$strong("Variable"),
            choices = list("Aucune variable disponible" = ""),
            selected = ""
          )
        )
      }
      
      # Je crée une liste nommée avec des labels en français
      choices_list <- as.list(vars)
      names(choices_list) <- vars
      
      selectInput(
        ns("variable"),
        label = tags$strong("Variable à analyser"),
        choices = choices_list,
        selected = vars[1]
      )
    })
    
    # ==========================================================================
    # INTERFACE DYNAMIQUE POUR LE FILTRE PAR PAYS
    # ==========================================================================
    
    # Ici, je gère le filtre pays en utilisant uniquement la variable "nom_pays"
    output$country_selector <- renderUI({
      ns <- session$ns
      data <- base_data()
      
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      # Utiliser uniquement "nom_pays" comme demandé
      if (!"nom_pays" %in% names(data)) {
        return(NULL)
      }
      
      # Obtenir les pays uniques depuis nom_pays
      countries <- unique(data[["nom_pays"]])
      countries <- countries[!is.na(countries) & countries != ""]
      countries <- sort(countries)
      
      if (length(countries) == 0) {
        return(NULL)
      }
      
      selectInput(
        ns("country_filter"),
        label = tags$strong("Pays"),
        choices = c("Tous" = "", countries),
        selected = "",
        multiple = FALSE
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
    # DONNÉES FILTRÉES SELON LES CRITÈRES COMBINÉS
    # ==========================================================================
    
    # Ici, je filtre les données selon les critères sélectionnés de manière combinée
    # Les filtres fonctionnent ensemble : variable, pays et période
    filtered_data <- reactive({
      data <- base_data()
      
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      # Je filtre par pays si sélectionné (en utilisant nom_pays uniquement)
      if (!is.null(input$country_filter) && input$country_filter != "" && "nom_pays" %in% names(data)) {
        data <- data[data[["nom_pays"]] == input$country_filter, ]
      }
      
      # Je filtre par période si sélectionnée (en utilisant annee et trimestre)
      if (!is.null(input$date_range) && length(input$date_range) == 2) {
        start_date <- as.Date(input$date_range[1])
        end_date <- as.Date(input$date_range[2])
        
        # Si une colonne annee existe, filtrer par année
        if ("annee" %in% names(data)) {
          start_year <- as.numeric(format(start_date, "%Y"))
          end_year <- as.numeric(format(end_date, "%Y"))
          data <- data[data[["annee"]] >= start_year & data[["annee"]] <= end_year, ]
        }
        
        # Si trimestre existe, on pourrait aussi filtrer par trimestre si nécessaire
        # Pour l'instant, on se contente du filtre par année
      }
      
      return(data)
    })
    
    # ==========================================================================
    # CALCUL DES STATISTIQUES DESCRIPTIVES
    # ==========================================================================
    
    # Ici, je calcule les statistiques de base (moyenne, médiane, écart-type, etc.)
    # pour la variable sélectionnée, en excluant les valeurs manquantes.
    descriptive_stats <- reactive({
      data <- filtered_data()
      var <- input$variable
      
      if (is.null(data) || is.null(var) || var == "" || !var %in% names(data)) {
        return(NULL)
      }
      
      values <- data[[var]]
      values <- values[!is.na(values)]
      
      if (length(values) == 0) {
        return(NULL)
      }
      
      # Je calcule toutes les statistiques
      stats <- list(
        "Nombre d'observations" = length(values),
        "Valeurs manquantes" = sum(is.na(data[[var]])),
        "Moyenne" = mean(values),
        "Médiane" = median(values),
        "Écart-type" = sd(values),
        "Variance" = var(values),
        "Minimum" = min(values),
        "Maximum" = max(values),
        "1er quartile (Q1)" = quantile(values, 0.25),
        "3ème quartile (Q3)" = quantile(values, 0.75),
        "Étendue" = max(values) - min(values),
        "Coefficient de variation" = sd(values) / mean(values) * 100
      )
      
      # Je formate les résultats dans un dataframe
      stats_df <- data.frame(
        Statistique = names(stats),
        Valeur = unlist(stats),
        row.names = NULL
      )
      
      return(stats_df)
    })
    
    # ==========================================================================
    # AFFICHAGE DU TABLEAU DES STATISTIQUES
    # ==========================================================================
    
    output$stats_table <- DT::renderDataTable({
      stats <- descriptive_stats()
      
      if (is.null(stats)) {
        return(data.frame(
          Statistique = "Aucune donnée disponible",
          Valeur = "Sélectionnez une variable numérique et vérifiez que les filtres ne sont pas trop restrictifs."
        ))
      }
      
      # Je formate les valeurs numériques
      stats$Valeur <- ifelse(
        is.numeric(stats$Valeur),
        round(stats$Valeur, 3),
        as.character(stats$Valeur)
      )
      
      DT::datatable(
        stats,
        options = list(
          pageLength = 15,
          dom = 't',
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
        ),
        rownames = FALSE,
        class = "display nowrap"
      ) %>%
        DT::formatStyle(
          "Statistique",
          fontWeight = "bold",
          color = "#1E5A8E"
        )
    })
    
    # ==========================================================================
    # HISTOGRAMME DE DISTRIBUTION
    # ==========================================================================
    
    output$histogram <- renderPlot({
      data <- filtered_data()
      var <- input$variable
      
      if (is.null(data) || is.null(var) || var == "" || !var %in% names(data)) {
        return(NULL)
      }
      
      values <- data[[var]]
      values <- values[!is.na(values)]
      
      if (length(values) == 0) {
        # Je crée un graphique vide avec un message informatif
        colors <- get_theme_colors()
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Aucune donnée disponible pour cette variable", 
                   size = 5, color = colors$text_secondary) +
          theme_void() +
          theme(plot.background = element_rect(fill = colors$bg_white))
        return(p)
      }
      
      # Je crée l'histogramme pour visualiser la distribution de la variable.
      # Le thème académique assure une cohérence visuelle avec le reste du dashboard.
      colors <- get_theme_colors()
      
      p <- ggplot(data.frame(x = values), aes(x = x)) +
        geom_histogram(
          fill = colors$chart_blue,
          color = "white",
          alpha = 0.7,
          bins = 30
        ) +
        theme_academique() +
        labs(
          title = paste("Distribution de", var),
          x = var,
          y = "Fréquence"
        )
      
      return(p)
    })
    
    # ==========================================================================
    # BOXPLOT DE DISTRIBUTION
    # ==========================================================================
    
    output$boxplot <- renderPlot({
      data <- filtered_data()
      var <- input$variable
      
      if (is.null(data) || is.null(var) || var == "" || !var %in% names(data)) {
        return(NULL)
      }
      
      values <- data[[var]]
      values <- values[!is.na(values)]
      
      if (length(values) == 0) {
        # Je crée un graphique vide avec un message informatif
        colors <- get_theme_colors()
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Aucune donnée disponible pour cette variable", 
                   size = 5, color = colors$text_secondary) +
          theme_void() +
          theme(plot.background = element_rect(fill = colors$bg_white))
        return(p)
      }
      
      # Je crée le boxplot pour visualiser les quartiles et détecter les valeurs aberrantes.
      # Le thème académique assure une cohérence visuelle avec le reste du dashboard.
      colors <- get_theme_colors()
      
      p <- ggplot(data.frame(x = values), aes(y = x)) +
        geom_boxplot(
          fill = colors$chart_blue_light,
          color = colors$chart_blue,
          alpha = 0.7,
          width = 0.5
        ) +
        theme_academique() +
        labs(
          title = paste("Boxplot de", var),
          y = var,
          x = ""
        ) +
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
      
      return(p)
    })
    
  })
}
