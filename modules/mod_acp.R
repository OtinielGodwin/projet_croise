# ==============================================================================
# MODULE ANALYSE EN COMPOSANTES PRINCIPALES (ACP)
# ==============================================================================
# 
# Module Shiny pour l'analyse en composantes principales
# Utilise FactoMineR pour le calcul et factoextra pour la visualisation
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

#' Interface utilisateur du module ACP
#'
#' @param id Identifiant unique du module
#' @return Interface utilisateur du module
mod_acp_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # Titre de la page
    div(
      class = "container-fluid",
      style = "margin-bottom: 2rem;",
      h1(
        style = "color: #1E5A8E; margin-bottom: 0.5rem;",
        icon("project-diagram", style = "margin-right: 0.5rem;"),
        "Analyse en Composantes Principales (ACP)"
      ),
      p(
        style = "color: #6C757D; font-size: 1.1rem;",
        "Réduction de dimensionnalité et visualisation des relations entre variables quantitatives"
      ),
      div(
        style = "background-color: #E7F3FF; border-left: 4px solid #1E5A8E; padding: 1rem; margin-top: 1rem; border-radius: 0.375rem;",
        p(
          style = "margin: 0; color: #0C5460; font-size: 0.95rem;",
          icon("info-circle", style = "margin-right: 0.5rem;"),
          tags$strong("Méthode :"),
          " L'ACP transforme un ensemble de variables quantitatives corrélées en un nombre réduit",
          " de composantes principales non corrélées, permettant de visualiser la structure des données",
          " dans un espace de dimension réduite."
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
            
            # Sélection des axes à visualiser
            column(
              width = 2,
              numericInput(
                ns("axis1"),
                label = tags$strong("Axe 1"),
                value = 1,
                min = 1,
                max = 10,
                step = 1
              )
            ),
            
            column(
              width = 2,
              numericInput(
                ns("axis2"),
                label = tags$strong("Axe 2"),
                value = 2,
                min = 1,
                max = 10,
                step = 1
              )
            ),
            
            # Filtre par pays
            column(
              width = 2,
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
    
    # Interprétation des axes (valeurs propres)
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
                "Valeurs Propres (Scree Plot)"
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
    
    # Cercle des corrélations
    div(
      class = "container-fluid",
      
      div(
        class = "card",
        
        div(
          class = "card-header",
          style = "background-color: #F8F9FA; border-bottom: 2px solid #1E5A8E;",
          h3(
            style = "margin: 0; color: #1E5A8E; font-size: 1.25rem;",
            icon("circle", style = "margin-right: 0.5rem;"),
            "Cercle des Corrélations"
          )
        ),
        
        div(
          class = "card-body",
          style = "padding: 1.5rem;",
          
          plotOutput(ns("variables_plot"), height = "500px")
        )
      )
    )
  )
}

# ==============================================================================
# SERVER DU MODULE
# ==============================================================================

#' Logique serveur du module ACP
#'
#' @param id Identifiant unique du module
#' @param genetic_data Données génétiques filtrées (réactif)
#' @param smoking_data Données de tabagisme filtrées (réactif)
#' @param alcohol_data Données d'alcool filtrées (réactif)
#' @param cancer_markers_data Données des marqueurs filtrées (réactif)
#' @param analytics_data Données analytiques filtrées (réactif)
#' @param available_countries Liste des pays disponibles (réactif)
#' @param available_date_range Plage de dates disponible (réactif)
mod_acp_server <- function(
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
    # IDENTIFICATION DES VARIABLES QUANTITATIVES DISPONIBLES
    # ==========================================================================
    
    # À ce niveau, je détecte automatiquement toutes les variables quantitatives
    # disponibles dans le dataset sélectionné. L'ACP nécessite uniquement des variables numériques.
    quantitative_variables <- reactive({
      data <- selected_data()
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      # Je filtre pour ne garder que les colonnes quantitatives
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
      vars <- quantitative_variables()
      
      if (is.null(vars) || length(vars) == 0) {
        return(
          p(
            style = "color: #DC3545;",
            "Aucune variable quantitative disponible dans ce dataset."
          )
        )
      }
      
      # Je crée une liste nommée avec des labels en français
      choices_list <- as.list(vars)
      names(choices_list) <- vars
      
      checkboxGroupInput(
        ns("selected_variables"),
        label = tags$strong("Variables quantitatives à analyser (sélectionnez au moins 2)"),
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
    # PRÉPARATION DES DONNÉES POUR L'ACP
    # ==========================================================================
    
    # Ici, je prépare les données avec uniquement les variables quantitatives sélectionnées.
    # Les valeurs manquantes sont exclues car l'ACP nécessite des données complètes.
    acp_data <- reactive({
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
      acp_data <- data[, vars, drop = FALSE]
      
      # Je retire les lignes avec des valeurs manquantes pour l'ACP
      acp_data <- acp_data[complete.cases(acp_data), ]
      
      # Je vérifie qu'il reste assez de données
      if (nrow(acp_data) < 2) {
        return(NULL)
      }
      
      return(acp_data)
    })
    
    # ==========================================================================
    # CALCUL DE L'ACP
    # ==========================================================================
    
    # Ici, je calcule l'ACP avec FactoMineR. Les variables sont standardisées
    # (scale.unit = TRUE) pour éviter que les variables avec de grandes valeurs
    # dominent l'analyse. Cette étape est cruciale pour une interprétation correcte.
    acp_result <- reactive({
      data <- acp_data()
      
      if (is.null(data) || nrow(data) < 2 || ncol(data) < 2) {
        return(NULL)
      }
      
      # Je calcule l'ACP avec FactoMineR
      # graph = FALSE pour éviter l'affichage automatique
      acp <- PCA(data, graph = FALSE, scale.unit = TRUE)
      
      return(acp)
    })
    
    # ==========================================================================
    # GRAPHIQUE DES VALEURS PROPRES
    # ==========================================================================
    
    # Ici, je crée le graphique des valeurs propres (scree plot) qui permet
    # d'identifier le nombre de composantes principales à retenir pour l'interprétation.
    output$eigenvalues_plot <- renderPlot({
      acp <- acp_result()
      
      if (is.null(acp)) {
        return(NULL)
      }
      
      # Je crée le graphique des valeurs propres avec factoextra
      p <- fviz_eig(acp, addlabels = TRUE, ylim = c(0, 50))
      
      # J'applique le thème académique
      colors <- get_theme_colors()
      p <- p + 
        theme_academique() +
        labs(
          title = "Valeurs propres des composantes principales",
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
    # TABLEAU DES VALEURS PROPRES ET INTERPRÉTATION
    # ==========================================================================
    
    output$eigenvalues_table <- DT::renderDataTable({
      acp <- acp_result()
      
      if (is.null(acp)) {
        return(data.frame(
          Message = "Sélectionnez au moins 2 variables pour effectuer l'ACP."
        ))
      }
      
      # Je récupère les valeurs propres et le pourcentage de variance avec gestion d'erreur robuste
      eig <- tryCatch({
        eig_result <- get_eigenvalue(acp)
        if (is.null(eig_result)) return(NULL)
        
        # Vérifier si c'est un vecteur atomique
        if (is.atomic(eig_result) && !is.null(names(eig_result))) {
          warning("⚠️ get_eigenvalue retourne un vecteur atomique, conversion impossible")
          return(NULL)
        }
        
        # Vérifier si c'est un data.frame ou une matrice
        if (is.data.frame(eig_result) || is.matrix(eig_result)) {
          # Vérifier que les colonnes nécessaires existent
          if (!"eigenvalue" %in% names(eig_result) && !"eigenvalue" %in% colnames(eig_result)) {
            warning("⚠️ Colonne 'eigenvalue' non trouvée dans get_eigenvalue")
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
          Message = "Impossible de récupérer les valeurs propres. Vérifiez que l'ACP a été calculée correctement."
        ))
      }
      
      # Je crée un dataframe avec l'interprétation
      tryCatch({
        eig_df <- data.frame(
          Axe = rownames(eig),
          "Valeur propre" = round(eig$eigenvalue, 3),
          "Pourcentage de variance" = round(eig$variance.percent, 2),
          "Pourcentage cumulé" = round(eig$cumulative.variance.percent, 2),
          check.names = FALSE,
          stringsAsFactors = FALSE
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
      }, error = function(e) {
        warning("❌ Erreur lors de la création du tableau : ", e$message)
        return(data.frame(
          Message = paste("Erreur lors de l'affichage des valeurs propres :", e$message)
        ))
      })
    })
    
    # ==========================================================================
    # PROJECTION DES INDIVIDUS
    # ==========================================================================
    
    # Ici, je crée la projection des individus sur les axes sélectionnés.
    # Cette visualisation permet d'identifier les groupes d'individus similaires.
    output$individuals_plot <- renderPlot({
      acp <- acp_result()
      axis1 <- input$axis1
      axis2 <- input$axis2
      
      if (is.null(acp)) {
        return(NULL)
      }
      
      # Je vérifie la structure de l'objet ACP
      # Parfois acp$ind peut être directement une matrice ou un vecteur atomique
      if (is.null(acp$ind)) {
        return(NULL)
      }
      
      # Je récupère les coordonnées des individus
      ind_coord <- if (is.list(acp$ind) && !is.null(acp$ind$coord)) {
        acp$ind$coord
      } else if (is.matrix(acp$ind) || is.data.frame(acp$ind)) {
        acp$ind
      } else {
        return(NULL)
      }
      
      # Je vérifie que les axes demandés existent
      n_axes <- ncol(ind_coord)
      if (is.null(n_axes) || axis1 > n_axes || axis2 > n_axes || axis1 < 1 || axis2 < 1) {
        return(NULL)
      }
      
      # Je crée le graphique de projection des individus avec factoextra
      p <- fviz_pca_ind(
        acp,
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
          subtitle = {
            eigenvals <- tryCatch({
              eig <- tryCatch({
                eig_result <- get_eigenvalue(acp)
                if (is.null(eig_result)) return(NULL)
                if (is.atomic(eig_result) && !is.null(names(eig_result))) {
                  warning("⚠️ get_eigenvalue retourne un vecteur atomique")
                  return(NULL)
                }
                if (is.data.frame(eig_result) || is.matrix(eig_result)) {
                  return(eig_result)
                }
                return(NULL)
              }, error = function(e) {
                warning("❌ Erreur get_eigenvalue : ", e$message)
                return(NULL)
              })
              
              if (is.null(eig)) {
                return(NULL)
              }
              if (is.null(eig) || !is.data.frame(eig) && !is.matrix(eig)) {
                return(list(var1 = 0, var2 = 0))
              }
              var_col <- if ("variance.percent" %in% names(eig)) eig$variance.percent else eig[, "variance.percent"]
              list(
                var1 = if (length(var_col) >= axis1) var_col[axis1] else 0,
                var2 = if (length(var_col) >= axis2) var_col[axis2] else 0
              )
            }, error = function(e) {
              return(list(var1 = 0, var2 = 0))
            })
            paste(
              "Axe", axis1, ":", round(eigenvals$var1, 2), "% de variance | ",
              "Axe", axis2, ":", round(eigenvals$var2, 2), "% de variance"
            )
          }
        ) +
        theme(
          plot.title = element_text(color = colors$text_primary, hjust = 0.5),
          plot.subtitle = element_text(color = colors$text_secondary, hjust = 0.5)
        )
      
      return(p)
    })
    
    # ==========================================================================
    # CERCLE DES CORRÉLATIONS
    # ==========================================================================
    
    # Ici, je crée le cercle des corrélations qui montre les relations entre
    # les variables originales et les axes factoriels. Les variables proches
    # sont corrélées, et la longueur des flèches indique la qualité de représentation.
    output$variables_plot <- renderPlot({
      acp <- acp_result()
      axis1 <- input$axis1
      axis2 <- input$axis2
      
      if (is.null(acp)) {
        return(NULL)
      }
      
      # Je vérifie la structure de l'objet ACP pour les variables
      if (is.null(acp$var)) {
        return(NULL)
      }
      
      # Je récupère les coordonnées des variables
      var_coord <- if (is.list(acp$var) && !is.null(acp$var$coord)) {
        acp$var$coord
      } else if (is.matrix(acp$var) || is.data.frame(acp$var)) {
        acp$var
      } else {
        return(NULL)
      }
      
      # Je vérifie que les axes demandés existent
      n_axes <- ncol(var_coord)
      if (is.null(n_axes) || axis1 > n_axes || axis2 > n_axes || axis1 < 1 || axis2 < 1) {
        return(NULL)
      }
      
      # Je crée le cercle des corrélations avec factoextra
      p <- fviz_pca_var(
        acp,
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
          title = paste("Cercle des corrélations - Axes", axis1, "et", axis2),
          subtitle = paste(
            "Longueur des flèches = qualité de représentation | ",
            "Couleur = contribution à l'axe"
          ),
          color = "Contribution"
        ) +
        theme(
          plot.title = element_text(color = colors$text_primary, hjust = 0.5),
          plot.subtitle = element_text(color = colors$text_secondary, hjust = 0.5, size = 10)
        )
      
      return(p)
    })
    
  })
}
