# ==============================================================================
# MODULE CLUSTERING NON SUPERVISÉ
# ==============================================================================
# 
# Module Shiny pour le clustering non supervisé
# Méthodes disponibles : k-means et clustering hiérarchique
# Visualisation sur l'espace ACP
#
# ==============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(FactoMineR)
library(factoextra)
library(cluster)

# À ce niveau, je charge theme_shiny.R avant theme_ggplot.R car
# theme_ggplot.R utilise la fonction get_theme_colors() définie dans theme_shiny.R
source("R/theme_shiny.R")
source("R/theme_ggplot.R")
source("R/constants.R")

# ==============================================================================
# UI DU MODULE
# ==============================================================================

#' Interface utilisateur du module clustering
#'
#' @param id Identifiant unique du module
#' @return Interface utilisateur du module
mod_clustering_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # Titre de la page
    div(
      class = "container-fluid",
      style = "margin-bottom: 2rem;",
      h1(
        style = "color: #1E5A8E; margin-bottom: 0.5rem;",
        icon("object-group", style = "margin-right: 0.5rem;"),
        "Clustering Non Supervisé"
      ),
      p(
        style = "color: #6C757D; font-size: 1.1rem;",
        "Identification de groupes d'individus similaires à partir de variables quantitatives"
      ),
      div(
        style = "background-color: #E7F3FF; border-left: 4px solid #1E5A8E; padding: 1rem; margin-top: 1rem; border-radius: 0.375rem;",
        p(
          style = "margin: 0; color: #0C5460; font-size: 0.95rem;",
          icon("info-circle", style = "margin-right: 0.5rem;"),
          tags$strong("Méthode :"),
          " Le clustering non supervisé permet d'identifier des groupes d'individus similaires",
          " sans connaître à l'avance le nombre de groupes. Deux méthodes sont disponibles :",
          " k-means (rapide, adapté aux grands datasets) et clustering hiérarchique",
          " (visualise la structure des données)."
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
            "Configuration du clustering"
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
            
            # Méthode de clustering
            column(
              width = 3,
              selectInput(
                ns("clustering_method"),
                label = tags$strong("Méthode de clustering"),
                choices = list(
                  "K-means" = "kmeans",
                  "Clustering hiérarchique" = "hierarchical"
                ),
                selected = "kmeans"
              )
            ),
            
            # Nombre de clusters
            column(
              width = 3,
              numericInput(
                ns("n_clusters"),
                label = tags$strong("Nombre de clusters"),
                value = 3,
                min = 2,
                max = 10,
                step = 1
              )
            ),
            
            # Distance pour clustering hiérarchique
            column(
              width = 3,
              uiOutput(ns("distance_selector"))
            )
          ),
          
          # Sélection des variables
          fluidRow(
            column(
              width = 12,
              style = "margin-top: 1rem;",
              uiOutput(ns("variables_selector"))
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
    
    # Visualisation sur l'espace ACP
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
            icon("project-diagram", style = "margin-right: 0.5rem;"),
            "Visualisation des Clusters sur l'Espace ACP"
          )
        ),
        
        div(
          class = "card-body",
          style = "padding: 1.5rem;",
          
          plotOutput(ns("clusters_acp_plot"), height = "500px")
        )
      )
    ),
    
    # Interprétation des groupes
    div(
      class = "container-fluid",
      
      fluidRow(
        # Caractéristiques moyennes par cluster
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
                "Caractéristiques Moyennes par Cluster"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              DT::dataTableOutput(ns("cluster_characteristics_table"), width = "100%")
            )
          )
        ),
        
        # Répartition des individus
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
                icon("chart-pie", style = "margin-right: 0.5rem;"),
                "Répartition des Individus"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 1.5rem;",
              
              DT::dataTableOutput(ns("cluster_distribution_table"), width = "100%"),
              plotOutput(ns("cluster_distribution_plot"), height = "300px")
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

#' Logique serveur du module clustering
#'
#' @param id Identifiant unique du module
#' @param genetic_data Données génétiques filtrées (réactif)
#' @param smoking_data Données de tabagisme filtrées (réactif)
#' @param alcohol_data Données d'alcool filtrées (réactif)
#' @param cancer_markers_data Données des marqueurs filtrées (réactif)
#' @param analytics_data Données analytiques filtrées (réactif)
#' @param available_countries Liste des pays disponibles (réactif)
#' @param available_date_range Plage de dates disponible (réactif)
mod_clustering_server <- function(
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
    # IDENTIFICATION DES VARIABLES QUANTITATIVES DISPONIBLES
    # ==========================================================================
    
    # À ce niveau, je détecte automatiquement toutes les variables quantitatives
    # disponibles dans le dataset sélectionné. Le clustering nécessite uniquement
    # des variables numériques pour calculer les distances entre individus.
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
      
      choices_list <- as.list(vars)
      names(choices_list) <- vars
      
      checkboxGroupInput(
        ns("selected_variables"),
        label = tags$strong("Variables quantitatives pour le clustering (sélectionnez au moins 2)"),
        choices = choices_list,
        selected = if (length(vars) <= 10) vars else vars[1:min(10, length(vars))]
      )
    })
    
    # ==========================================================================
    # INTERFACE DYNAMIQUE POUR LA DISTANCE (clustering hiérarchique)
    # ==========================================================================
    
    output$distance_selector <- renderUI({
      ns <- session$ns
      
      if (input$clustering_method == "hierarchical") {
        selectInput(
          ns("distance_method"),
          label = tags$strong("Méthode de distance"),
          choices = list(
            "Euclidienne" = "euclidean",
            "Manhattan" = "manhattan",
            "Maximum" = "maximum"
          ),
          selected = "euclidean"
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
    # PRÉPARATION DES DONNÉES POUR LE CLUSTERING
    # ==========================================================================
    
    # Ici, je prépare les données avec uniquement les variables quantitatives
    # sélectionnées. Je standardise les données pour que toutes les variables
    # aient le même poids dans le calcul des distances.
    clustering_data <- reactive({
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
      cluster_data <- data[, vars, drop = FALSE]
      
      # Je retire les lignes avec des valeurs manquantes
      cluster_data <- cluster_data[complete.cases(cluster_data), ]
      
      # Je standardise les données pour le clustering. Cette étape est essentielle
      # pour que toutes les variables aient le même poids, indépendamment de leur échelle.
      cluster_data_scaled <- scale(cluster_data)
      
      if (nrow(cluster_data_scaled) < 2) {
        return(NULL)
      }
      
      return(list(
        raw = cluster_data,
        scaled = cluster_data_scaled,
        variables = vars
      ))
    })
    
    # ==========================================================================
    # CALCUL DE L'ACP POUR LA VISUALISATION
    # ==========================================================================
    
    # Ici, je calcule une ACP pour visualiser les clusters dans un espace de
    # dimension réduite (2D). Cette visualisation facilite l'interprétation des groupes.
    acp_for_clustering <- reactive({
      cluster_data <- clustering_data()
      
      if (is.null(cluster_data)) {
        return(NULL)
      }
      
      # Je calcule l'ACP avec FactoMineR
      acp <- PCA(cluster_data$scaled, graph = FALSE, scale.unit = FALSE)
      
      return(acp)
    })
    
    # ==========================================================================
    # CALCUL DU CLUSTERING
    # ==========================================================================
    
    # Ici, je calcule le clustering selon la méthode choisie (k-means ou hiérarchique).
    # Le k-means est rapide et adapté aux grands datasets, tandis que le clustering
    # hiérarchique permet de visualiser la structure des données.
    clustering_result <- reactive({
      cluster_data <- clustering_data()
      n_clusters <- input$n_clusters
      method <- input$clustering_method
      
      if (is.null(cluster_data) || n_clusters < 2) {
        return(NULL)
      }
      
      if (method == "kmeans") {
        # Je calcule le k-means avec plusieurs initialisations (nstart = 25)
        # pour obtenir un résultat stable. Le set.seed assure la reproductibilité.
        set.seed(123)
        kmeans_result <- kmeans(cluster_data$scaled, centers = n_clusters, nstart = 25)
        
        return(list(
          cluster = kmeans_result$cluster,
          centers = kmeans_result$centers,
          method = "kmeans"
        ))
      } else if (method == "hierarchical") {
        # Je calcule le clustering hiérarchique en deux étapes :
        # 1) Calcul de la matrice de distances entre tous les individus
        # 2) Construction de l'arbre hiérarchique avec la méthode Ward
        distance_method <- input$distance_method
        if (is.null(distance_method)) {
          distance_method <- "euclidean"
        }
        
        # Je calcule la matrice de distances selon la méthode choisie
        dist_matrix <- dist(cluster_data$scaled, method = distance_method)
        
        # Je calcule le clustering hiérarchique avec la méthode Ward
        # qui minimise la variance intra-cluster
        hclust_result <- hclust(dist_matrix, method = "ward.D2")
        
        # Je découpe l'arbre au niveau du nombre de clusters souhaité
        clusters <- cutree(hclust_result, k = n_clusters)
        
        return(list(
          cluster = clusters,
          hclust = hclust_result,
          method = "hierarchical"
        ))
      }
      
      return(NULL)
    })
    
    # ==========================================================================
    # VISUALISATION DES CLUSTERS SUR L'ESPACE ACP
    # ==========================================================================
    
    # Ici, je crée la visualisation des clusters projetés sur les deux premiers
    # axes de l'ACP. Les ellipses montrent la dispersion de chaque cluster.
    output$clusters_acp_plot <- renderPlot({
      acp <- acp_for_clustering()
      clusters <- clustering_result()
      
      if (is.null(acp) || is.null(clusters)) {
        return(NULL)
      }
      
      # Je vérifie la structure de l'objet ACP avec gestion d'erreur robuste
      tryCatch({
        # Vérifier que acp$ind existe
        if (is.null(acp$ind)) {
          warning("⚠️ acp$ind est NULL")
          return(NULL)
        }
        
        # Je récupère les coordonnées des individus avec vérifications multiples
        ind_coord <- NULL
        
        # Cas 1 : acp$ind est une liste avec $coord
        if (is.list(acp$ind) && !is.null(acp$ind$coord)) {
          if (is.matrix(acp$ind$coord) || is.data.frame(acp$ind$coord)) {
            ind_coord <- acp$ind$coord
          } else if (is.atomic(acp$ind$coord)) {
            # Si c'est un vecteur atomique, on ne peut pas l'utiliser directement
            warning("⚠️ acp$ind$coord est un vecteur atomique, structure inattendue")
            return(NULL)
          }
        }
        # Cas 2 : acp$ind est directement une matrice ou un data.frame
        else if (is.matrix(acp$ind) || is.data.frame(acp$ind)) {
          ind_coord <- acp$ind
        }
        # Cas 3 : acp$ind est un vecteur atomique (erreur)
        else if (is.atomic(acp$ind)) {
          warning("⚠️ acp$ind est un vecteur atomique, impossible d'accéder aux coordonnées")
          return(NULL)
        }
        
        # Vérifier que ind_coord a été correctement assigné
        if (is.null(ind_coord)) {
          warning("⚠️ Impossible de récupérer les coordonnées des individus")
          return(NULL)
        }
        
        # Je vérifie qu'il y a au moins 2 dimensions
        if (ncol(ind_coord) < 2) {
          warning("⚠️ Moins de 2 dimensions disponibles dans les coordonnées")
          return(NULL)
        }
        
        # Vérifier que le nombre de lignes correspond au nombre de clusters
        if (nrow(ind_coord) != length(clusters$cluster)) {
          warning("⚠️ Le nombre de lignes des coordonnées ne correspond pas au nombre de clusters")
          return(NULL)
        }
      }, error = function(e) {
        warning("❌ Erreur lors de l'accès aux coordonnées ACP : ", e$message)
        return(NULL)
      })
      
      # Je crée le graphique avec factoextra
      # Je prépare les données pour fviz_cluster
      cluster_data_viz <- data.frame(
        Dim.1 = ind_coord[, 1],
        Dim.2 = ind_coord[, 2],
        cluster = as.factor(clusters$cluster)
      )
      
      # Je récupère les valeurs propres pour les labels des axes avec gestion d'erreur
      eigenvals <- tryCatch({
        eigen_df <- get_eigenvalue(acp)
        if (is.null(eigen_df) || nrow(eigen_df) < 2) {
          return(list(var1 = 0, var2 = 0))
        }
        list(
          var1 = if ("variance.percent" %in% names(eigen_df)) eigen_df$variance.percent[1] else 0,
          var2 = if ("variance.percent" %in% names(eigen_df)) eigen_df$variance.percent[2] else 0
        )
      }, error = function(e) {
        warning("Erreur lors de la récupération des valeurs propres : ", e$message)
        return(list(var1 = 0, var2 = 0))
      })
      
      # Je crée le graphique avec ggplot2 directement pour plus de contrôle
      colors <- get_theme_colors()
      palette <- get_chart_palette(input$n_clusters)
      
      p <- ggplot(cluster_data_viz, aes(x = Dim.1, y = Dim.2, color = cluster, fill = cluster)) +
        geom_point(size = 2, alpha = 0.7) +
        stat_ellipse(type = "norm", level = 0.68, alpha = 0.2) +
        scale_color_manual(values = palette) +
        scale_fill_manual(values = palette) +
        theme_academique() +
        labs(
          title = paste("Visualisation des clusters sur l'espace ACP (méthode", 
                       ifelse(clusters$method == "kmeans", "k-means", "hiérarchique"), ")"),
          subtitle = paste("Nombre de clusters :", input$n_clusters),
          x = paste("Axe 1 (", round(eigenvals$var1, 2), "% de variance)"),
          y = paste("Axe 2 (", round(eigenvals$var2, 2), "% de variance)"),
          color = "Cluster",
          fill = "Cluster"
        ) +
        theme(
          plot.title = element_text(color = colors$text_primary, hjust = 0.5),
          plot.subtitle = element_text(color = colors$text_secondary, hjust = 0.5)
        )
      
      return(p)
    })
    
    # ==========================================================================
    # CARACTÉRISTIQUES MOYENNES PAR CLUSTER
    # ==========================================================================
    
    # Ici, je calcule les moyennes de chaque variable par cluster pour permettre
    # l'interprétation des groupes identifiés.
    output$cluster_characteristics_table <- DT::renderDataTable({
      cluster_data <- clustering_data()
      clusters <- clustering_result()
      
      if (is.null(cluster_data) || is.null(clusters)) {
        return(data.frame(
          Message = "Sélectionnez au moins 2 variables pour effectuer le clustering."
        ))
      }
      
      # Je calcule les moyennes par cluster
      data_with_clusters <- cluster_data$raw
      data_with_clusters$Cluster <- paste("Cluster", clusters$cluster)
      
      cluster_means <- data_with_clusters %>%
        group_by(Cluster) %>%
        summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop")
      
      # Je formate les valeurs numériques
      numeric_cols <- sapply(cluster_means, is.numeric)
      cluster_means[, numeric_cols] <- round(cluster_means[, numeric_cols], 3)
      
      DT::datatable(
        cluster_means,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 't',
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
        ),
        rownames = FALSE,
        class = "display nowrap"
      ) %>%
        DT::formatStyle(
          "Cluster",
          fontWeight = "bold",
          color = "#1E5A8E"
        )
    })
    
    # ==========================================================================
    # RÉPARTITION DES INDIVIDUS PAR CLUSTER
    # ==========================================================================
    
    output$cluster_distribution_table <- DT::renderDataTable({
      clusters <- clustering_result()
      
      if (is.null(clusters)) {
        return(data.frame(
          Message = "Effectuez d'abord le clustering."
        ))
      }
      
      # Je compte le nombre d'individus par cluster
      cluster_counts <- table(clusters$cluster)
      cluster_df <- data.frame(
        Cluster = paste("Cluster", names(cluster_counts)),
        "Nombre d'individus" = as.numeric(cluster_counts),
        "Pourcentage" = round(as.numeric(cluster_counts) / sum(cluster_counts) * 100, 2),
        check.names = FALSE
      )
      
      DT::datatable(
        cluster_df,
        options = list(
          pageLength = 10,
          dom = 't',
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
        ),
        rownames = FALSE,
        class = "display nowrap"
      ) %>%
        DT::formatStyle(
          "Cluster",
          fontWeight = "bold",
          color = "#1E5A8E"
        )
    })
    
    output$cluster_distribution_plot <- renderPlot({
      clusters <- clustering_result()
      
      if (is.null(clusters)) {
        return(NULL)
      }
      
      # Je compte le nombre d'individus par cluster
      cluster_counts <- table(clusters$cluster)
      cluster_df <- data.frame(
        Cluster = paste("Cluster", names(cluster_counts)),
        Count = as.numeric(cluster_counts)
      )
      
      # Je crée le graphique en barres
      colors <- get_theme_colors()
      palette <- get_chart_palette(nrow(cluster_df))
      
      p <- ggplot(cluster_df, aes(x = Cluster, y = Count, fill = Cluster)) +
        geom_bar(stat = "identity", alpha = 0.7) +
        scale_fill_manual(values = palette) +
        theme_academique() +
        labs(
          title = "Répartition des individus par cluster",
          x = "Cluster",
          y = "Nombre d'individus"
        ) +
        theme(
          plot.title = element_text(color = colors$text_primary, hjust = 0.5),
          legend.position = "none"
        )
      
      return(p)
    })
    
  })
}
