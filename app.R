# ==============================================================================
# APPLICATION SHINY - Dashboard Projet Croisé
# ==============================================================================
# 
# Version utilisant directement db.csv depuis le GitHub
# Plus besoin d'importer un fichier CSV - les données sont chargées automatiquement
#
# ==============================================================================

# Charger les bibliothèques nécessaires
library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)
library(FactoMineR)
library(factoextra)
library(cluster)
library(glmnet)
library(tidyr)

# Charger les fichiers de configuration et de fonctions
tryCatch({
  source("R/theme_shiny.R")
}, error = function(e) {
  warning("⚠️  Erreur lors du chargement de R/theme_shiny.R : ", e$message)
})

tryCatch({
  source("R/theme_ggplot.R")
}, error = function(e) {
  warning("⚠️  Erreur lors du chargement de R/theme_ggplot.R : ", e$message)
})

tryCatch({
  source("R/utils.R")
}, error = function(e) {
  warning("⚠️  Erreur lors du chargement de R/utils.R : ", e$message)
})

tryCatch({
  source("R/detect_primers.R")
}, error = function(e) {
  warning("⚠️  Erreur lors du chargement de R/detect_primers.R : ", e$message)
})

tryCatch({
  source("R/time_series_analysis.R")
}, error = function(e) {
  warning("⚠️  Erreur lors du chargement de R/time_series_analysis.R : ", e$message)
})

tryCatch({
  source("R/ar_model.R")
}, error = function(e) {
  warning("⚠️  Erreur lors du chargement de R/ar_model.R : ", e$message)
})

# Charger les modules
tryCatch({
  source("modules/mod_stats_descriptives.R")
}, error = function(e) {
  warning("⚠️  Erreur lors du chargement de modules/mod_stats_descriptives.R : ", e$message)
})

tryCatch({
  source("modules/mod_correlations.R")
}, error = function(e) {
  warning("⚠️  Erreur lors du chargement de modules/mod_correlations.R : ", e$message)
})

tryCatch({
  source("modules/mod_acp.R")
}, error = function(e) {
  warning("⚠️  Erreur lors du chargement de modules/mod_acp.R : ", e$message)
})

tryCatch({
  source("modules/mod_afc.R")
}, error = function(e) {
  warning("⚠️  Erreur lors du chargement de modules/mod_afc.R : ", e$message)
})

tryCatch({
  source("modules/mod_famd.R")
}, error = function(e) {
  warning("⚠️  Erreur lors du chargement de modules/mod_famd.R : ", e$message)
})

tryCatch({
  source("modules/mod_clustering.R")
}, error = function(e) {
  warning("⚠️  Erreur lors du chargement de modules/mod_clustering.R : ", e$message)
})

tryCatch({
  source("modules/mod_regression.R")
}, error = function(e) {
  warning("⚠️  Erreur lors du chargement de modules/mod_regression.R : ", e$message)
})

tryCatch({
  source("modules/mod_amorces_adn.R")
}, error = function(e) {
  warning("⚠️  Erreur lors du chargement de modules/mod_amorces_adn.R : ", e$message)
})

tryCatch({
  source("modules/mod_temporal.R")
}, error = function(e) {
  warning("⚠️  Erreur lors du chargement de modules/mod_temporal.R : ", e$message)
})

tryCatch({
  source("modules/mod_ar_prediction.R")
}, error = function(e) {
  warning("⚠️  Erreur lors du chargement de modules/mod_ar_prediction.R : ", e$message)
})

# Appliquer le thème ggplot2 par défaut
tryCatch({
  if (exists("set_default_ggplot_theme")) {
    set_default_ggplot_theme()
  }
}, error = function(e) {
  warning("Impossible d'appliquer le thème ggplot2 par défaut : ", e$message)
})

# ==============================================================================
# CHARGEMENT DES DONNÉES DEPUIS db.csv
# ==============================================================================

# Charger db.csv au démarrage de l'application
load_db_data <- function() {
  db_path <- "db.csv"
  
  if (!file.exists(db_path)) {
    warning("⚠️  Le fichier db.csv n'existe pas. Veuillez le télécharger depuis le GitHub.")
    return(data.frame())
  }
  
  tryCatch({
    data <- read.csv(db_path, stringsAsFactors = FALSE, encoding = "UTF-8")
    message("✅ Données chargées depuis db.csv : ", nrow(data), " lignes, ", ncol(data), " colonnes")
    return(data)
  }, error = function(e) {
    warning("⚠️  Erreur lors du chargement de db.csv : ", e$message)
    return(data.frame())
  })
}

# Charger les données une seule fois au démarrage
db_data <- load_db_data()

# ==============================================================================
# INTERFACE UTILISATEUR
# ==============================================================================

# Thème Shiny
shiny_theme <- tryCatch({
  if (exists("get_shiny_theme")) {
    get_shiny_theme()
  } else {
    bslib::bs_theme(version = 5, bootswatch = "flatly")
  }
}, error = function(e) {
  bslib::bs_theme(version = 5, bootswatch = "flatly")
})

# DÉFINITION DE L'UI AVEC SIDEBAR REPLIABLE
ui <- page_sidebar(
  theme = shiny_theme,
  
  # Activer shinyjs pour toute l'application
  useShinyjs(),
  
  # CSS personnalisé
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      .nav-link {
        padding: 0.75rem 1rem;
        margin-bottom: 0.5rem;
        border-radius: 0.375rem;
        color: #1E5A8E;
        font-weight: 500;
        cursor: pointer;
        display: block;
        transition: all 0.3s ease;
        text-decoration: none;
      }
      .nav-link:hover {
        background-color: #E7F3FF;
        transform: translateX(5px);
      }
      .nav-link.active {
        background-color: #1E5A8E;
        color: white;
      }
    "))
  ),
  
  # ==========================================================================
  # SIDEBAR : NAVIGATION VERTICALE REPLIABLE
  # ==========================================================================
  sidebar = sidebar(
    width = 300,
    open = "open",
    id = "sidebar",
    
    h4(
      style = "color: #1E5A8E; margin-bottom: 1.5rem; font-weight: 600;",
      icon("bars", style = "margin-right: 0.5rem;"),
      "Navigation"
    ),
    
    # Onglets de navigation (rendu dynamiquement avec classes CSS)
    uiOutput("nav_links"),
    
    # Note sur les données (conditionnelle)
    uiOutput("note_amorces_adn")
  ),
  
  # ==========================================================================
  # CONTENU PRINCIPAL : PAGES D'ANALYSE
  # ==========================================================================
  # Contenu conditionnel selon l'onglet sélectionné
  uiOutput("main_content")
)

# ==============================================================================
# LOGIQUE SERVEUR
# ==============================================================================

server <- function(input, output, session) {
  
  # ============================================================================
  # GESTION DE LA NAVIGATION VERTICALE
  # ============================================================================
  
  # Variable réactive pour suivre l'onglet actif
  current_tab <- reactiveVal("accueil")
  
  # Observer les clics sur les liens de navigation
  observeEvent(input$nav_accueil, {
    current_tab("accueil")
  })
  
  observeEvent(input$nav_stats, {
    current_tab("stats")
  })
  
  observeEvent(input$nav_correlations, {
    current_tab("correlations")
  })
  
  observeEvent(input$nav_acp, {
    current_tab("acp")
  })
  
  observeEvent(input$nav_afc, {
    current_tab("afc")
  })
  
  observeEvent(input$nav_famd, {
    current_tab("famd")
  })
  
  observeEvent(input$nav_clustering, {
    current_tab("clustering")
  })
  
  observeEvent(input$nav_regression, {
    current_tab("regression")
  })
  
  observeEvent(input$nav_amorces, {
    current_tab("amorces")
  })
  
  observeEvent(input$nav_temporal, {
    current_tab("temporal")
  })
  
  observeEvent(input$nav_ar, {
    current_tab("ar_prediction")
  })
  
  # Observers pour les boutons de la page d'accueil
  observeEvent(input$nav_amorces, {
    current_tab("amorces")
  })
  
  observeEvent(input$nav_stats, {
    current_tab("stats")
  })
  
  observeEvent(input$nav_regression, {
    current_tab("regression")
  })
  
  # ============================================================================
  # RENDU DYNAMIQUE DES LIENS DE NAVIGATION AVEC CLASSES CSS
  # ============================================================================
  
  output$nav_links <- renderUI({
    active_tab <- current_tab()
    
    # Fonction pour créer un lien avec la classe active si nécessaire
    make_nav_link <- function(id, icon_name, label, tab_value) {
      is_active <- (tab_value == active_tab)
      link_class <- if (is_active) "nav-link active" else "nav-link"
      
      actionLink(
        id,
        label = tags$span(icon(icon_name), label),
        class = link_class,
        style = "text-decoration: none; display: block;"
      )
    }
    
    div(
      id = "nav-tabs",
      style = "display: flex; flex-direction: column;",
      
      # Bouton Accueil
      make_nav_link("nav_accueil", "home", "Accueil", "accueil"),
      
      # Bouton Statistiques
      make_nav_link("nav_stats", "chart-bar", "Statistiques", "stats"),
      
      # Bouton Corrélations
      make_nav_link("nav_correlations", "project-diagram", "Corrélations", "correlations"),
      
      # Bouton ACP
      make_nav_link("nav_acp", "project-diagram", "ACP", "acp"),
      
      # Bouton AFC
      make_nav_link("nav_afc", "sitemap", "AFC", "afc"),
      
      # Bouton FAMD
      make_nav_link("nav_famd", "network-wired", "FAMD", "famd"),
      
      # Bouton Clustering
      make_nav_link("nav_clustering", "object-group", "Clustering", "clustering"),
      
      # Bouton Régression
      make_nav_link("nav_regression", "chart-line", "Régression", "regression"),
      
      # Bouton Amorces ADN
      make_nav_link("nav_amorces", "dna", "Amorces ADN", "amorces"),
      
      # Bouton Analyse Temporelle
      make_nav_link("nav_temporal", "clock", "Analyse Temporelle", "temporal"),
      
      # Bouton Prédiction AR
      make_nav_link("nav_ar", "chart-area", "Prédiction AR", "ar_prediction")
    )
  })
  
  # ============================================================================
  # CONTENU PRINCIPAL CONDITIONNEL
  # ============================================================================
  
  output$main_content <- renderUI({
    tab <- current_tab()
    
    switch(
      tab,
      "accueil" = {
        tryCatch({
          if (file.exists("ui/page_accueil.R")) {
            # Charger la page d'accueil directement (pas besoin d'environnement isolé)
            source("ui/page_accueil.R", local = TRUE)
            if (exists("page_accueil_ui")) {
              page_accueil_ui()
            } else {
              div(
                class = "container-fluid",
                style = "padding: 2rem;",
                h1("Accueil", style = "color: #1E5A8E;"),
                p("Bienvenue sur le Dashboard Projet Croisé"),
                p("Données chargées depuis db.csv : ", nrow(db_data), " lignes, ", ncol(db_data), " colonnes")
              )
            }
          } else {
            div(
              class = "container-fluid",
              style = "padding: 2rem;",
              h1("Accueil", style = "color: #1E5A8E;"),
              p("Bienvenue sur le Dashboard Projet Croisé"),
              p("Données chargées depuis db.csv : ", nrow(db_data), " lignes, ", ncol(db_data), " colonnes")
            )
          }
        }, error = function(e) {
          # Afficher l'erreur pour le débogage
          warning("Erreur lors du chargement de la page d'accueil : ", e$message)
          div(
            class = "container-fluid",
            style = "padding: 2rem;",
            h1("Accueil", style = "color: #1E5A8E;"),
            p("Bienvenue sur le Dashboard Projet Croisé"),
            p("Données chargées depuis db.csv : ", nrow(db_data), " lignes, ", ncol(db_data), " colonnes"),
            div(
              style = "margin-top: 1rem; padding: 1rem; background-color: #FFF3CD; border-left: 4px solid #FFC107; border-radius: 0.375rem;",
              p(style = "margin: 0; color: #856404;", 
                icon("exclamation-triangle"), 
                " Note : Erreur lors du chargement de la page d'accueil personnalisée. ",
                "L'application fonctionne normalement avec les autres modules."
              ),
              p(style = "margin-top: 0.5rem; color: #856404; font-size: 0.9rem;", 
                "Détails de l'erreur : ", as.character(e$message)
              )
            )
          )
        })
      },
      "stats" = {
        tryCatch({
          if (exists("mod_stats_descriptives_ui")) {
            mod_stats_descriptives_ui("stats_descriptives")
          } else {
            div(h1("Statistiques Descriptives"), p("Module non disponible"))
          }
        }, error = function(e) {
          div(h1("Statistiques Descriptives"), p("Erreur lors du chargement du module."))
        })
      },
      "correlations" = {
        tryCatch({
          if (exists("mod_correlations_ui")) {
            mod_correlations_ui("correlations")
          } else {
            div(h1("Corrélations"), p("Module non disponible"))
          }
        }, error = function(e) {
          div(h1("Corrélations"), p("Erreur lors du chargement du module."))
        })
      },
      "acp" = {
        tryCatch({
          if (exists("mod_acp_ui")) {
            mod_acp_ui("acp")
          } else {
            div(h1("ACP"), p("Module non disponible"))
          }
        }, error = function(e) {
          div(h1("ACP"), p("Erreur lors du chargement du module."))
        })
      },
      "afc" = {
        tryCatch({
          if (exists("mod_afc_ui")) {
            mod_afc_ui("afc")
          } else {
            div(h1("AFC"), p("Module non disponible"))
          }
        }, error = function(e) {
          div(h1("AFC"), p("Erreur lors du chargement du module."))
        })
      },
      "famd" = {
        tryCatch({
          if (exists("mod_famd_ui")) {
            mod_famd_ui("famd")
          } else {
            div(h1("FAMD"), p("Module non disponible"))
          }
        }, error = function(e) {
          div(h1("FAMD"), p("Erreur lors du chargement du module."))
        })
      },
      "clustering" = {
        tryCatch({
          if (exists("mod_clustering_ui")) {
            mod_clustering_ui("clustering")
          } else {
            div(h1("Clustering"), p("Module non disponible"))
          }
        }, error = function(e) {
          div(h1("Clustering"), p("Erreur lors du chargement du module."))
        })
      },
      "regression" = {
        tryCatch({
          if (exists("mod_regression_ui")) {
            mod_regression_ui("regression")
          } else {
            div(h1("Régression"), p("Module non disponible"))
          }
        }, error = function(e) {
          div(h1("Régression"), p("Erreur lors du chargement du module."))
        })
      },
      "amorces" = {
        tryCatch({
          if (exists("mod_amorces_adn_ui")) {
            mod_amorces_adn_ui("amorces_adn")
          } else {
            div(h1("Amorces ADN"), p("Module non disponible"))
          }
        }, error = function(e) {
          div(h1("Amorces ADN"), p("Erreur lors du chargement du module."))
        })
      },
      "temporal" = {
        tryCatch({
          if (exists("mod_temporal_ui")) {
            mod_temporal_ui("temporal")
          } else {
            div(h1("Analyse Temporelle"), p("Module non disponible"))
          }
        }, error = function(e) {
          div(h1("Analyse Temporelle"), p("Erreur lors du chargement du module."))
        })
      },
      "ar_prediction" = {
        tryCatch({
          if (exists("mod_ar_prediction_ui")) {
            mod_ar_prediction_ui("ar_prediction")
          } else {
            div(h1("Prédiction AR"), p("Module non disponible"))
          }
        }, error = function(e) {
          div(h1("Prédiction AR"), p("Erreur lors du chargement du module."))
        })
      },
      # Par défaut, afficher l'accueil
      {
        div(h1("Accueil"), p("Bienvenue sur le Dashboard Projet Croisé"))
      }
    )
  })
  
  # ============================================================================
  # DONNÉES RÉACTIVES (db.csv chargé au démarrage)
  # ============================================================================
  
  # Données principales réactives
  db_data_reactive <- reactive({
    if (nrow(db_data) > 0) {
      return(db_data)
    }
    return(data.frame())
  })
  
  # Note conditionnelle sur les amorces ADN
  output$note_amorces_adn <- renderUI({
    data <- db_data_reactive()
    
    # Vérifier si une colonne de séquence ADN existe
    has_sequence <- FALSE
    if (!is.null(data) && nrow(data) > 0) {
      possible_cols <- c("sequence_adn", "sequence", "adn", "dna", "seq")
      for (col in names(data)) {
        col_lower <- tolower(col)
        if (any(sapply(possible_cols, function(x) grepl(x, col_lower, fixed = TRUE)))) {
          has_sequence <- TRUE
          break
        }
      }
    }
    
    if (has_sequence) {
      return(NULL)  # Pas besoin d'afficher la note si les données sont présentes
    }
    
    # Afficher la note seulement si pas de colonne séquence détectée
    div(
      style = "margin-top: 2rem; padding: 1rem; background-color: #E7F3FF; border-left: 4px solid #1E5A8E; border-radius: 0.375rem;",
      p(
        style = "margin: 0; color: #0C5460; font-size: 0.9rem;",
        icon("info-circle", style = "margin-right: 0.5rem;"),
        tags$strong("Note :"), " Le module 'Amorces ADN' nécessite une colonne contenant des séquences ADN (ex: 'sequence_adn')."
      )
    )
  })
  
  # ============================================================================
  # FILTRES COMMUNS
  # ============================================================================
  
  # Liste des pays disponibles (utilise uniquement nom_pays)
  available_countries <- reactive({
    data <- db_data_reactive()
    if (!is.null(data) && nrow(data) > 0 && "nom_pays" %in% names(data)) {
      # Utiliser uniquement nom_pays comme demandé
      countries <- unique(data[["nom_pays"]])
      countries <- countries[!is.na(countries) & countries != ""]
      if (length(countries) > 0) {
        return(sort(countries))
      }
    }
    return(character(0))
  })
  
  # Plage de dates disponible
  available_date_range <- reactive({
    data <- db_data_reactive()
    if (!is.null(data) && nrow(data) > 0) {
      # Utiliser la colonne annee pour créer une plage de dates
      if ("annee" %in% names(data)) {
        years <- data[["annee"]]
        years <- years[!is.na(years)]
        if (length(years) > 0) {
          min_year <- min(years, na.rm = TRUE)
          max_year <- max(years, na.rm = TRUE)
          # Convertir les années en dates (1er janvier de chaque année)
          return(list(
            start_date = as.Date(paste0(min_year, "-01-01")),
            end_date = as.Date(paste0(max_year, "-12-31"))
          ))
        }
      }
    }
    return(NULL)
  })
  
  # ============================================================================
  # INITIALISATION DES MODULES
  # ============================================================================
  
  # Module Statistiques
  tryCatch({
    if (exists("mod_stats_descriptives_server")) {
      mod_stats_descriptives_server(
        "stats_descriptives",
        imported_data = db_data_reactive,
        available_countries = available_countries,
        available_date_range = available_date_range
      )
    }
  }, error = function(e) {
    warning("Erreur lors de l'initialisation du module statistiques : ", e$message)
  })
  
  # Module Corrélations
  tryCatch({
    if (exists("mod_correlations_server")) {
      mod_correlations_server(
        "correlations",
        genetic_data = db_data_reactive,
        smoking_data = reactive(data.frame()),
        alcohol_data = reactive(data.frame()),
        cancer_markers_data = reactive(data.frame()),
        analytics_data = db_data_reactive,
        available_countries = available_countries,
        available_date_range = available_date_range
      )
    }
  }, error = function(e) {
    warning("Erreur lors de l'initialisation du module corrélations : ", e$message)
  })
  
  # Module ACP
  tryCatch({
    if (exists("mod_acp_server")) {
      mod_acp_server(
        "acp",
        genetic_data = db_data_reactive,
        smoking_data = reactive(data.frame()),
        alcohol_data = reactive(data.frame()),
        cancer_markers_data = reactive(data.frame()),
        analytics_data = db_data_reactive,
        available_countries = available_countries,
        available_date_range = available_date_range
      )
    }
  }, error = function(e) {
    warning("Erreur lors de l'initialisation du module ACP : ", e$message)
  })
  
  # Module AFC
  tryCatch({
    if (exists("mod_afc_server")) {
      mod_afc_server(
        "afc",
        genetic_data = db_data_reactive,
        smoking_data = reactive(data.frame()),
        alcohol_data = reactive(data.frame()),
        cancer_markers_data = reactive(data.frame()),
        analytics_data = db_data_reactive,
        available_countries = available_countries,
        available_date_range = available_date_range
      )
    }
  }, error = function(e) {
    warning("Erreur lors de l'initialisation du module AFC : ", e$message)
  })
  
  # Module FAMD
  tryCatch({
    if (exists("mod_famd_server")) {
      mod_famd_server(
        "famd",
        genetic_data = db_data_reactive,
        smoking_data = reactive(data.frame()),
        alcohol_data = reactive(data.frame()),
        cancer_markers_data = reactive(data.frame()),
        analytics_data = db_data_reactive,
        available_countries = available_countries,
        available_date_range = available_date_range
      )
    }
  }, error = function(e) {
    warning("Erreur lors de l'initialisation du module FAMD : ", e$message)
  })
  
  # Module Clustering
  tryCatch({
    if (exists("mod_clustering_server")) {
      mod_clustering_server(
        "clustering",
        genetic_data = db_data_reactive,
        smoking_data = reactive(data.frame()),
        alcohol_data = reactive(data.frame()),
        cancer_markers_data = reactive(data.frame()),
        analytics_data = db_data_reactive,
        available_countries = available_countries,
        available_date_range = available_date_range
      )
    }
  }, error = function(e) {
    warning("Erreur lors de l'initialisation du module clustering : ", e$message)
  })
  
  # Module Régression
  tryCatch({
    if (exists("mod_regression_server")) {
      mod_regression_server(
        "regression",
        genetic_data = db_data_reactive,
        smoking_data = reactive(data.frame()),
        alcohol_data = reactive(data.frame()),
        cancer_markers_data = reactive(data.frame()),
        analytics_data = db_data_reactive,
        available_countries = available_countries,
        available_date_range = available_date_range
      )
    }
  }, error = function(e) {
    warning("Erreur lors de l'initialisation du module régression : ", e$message)
  })
  
  # Module Amorces ADN
  tryCatch({
    if (exists("mod_amorces_adn_server")) {
      mod_amorces_adn_server(
        "amorces_adn",
        imported_data = db_data_reactive
      )
    }
  }, error = function(e) {
    warning("Erreur lors de l'initialisation du module amorces ADN : ", e$message)
  })
  
  # Module Analyse Temporelle
  tryCatch({
    if (exists("mod_temporal_server")) {
      mod_temporal_server(
        "temporal",
        imported_data = db_data_reactive
      )
    }
  }, error = function(e) {
    warning("Erreur lors de l'initialisation du module analyse temporelle : ", e$message)
  })
  
  # Module Prédiction AR
  tryCatch({
    if (exists("mod_ar_prediction_server")) {
      mod_ar_prediction_server(
        "ar_prediction",
        imported_data = db_data_reactive
      )
    }
  }, error = function(e) {
    warning("Erreur lors de l'initialisation du module prédiction AR : ", e$message)
  })
}

# ==============================================================================
# LANCER L'APPLICATION
# ==============================================================================

shinyApp(
  ui = ui, 
  server = server,
  options = list(
    port = 3838,
    host = "127.0.0.1",
    launch.browser = TRUE
  )
)
