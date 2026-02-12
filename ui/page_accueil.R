# ==============================================================================
# PAGE D'ACCUEIL - Dashboard Shiny
# ==============================================================================
# 
# Page d'introduction soignée et professionnelle pour le dashboard.
# Objectif : faire comprendre immédiatement le projet et impressionner visuellement.
#
# ==============================================================================

#' Interface utilisateur de la page d'accueil
#'
#' @return Une liste de composants Shiny pour la page d'accueil
#' @export
page_accueil_ui <- function() {
  
  # Charger les fonctions nécessaires avec gestion d'erreur robuste
  colors <- tryCatch({
    if (file.exists("R/theme_shiny.R")) {
      source("R/theme_shiny.R", local = TRUE)
      if (exists("get_theme_colors")) {
        get_theme_colors()
      } else {
        list(
          primary = "#1E5A8E",
          secondary = "#6C757D",
          text_primary = "#1E5A8E",
          text_secondary = "#6C757D"
        )
      }
    } else {
      list(
        primary = "#1E5A8E",
        secondary = "#6C757D",
        text_primary = "#1E5A8E",
        text_secondary = "#6C757D"
      )
    }
  }, error = function(e) {
    # En cas d'erreur, utiliser des couleurs par défaut
    list(
      primary = "#1E5A8E",
      secondary = "#6C757D",
      text_primary = "#1E5A8E",
      text_secondary = "#6C757D"
    )
  })
  
  tagList(
    
    # ==========================================================================
    # EN-TÊTE PRINCIPAL
    # ==========================================================================
    div(
      class = "container-fluid",
      style = "padding: 2rem 0; background: linear-gradient(135deg, #F8F9FA 0%, #FFFFFF 100%); border-bottom: 3px solid #1E5A8E; margin-bottom: 3rem;",
      
      div(
        class = "row justify-content-center",
        div(
          class = "col-lg-10 col-xl-9",
          
          # Titre principal
          h1(
            class = "text-center",
            style = "font-size: 2.5rem; font-weight: 700; color: #1E5A8E; margin-bottom: 1rem; border: none; padding: 0;",
            "Analyse de l'Impact des Facteurs Génétiques et Comportementaux",
            br(),
            "sur les Marqueurs du Cancer du Sein"
          ),
          
          # Sous-titre
          p(
            class = "text-center",
            style = "font-size: 1.25rem; color: #6C757D; margin-top: 1rem; font-weight: 400;",
            "Étude intégrative des déterminants génétiques et environnementaux",
            br(),
            "dans la détection et le suivi du cancer du sein"
          )
        )
      )
    ),
    
    # ==========================================================================
    # ALERTE DONNÉES MANQUANTES (si applicable)
    # ==========================================================================
    # Note: Cette sortie n'est plus nécessaire car db.csv est chargé directement
    # uiOutput("data_missing_alert"),
    
    # ==========================================================================
    # INDICATEURS CLÉS (KPI)
    # ==========================================================================
    div(
      class = "container-fluid",
      style = "margin-bottom: 3rem;",
      
      div(
        class = "row justify-content-center",
        div(
          class = "col-lg-10 col-xl-9",
          
          h2(
            class = "text-center",
            style = "color: #1E5A8E; margin-bottom: 2rem; font-size: 1.75rem;",
            "Vue d'ensemble du projet"
          ),
          
          # Row d'indicateurs clés (remplace valueBox par des divs personnalisées)
          fluidRow(
            # Nombre de patients
            column(
              width = 3,
              div(
                class = "card",
                style = "text-align: center; padding: 1.5rem; background: linear-gradient(135deg, #1E5A8E 0%, #4A7BA7 100%); color: white; border-radius: 0.5rem;",
                icon("users", style = "font-size: 2.5rem; margin-bottom: 0.5rem;"),
                h3(style = "font-size: 2.5rem; font-weight: 700; margin: 0.5rem 0;", "2,370"),
                p(style = "font-size: 0.95rem; font-weight: 500; margin: 0; opacity: 0.9;", "Patients analysés")
              )
            ),
            
            # Nombre de gènes
            column(
              width = 3,
              div(
                class = "card",
                style = "text-align: center; padding: 1.5rem; background: linear-gradient(135deg, #17A2B8 0%, #4FC3DC 100%); color: white; border-radius: 0.5rem;",
                icon("dna", style = "font-size: 2.5rem; margin-bottom: 0.5rem;"),
                h3(style = "font-size: 2.5rem; font-weight: 700; margin: 0.5rem 0;", "6"),
                p(style = "font-size: 0.95rem; font-weight: 500; margin: 0; opacity: 0.9;", "Amorces ADN")
              )
            ),
            
            # Marqueurs tumoraux
            column(
              width = 3,
              div(
                class = "card",
                style = "text-align: center; padding: 1.5rem; background: linear-gradient(135deg, #28A745 0%, #5CB85C 100%); color: white; border-radius: 0.5rem;",
                icon("chart-line", style = "font-size: 2.5rem; margin-bottom: 0.5rem;"),
                h3(style = "font-size: 2.5rem; font-weight: 700; margin: 0.5rem 0;", "CA 15-3"),
                p(style = "font-size: 0.95rem; font-weight: 500; margin: 0; opacity: 0.9;", "Marqueurs tumoraux")
              )
            ),
            
            # Facteurs comportementaux
            column(
              width = 3,
              div(
                class = "card",
                style = "text-align: center; padding: 1.5rem; background: linear-gradient(135deg, #FFC107 0%, #FFD54F 100%); color: #856404; border-radius: 0.5rem;",
                icon("user-md", style = "font-size: 2.5rem; margin-bottom: 0.5rem;"),
                h3(style = "font-size: 2.5rem; font-weight: 700; margin: 0.5rem 0;", "3"),
                p(style = "font-size: 0.95rem; font-weight: 500; margin: 0; opacity: 0.9;", "Facteurs comportementaux")
              )
            )
          )
        )
      )
    ),
    
    # ==========================================================================
    # CONTEXTE ET PROBLÉMATIQUE
    # ==========================================================================
    # Ici, je présente le contexte scientifique et la problématique principale
    # du projet pour que l'utilisateur comprenne immédiatement les enjeux.
    
    div(
      class = "container-fluid",
      style = "margin-bottom: 3rem;",
      
      div(
        class = "row justify-content-center",
        div(
          class = "col-lg-10 col-xl-9",
          
          div(
            class = "card",
            style = "border-left: 4px solid #1E5A8E;",
            
            div(
              class = "card-header",
              style = "background-color: #F8F9FA; border-bottom: 2px solid #1E5A8E;",
              h3(
                style = "margin: 0; color: #1E5A8E; font-size: 1.5rem; font-weight: 600;",
                icon("info-circle", style = "margin-right: 0.5rem;"),
                "Contexte et Problématique"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 2rem;",
              
              p(
                style = "font-size: 1.1rem; line-height: 1.8; color: #2C3E50; margin-bottom: 1.5rem;",
                "Le cancer du sein représente l'une des principales causes de mortalité par cancer chez les femmes.",
                "Sa survenue résulte de l'interaction complexe entre des facteurs génétiques prédisposants",
                "et des facteurs environnementaux ou comportementaux modifiables."
              ),
              
              p(
                style = "font-size: 1.1rem; line-height: 1.8; color: #2C3E50; margin-bottom: 1.5rem;",
                tags$strong("Problématique principale :"),
                "Comment identifier les gènes présentant une tolérance aux mutations et comprendre",
                "leur interaction avec les facteurs comportementaux (tabac, alcool) pour mieux",
                "prédire et suivre l'évolution du cancer du sein via les marqueurs tumoraux (CA 15-3) ?"
              ),
              
              div(
                class = "row",
                style = "margin-top: 1.5rem;",
                
                # Objectif 1
                div(
                  class = "col-md-6",
                  style = "padding: 1rem; background-color: #F8F9FA; border-radius: 0.375rem; margin-bottom: 1rem;",
                  h4(
                    style = "color: #1E5A8E; font-size: 1.1rem; margin-bottom: 0.5rem;",
                    icon("bullseye", style = "margin-right: 0.5rem;"),
                    "Objectif 1"
                  ),
                  p(
                    style = "margin: 0; color: #2C3E50;",
                    "Détecter les gènes avec tolérance aux mutations et caractériser",
                    "leur profil mutationnel dans une cohorte de patients."
                  )
                ),
                
                # Objectif 2
                div(
                  class = "col-md-6",
                  style = "padding: 1rem; background-color: #F8F9FA; border-radius: 0.375rem; margin-bottom: 1rem;",
                  h4(
                    style = "color: #1E5A8E; font-size: 1.1rem; margin-bottom: 0.5rem;",
                    icon("bullseye", style = "margin-right: 0.5rem;"),
                    "Objectif 2"
                  ),
                  p(
                    style = "margin: 0; color: #2C3E50;",
                    "Analyser l'impact des facteurs comportementaux (tabac, alcool)",
                    "sur l'expression des marqueurs tumoraux du cancer du sein."
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # ==========================================================================
    # DONNÉES UTILISÉES
    # ==========================================================================
    # À ce niveau, je détaille les trois types de données utilisées dans
    # l'analyse : génétiques, comportementales et marqueurs tumoraux.
    
    div(
      class = "container-fluid",
      style = "margin-bottom: 3rem;",
      
      div(
        class = "row justify-content-center",
        div(
          class = "col-lg-10 col-xl-9",
          
          div(
            class = "card",
            style = "border-left: 4px solid #17A2B8;",
            
            div(
              class = "card-header",
              style = "background-color: #F8F9FA; border-bottom: 2px solid #17A2B8;",
              h3(
                style = "margin: 0; color: #1E5A8E; font-size: 1.5rem; font-weight: 600;",
                icon("database", style = "margin-right: 0.5rem;"),
                "Données Utilisées"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 2rem;",
              
              div(
                class = "row",
                
                # Données génétiques
                div(
                  class = "col-md-4",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #F8F9FA; border-radius: 0.375rem; height: 100%;",
                    h4(
                      style = "color: #1E5A8E; font-size: 1.2rem; margin-bottom: 1rem;",
                      icon("dna", style = "margin-right: 0.5rem; color: #1E5A8E;"),
                      "Données Génétiques"
                    ),
                    tags$ul(
                      style = "list-style: none; padding: 0; margin: 0;",
                      tags$li(
                        style = "padding: 0.5rem 0; color: #2C3E50;",
                        icon("check-circle", style = "color: #28A745; margin-right: 0.5rem;"),
                        "Profils mutationnels par gène"
                      ),
                      tags$li(
                        style = "padding: 0.5rem 0; color: #2C3E50;",
                        icon("check-circle", style = "color: #28A745; margin-right: 0.5rem;"),
                        "Scores de tolérance aux mutations"
                      ),
                      tags$li(
                        style = "padding: 0.5rem 0; color: #2C3E50;",
                        icon("check-circle", style = "color: #28A745; margin-right: 0.5rem;"),
                        "Séquences ADN complètes"
                      ),
                      tags$li(
                        style = "padding: 0.5rem 0; color: #2C3E50;",
                        icon("check-circle", style = "color: #28A745; margin-right: 0.5rem;"),
                        "Détection d'amorces génétiques"
                      )
                    )
                  )
                ),
                
                # Données comportementales
                div(
                  class = "col-md-4",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #F8F9FA; border-radius: 0.375rem; height: 100%;",
                    h4(
                      style = "color: #1E5A8E; font-size: 1.2rem; margin-bottom: 1rem;",
                      icon("user-md", style = "margin-right: 0.5rem; color: #1E5A8E;"),
                      "Facteurs Comportementaux"
                    ),
                    tags$ul(
                      style = "list-style: none; padding: 0; margin: 0;",
                      tags$li(
                        style = "padding: 0.5rem 0; color: #2C3E50;",
                        icon("smoking-ban", style = "color: #DC3545; margin-right: 0.5rem;"),
                        "Statut tabagique (fumeur/non-fumeur)"
                      ),
                      tags$li(
                        style = "padding: 0.5rem 0; color: #2C3E50;",
                        icon("wine-bottle", style = "color: #FFC107; margin-right: 0.5rem;"),
                        "Consommation d'alcool (niveaux)"
                      ),
                      tags$li(
                        style = "padding: 0.5rem 0; color: #2C3E50;",
                        icon("calendar-alt", style = "color: #17A2B8; margin-right: 0.5rem;"),
                        "Historique et durée d'exposition"
                      )
                    )
                  )
                ),
                
                # Marqueurs tumoraux
                div(
                  class = "col-md-4",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #F8F9FA; border-radius: 0.375rem; height: 100%;",
                    h4(
                      style = "color: #1E5A8E; font-size: 1.2rem; margin-bottom: 1rem;",
                      icon("chart-line", style = "margin-right: 0.5rem; color: #1E5A8E;"),
                      "Marqueurs Tumoraux"
                    ),
                    tags$ul(
                      style = "list-style: none; padding: 0; margin: 0;",
                      tags$li(
                        style = "padding: 0.5rem 0; color: #2C3E50;",
                        icon("check-circle", style = "color: #28A745; margin-right: 0.5rem;"),
                        "CA 15-3 (antigène cancéreux)"
                      ),
                      tags$li(
                        style = "padding: 0.5rem 0; color: #2C3E50;",
                        icon("check-circle", style = "color: #28A745; margin-right: 0.5rem;"),
                        "Niveaux sériques (U/mL)"
                      ),
                      tags$li(
                        style = "padding: 0.5rem 0; color: #2C3E50;",
                        icon("check-circle", style = "color: #28A745; margin-right: 0.5rem;"),
                        "Suivi temporel (si disponible)"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # ==========================================================================
    # MÉTHODE
    # ==========================================================================
    # Ici, j'explique la méthode principale de détection de gènes avec
    # tolérance aux mutations, en détaillant les étapes du processus.
    
    div(
      class = "container-fluid",
      style = "margin-bottom: 3rem;",
      
      div(
        class = "row justify-content-center",
        div(
          class = "col-lg-10 col-xl-9",
          
          div(
            class = "card",
            style = "border-left: 4px solid #28A745;",
            
            div(
              class = "card-header",
              style = "background-color: #F8F9FA; border-bottom: 2px solid #28A745;",
              h3(
                style = "margin: 0; color: #1E5A8E; font-size: 1.5rem; font-weight: 600;",
                icon("flask", style = "margin-right: 0.5rem;"),
                "Méthode : Détection de Gènes avec Tolérance aux Mutations"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 2rem;",
              
                    p(
                      style = "font-size: 1.1rem; line-height: 1.8; color: #2C3E50; margin-bottom: 1.5rem;",
                      "La détection de gènes avec tolérance aux mutations est une approche computationnelle",
                      "permettant d'identifier les gènes qui peuvent accumuler des mutations sans compromettre",
                      "la fonction cellulaire. Cette propriété est particulièrement importante dans le contexte",
                      "du cancer, où certaines mutations peuvent conférer un avantage sélectif aux cellules tumorales."
                    ),
                    
                    p(
                      style = "font-size: 1.1rem; line-height: 1.8; color: #2C3E50; margin-bottom: 1.5rem;",
                      tags$strong("Détection d'amorces ADN :"),
                      " Le projet inclut également la détection de 6 amorces génétiques spécifiques",
                      " (gggccc, acctcca, tttttta, gggacggg, atatatat, gtacacgt) dans les séquences ADN.",
                      " Cette analyse utilise une fenêtre glissante avec tolérance aux erreurs (2 mutations)",
                      " pour identifier les occurrences même en présence de variations naturelles."
                    ),
              
              div(
                class = "row",
                style = "margin-top: 2rem;",
                
                # Étape 1
                div(
                  class = "col-md-6",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background: linear-gradient(135deg, #FFFFFF 0%, #F8F9FA 100%); border: 1px solid #E9ECEF; border-radius: 0.375rem; height: 100%;",
                    div(
                      style = "display: flex; align-items: center; margin-bottom: 1rem;",
                      span(
                        style = "background-color: #1E5A8E; color: white; width: 2.5rem; height: 2.5rem; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: 700; font-size: 1.2rem; margin-right: 1rem;",
                        "1"
                      ),
                      h4(
                        style = "margin: 0; color: #1E5A8E; font-size: 1.1rem;",
                        "Analyse des Mutations"
                      )
                    ),
                    p(
                      style = "margin: 0; color: #2C3E50; line-height: 1.6;",
                      "Identification et catalogage des mutations par gène dans la cohorte de patients.",
                      "Calcul de la fréquence mutationnelle et caractérisation des types de mutations."
                    )
                  )
                ),
                
                # Étape 2
                div(
                  class = "col-md-6",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background: linear-gradient(135deg, #FFFFFF 0%, #F8F9FA 100%); border: 1px solid #E9ECEF; border-radius: 0.375rem; height: 100%;",
                    div(
                      style = "display: flex; align-items: center; margin-bottom: 1rem;",
                      span(
                        style = "background-color: #1E5A8E; color: white; width: 2.5rem; height: 2.5rem; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: 700; font-size: 1.2rem; margin-right: 1rem;",
                        "2"
                      ),
                      h4(
                        style = "margin: 0; color: #1E5A8E; font-size: 1.1rem;",
                        "Calcul de la Tolérance"
                      )
                    ),
                    p(
                      style = "margin: 0; color: #2C3E50; line-height: 1.6;",
                      "Évaluation de la tolérance aux mutations basée sur des modèles prédictifs",
                      "et des annotations fonctionnelles. Score de tolérance calculé pour chaque gène."
                    )
                  )
                ),
                
                # Étape 3
                div(
                  class = "col-md-6",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background: linear-gradient(135deg, #FFFFFF 0%, #F8F9FA 100%); border: 1px solid #E9ECEF; border-radius: 0.375rem; height: 100%;",
                    div(
                      style = "display: flex; align-items: center; margin-bottom: 1rem;",
                      span(
                        style = "background-color: #1E5A8E; color: white; width: 2.5rem; height: 2.5rem; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: 700; font-size: 1.2rem; margin-right: 1rem;",
                        "3"
                      ),
                      h4(
                        style = "margin: 0; color: #1E5A8E; font-size: 1.1rem;",
                        "Sélection des Gènes"
                      )
                    ),
                    p(
                      style = "margin: 0; color: #2C3E50; line-height: 1.6;",
                      "Identification des gènes présentant une tolérance élevée aux mutations.",
                      "Ces gènes sont considérés comme des candidats pour l'analyse d'association."
                    )
                  )
                ),
                
                # Étape 4
                div(
                  class = "col-md-6",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background: linear-gradient(135deg, #FFFFFF 0%, #F8F9FA 100%); border: 1px solid #E9ECEF; border-radius: 0.375rem; height: 100%;",
                    div(
                      style = "display: flex; align-items: center; margin-bottom: 1rem;",
                      span(
                        style = "background-color: #1E5A8E; color: white; width: 2.5rem; height: 2.5rem; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: 700; font-size: 1.2rem; margin-right: 1rem;",
                        "4"
                      ),
                      h4(
                        style = "margin: 0; color: #1E5A8E; font-size: 1.1rem;",
                        "Intégration Multi-Omiques"
                      )
                    ),
                    p(
                      style = "margin: 0; color: #2C3E50; line-height: 1.6;",
                      "Corrélation avec les facteurs comportementaux et les marqueurs tumoraux.",
                      "Analyse intégrative pour identifier les interactions significatives."
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # ==========================================================================
    # ANALYSES RÉALISÉES
    # ==========================================================================
    # À ce niveau, je présente toutes les méthodes d'analyse disponibles
    # dans le dashboard : statistiques, ACP, AFC, FAMD, clustering et régression.
    
    div(
      class = "container-fluid",
      style = "margin-bottom: 3rem;",
      
      div(
        class = "row justify-content-center",
        div(
          class = "col-lg-10 col-xl-9",
          
          div(
            class = "card",
            style = "border-left: 4px solid #6C757D;",
            
            div(
              class = "card-header",
              style = "background-color: #F8F9FA; border-bottom: 2px solid #6C757D;",
              h3(
                style = "margin: 0; color: #1E5A8E; font-size: 1.5rem; font-weight: 600;",
                icon("chart-bar", style = "margin-right: 0.5rem;"),
                "Analyses Réalisées"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 2rem;",
              
              p(
                style = "font-size: 1.1rem; line-height: 1.8; color: #2C3E50; margin-bottom: 2rem;",
                "Ce dashboard présente les résultats d'analyses statistiques et computationnelles",
                "multiples, permettant une compréhension approfondie des relations entre",
                "les facteurs génétiques, comportementaux et les marqueurs du cancer du sein.",
                "Chaque méthode d'analyse est accessible via les onglets de navigation ci-dessus."
              ),
              
              div(
                class = "row",
                
                # Statistiques descriptives
                div(
                  class = "col-md-6 col-lg-4",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #FFFFFF; border: 2px solid #1E5A8E; border-radius: 0.375rem; height: 100%; transition: all 0.3s ease;",
                    h4(
                      style = "color: #1E5A8E; font-size: 1.2rem; margin-bottom: 1rem; display: flex; align-items: center;",
                      icon("chart-bar", style = "margin-right: 0.75rem; font-size: 1.5rem;"),
                      "Statistiques"
                    ),
                    tags$ul(
                      style = "list-style: none; padding: 0; margin: 0;",
                      tags$li(style = "padding: 0.5rem 0; color: #2C3E50; font-size: 0.9rem;", icon("check", style = "color: #28A745; margin-right: 0.5rem;"), "Statistiques descriptives"),
                      tags$li(style = "padding: 0.5rem 0; color: #2C3E50; font-size: 0.9rem;", icon("check", style = "color: #28A745; margin-right: 0.5rem;"), "Distributions et visualisations"),
                      tags$li(style = "padding: 0.5rem 0; color: #2C3E50; font-size: 0.9rem;", icon("check", style = "color: #28A745; margin-right: 0.5rem;"), "Moyennes, médianes, écarts-types")
                    )
                  )
                ),
                
                # Corrélations
                div(
                  class = "col-md-6 col-lg-4",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #FFFFFF; border: 2px solid #17A2B8; border-radius: 0.375rem; height: 100%; transition: all 0.3s ease;",
                    h4(
                      style = "color: #1E5A8E; font-size: 1.2rem; margin-bottom: 1rem; display: flex; align-items: center;",
                      icon("project-diagram", style = "margin-right: 0.75rem; font-size: 1.5rem;"),
                      "Corrélations"
                    ),
                    p(
                      style = "color: #2C3E50; margin-bottom: 1rem; font-size: 0.95rem;",
                      tags$strong("Analyse des corrélations")
                    ),
                    tags$ul(
                      style = "list-style: none; padding: 0; margin: 0;",
                      tags$li(style = "padding: 0.5rem 0; color: #2C3E50; font-size: 0.9rem;", icon("check", style = "color: #28A745; margin-right: 0.5rem;"), "Pearson et Spearman"),
                      tags$li(style = "padding: 0.5rem 0; color: #2C3E50; font-size: 0.9rem;", icon("check", style = "color: #28A745; margin-right: 0.5rem;"), "Matrice de corrélation"),
                      tags$li(style = "padding: 0.5rem 0; color: #2C3E50; font-size: 0.9rem;", icon("check", style = "color: #28A745; margin-right: 0.5rem;"), "Heatmap interactive")
                    )
                  )
                ),
                
                # ACP
                div(
                  class = "col-md-6 col-lg-4",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #FFFFFF; border: 2px solid #17A2B8; border-radius: 0.375rem; height: 100%; transition: all 0.3s ease;",
                    h4(
                      style = "color: #1E5A8E; font-size: 1.2rem; margin-bottom: 1rem; display: flex; align-items: center;",
                      icon("project-diagram", style = "margin-right: 0.75rem; font-size: 1.5rem;"),
                      "ACP"
                    ),
                    p(
                      style = "color: #2C3E50; margin-bottom: 1rem; font-size: 0.95rem;",
                      tags$strong("Analyse en Composantes Principales")
                    ),
                    p(
                      style = "color: #6C757D; margin: 0; font-size: 0.9rem; line-height: 1.6;",
                      "Réduction de dimensionnalité pour visualiser les relations",
                      "entre variables génétiques et marqueurs tumoraux."
                    )
                  )
                ),
                
                # AFC
                div(
                  class = "col-md-6 col-lg-4",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #FFFFFF; border: 2px solid #28A745; border-radius: 0.375rem; height: 100%; transition: all 0.3s ease;",
                    h4(
                      style = "color: #1E5A8E; font-size: 1.2rem; margin-bottom: 1rem; display: flex; align-items: center;",
                      icon("sitemap", style = "margin-right: 0.75rem; font-size: 1.5rem;"),
                      "AFC"
                    ),
                    p(
                      style = "color: #2C3E50; margin-bottom: 1rem; font-size: 0.95rem;",
                      tags$strong("Analyse Factorielle des Correspondances")
                    ),
                    p(
                      style = "color: #6C757D; margin: 0; font-size: 0.9rem; line-height: 1.6;",
                      "Analyse des associations entre variables catégorielles",
                      "(gènes mutés vs non mutés, facteurs comportementaux)."
                    )
                  )
                ),
                
                # FAMD
                div(
                  class = "col-md-6 col-lg-4",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #FFFFFF; border: 2px solid #FFC107; border-radius: 0.375rem; height: 100%; transition: all 0.3s ease;",
                    h4(
                      style = "color: #1E5A8E; font-size: 1.2rem; margin-bottom: 1rem; display: flex; align-items: center;",
                      icon("network-wired", style = "margin-right: 0.75rem; font-size: 1.5rem;"),
                      "FAMD"
                    ),
                    p(
                      style = "color: #2C3E50; margin-bottom: 1rem; font-size: 0.95rem;",
                      tags$strong("Factor Analysis of Mixed Data")
                    ),
                    p(
                      style = "color: #6C757D; margin: 0; font-size: 0.9rem; line-height: 1.6;",
                      "Analyse factorielle pour données mixtes (quantitatives et qualitatives),",
                      "permettant d'explorer les relations entre tous les types de variables."
                    )
                  )
                ),
                
                # Clustering
                div(
                  class = "col-md-6 col-lg-4",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #FFFFFF; border: 2px solid #6C757D; border-radius: 0.375rem; height: 100%; transition: all 0.3s ease;",
                    h4(
                      style = "color: #1E5A8E; font-size: 1.2rem; margin-bottom: 1rem; display: flex; align-items: center;",
                      icon("object-group", style = "margin-right: 0.75rem; font-size: 1.5rem;"),
                      "Clustering"
                    ),
                    p(
                      style = "color: #2C3E50; margin-bottom: 1rem; font-size: 0.95rem;",
                      tags$strong("Clustering non supervisé")
                    ),
                    tags$ul(
                      style = "list-style: none; padding: 0; margin: 0;",
                      tags$li(style = "padding: 0.5rem 0; color: #2C3E50; font-size: 0.9rem;", icon("check", style = "color: #28A745; margin-right: 0.5rem;"), "K-means"),
                      tags$li(style = "padding: 0.5rem 0; color: #2C3E50; font-size: 0.9rem;", icon("check", style = "color: #28A745; margin-right: 0.5rem;"), "Clustering hiérarchique"),
                      tags$li(style = "padding: 0.5rem 0; color: #2C3E50; font-size: 0.9rem;", icon("check", style = "color: #28A745; margin-right: 0.5rem;"), "Identification de groupes de patients")
                    )
                  )
                ),
                
                # Machine Learning / Régression
                div(
                  class = "col-md-6 col-lg-4",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #FFFFFF; border: 2px solid #DC3545; border-radius: 0.375rem; height: 100%; transition: all 0.3s ease;",
                    h4(
                      style = "color: #1E5A8E; font-size: 1.2rem; margin-bottom: 1rem; display: flex; align-items: center;",
                      icon("chart-line", style = "margin-right: 0.75rem; font-size: 1.5rem;"),
                      "Régression"
                    ),
                    p(
                      style = "color: #2C3E50; margin-bottom: 1rem; font-size: 0.95rem;",
                      tags$strong("Modèles supervisés")
                    ),
                    tags$ul(
                      style = "list-style: none; padding: 0; margin: 0;",
                      tags$li(style = "padding: 0.5rem 0; color: #2C3E50; font-size: 0.9rem;", icon("check", style = "color: #28A745; margin-right: 0.5rem;"), "Régression linéaire multiple"),
                      tags$li(style = "padding: 0.5rem 0; color: #2C3E50; font-size: 0.9rem;", icon("check", style = "color: #28A745; margin-right: 0.5rem;"), "LASSO / Ridge"),
                      tags$li(style = "padding: 0.5rem 0; color: #2C3E50; font-size: 0.9rem;", icon("check", style = "color: #28A745; margin-right: 0.5rem;"), "Prédiction des marqueurs tumoraux")
                    )
                  )
                ),
                
                # Amorces ADN (Nouveau - Basé sur GitHub)
                div(
                  class = "col-md-6 col-lg-4",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #FFFFFF; border: 2px solid #9C27B0; border-radius: 0.375rem; height: 100%; transition: all 0.3s ease;",
                    h4(
                      style = "color: #1E5A8E; font-size: 1.2rem; margin-bottom: 1rem; display: flex; align-items: center;",
                      icon("dna", style = "margin-right: 0.75rem; font-size: 1.5rem; color: #9C27B0;"),
                      "Amorces ADN"
                    ),
                    p(
                      style = "color: #2C3E50; margin-bottom: 1rem; font-size: 0.95rem;",
                      tags$strong("Détection d'amorces génétiques")
                    ),
                    p(
                      style = "color: #6C757D; margin-bottom: 1rem; font-size: 0.9rem; line-height: 1.6;",
                      "Détection de 6 amorces spécifiques dans les séquences ADN",
                      "avec tolérance aux mutations (2 erreurs)."
                    ),
                    tags$ul(
                      style = "list-style: none; padding: 0; margin: 0;",
                      tags$li(style = "padding: 0.5rem 0; color: #2C3E50; font-size: 0.9rem;", icon("check", style = "color: #28A745; margin-right: 0.5rem;"), "6 amorces détectées (gggccc, acctcca, etc.)"),
                      tags$li(style = "padding: 0.5rem 0; color: #2C3E50; font-size: 0.9rem;", icon("check", style = "color: #28A745; margin-right: 0.5rem;"), "Visualisations par pays et par amorce"),
                      tags$li(style = "padding: 0.5rem 0; color: #2C3E50; font-size: 0.9rem;", icon("check", style = "color: #28A745; margin-right: 0.5rem;"), "Statistiques détaillées")
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # ==========================================================================
    # PIED DE PAGE / APPEL À L'ACTION
    # ==========================================================================
    # Ici, je propose des boutons de navigation rapide vers les principales
    # sections d'analyse pour faciliter l'exploration du dashboard.
    
    div(
      class = "container-fluid",
      style = "margin-bottom: 3rem; padding: 2rem 0; background-color: #F8F9FA; border-top: 2px solid #E9ECEF;",
      
      div(
        class = "row justify-content-center",
        div(
          class = "col-lg-10 col-xl-9 text-center",
          
          h3(
            style = "color: #1E5A8E; margin-bottom: 1rem; font-size: 1.5rem;",
            "Explorez les analyses détaillées"
          ),
          
          p(
            style = "color: #6C757D; font-size: 1.1rem; margin-bottom: 1.5rem;",
            "Utilisez la navigation ci-dessus pour accéder aux différentes sections d'analyse",
            "et visualiser les résultats interactifs de ce projet de recherche."
          ),
          
          div(
            style = "display: flex; justify-content: center; gap: 1rem; flex-wrap: wrap;",
            tags$a(
              href = "#",
              onclick = "Shiny.setInputValue('nav_amorces', Math.random(), {priority: 'event'}); return false;",
              class = "btn btn-primary btn-lg",
              style = "font-size: 1rem; padding: 0.75rem 1.5rem; text-decoration: none; color: white;",
              tags$span(icon("dna"), " Analyse Génétique")
            ),
            tags$a(
              href = "#",
              onclick = "Shiny.setInputValue('nav_stats', Math.random(), {priority: 'event'}); return false;",
              class = "btn btn-info btn-lg",
              style = "font-size: 1rem; padding: 0.75rem 1.5rem; text-decoration: none; color: white;",
              tags$span(icon("user-md"), " Analyse Comportementale")
            ),
            tags$a(
              href = "#",
              onclick = "Shiny.setInputValue('nav_regression', Math.random(), {priority: 'event'}); return false;",
              class = "btn btn-success btn-lg",
              style = "font-size: 1rem; padding: 0.75rem 1.5rem; text-decoration: none; color: white;",
              tags$span(icon("chart-line"), " Marqueurs Tumoraux")
            )
          )
        )
      )
    )
  )
}
