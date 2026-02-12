# ==============================================================================
# PAGE LIMITES ET PERSPECTIVES - Dashboard Shiny
# ==============================================================================
# 
# Page dédiée à l'analyse critique du projet : limites, biais, et perspectives.
# Ton scientifique, clair et honnête.
#
# ==============================================================================

#' Interface utilisateur de la page limites et perspectives
#'
#' @return Une liste de composants Shiny pour la page limites et perspectives
#' @export
page_limites_perspectives_ui <- function() {
  
  # Charger les fonctions nécessaires
  source("R/theme_shiny.R", local = TRUE)
  
  # Charger les couleurs du thème
  colors <- get_theme_colors()
  
  tagList(
    
    # ==========================================================================
    # EN-TÊTE PRINCIPAL
    # ==========================================================================
    div(
      class = "container-fluid",
      style = "padding: 2rem 0; background: linear-gradient(135deg, #F8F9FA 0%, #FFFFFF 100%); border-bottom: 3px solid #6C757D; margin-bottom: 3rem;",
      
      div(
        class = "row justify-content-center",
        div(
          class = "col-lg-10 col-xl-9",
          
          # Titre principal
          h1(
            class = "text-center",
            style = "font-size: 2.5rem; font-weight: 700; color: #1E5A8E; margin-bottom: 1rem; border: none; padding: 0;",
            icon("balance-scale", style = "margin-right: 0.5rem;"),
            "Limites et Perspectives"
          ),
          
          # Sous-titre
          p(
            class = "text-center",
            style = "font-size: 1.25rem; color: #6C757D; margin-top: 1rem; font-weight: 400;",
            "Analyse critique du projet et réflexion sur les améliorations futures"
          )
        )
      )
    ),
    
    # ==========================================================================
    # LIMITES DES DONNÉES
    # ==========================================================================
    div(
      class = "container-fluid",
      style = "margin-bottom: 3rem;",
      
      div(
        class = "row justify-content-center",
        div(
          class = "col-lg-10 col-xl-9",
          
          div(
            class = "card",
            style = "border-left: 4px solid #DC3545;",
            
            div(
              class = "card-header",
              style = "background-color: #F8F9FA; border-bottom: 2px solid #DC3545;",
              h2(
                style = "margin: 0; color: #1E5A8E; font-size: 1.75rem; font-weight: 600;",
                icon("exclamation-triangle", style = "margin-right: 0.5rem; color: #DC3545;"),
                "Limites des Données"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 2rem;",
              
              div(
                class = "row",
                
                # Limite 1 : Données observationnelles
                div(
                  class = "col-md-6",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #FFF5F5; border-left: 3px solid #DC3545; border-radius: 0.375rem; height: 100%;",
                    h3(
                      style = "color: #DC3545; font-size: 1.2rem; margin-bottom: 0.75rem;",
                      icon("database", style = "margin-right: 0.5rem;"),
                      "Nature observationnelle"
                    ),
                    p(
                      style = "color: #2C3E50; line-height: 1.8; margin: 0;",
                      "Les données utilisées sont de nature observationnelle et proviennent",
                      "de fichiers CSV locaux. Cette approche présente plusieurs limitations :",
                      tags$ul(
                        style = "margin-top: 0.5rem; padding-left: 1.5rem;",
                        tags$li("Absence de contrôle expérimental sur les variables"),
                        tags$li("Pas de randomisation des groupes"),
                        tags$li("Données potentiellement incomplètes ou avec valeurs manquantes"),
                        tags$li("Pas de validation externe sur des données indépendantes")
                      )
                    )
                  )
                ),
                
                # Limite 2 : Taille de l'échantillon
                div(
                  class = "col-md-6",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #FFF5F5; border-left: 3px solid #DC3545; border-radius: 0.375rem; height: 100%;",
                    h3(
                      style = "color: #DC3545; font-size: 1.2rem; margin-bottom: 0.75rem;",
                      icon("users", style = "margin-right: 0.5rem;"),
                      "Taille de l'échantillon"
                    ),
                    p(
                      style = "color: #2C3E50; line-height: 1.8; margin: 0;",
                      "La taille de l'échantillon peut limiter la puissance statistique",
                      "et la généralisabilité des résultats :",
                      tags$ul(
                        style = "margin-top: 0.5rem; padding-left: 1.5rem;",
                        tags$li("Risque de manque de puissance pour détecter des effets faibles"),
                        tags$li("Limitations dans les analyses multivariées complexes"),
                        tags$li("Difficultés à généraliser à d'autres populations"),
                        tags$li("Risque de surajustement (overfitting) dans les modèles prédictifs")
                      )
                    )
                  )
                ),
                
                # Limite 3 : Qualité des données
                div(
                  class = "col-md-6",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #FFF5F5; border-left: 3px solid #DC3545; border-radius: 0.375rem; height: 100%;",
                    h3(
                      style = "color: #DC3545; font-size: 1.2rem; margin-bottom: 0.75rem;",
                      icon("check-circle", style = "margin-right: 0.5rem;"),
                      "Qualité et complétude"
                    ),
                    p(
                      style = "color: #2C3E50; line-height: 1.8; margin: 0;",
                      "La qualité des données peut varier et présenter des limitations :",
                      tags$ul(
                        style = "margin-top: 0.5rem; padding-left: 1.5rem;",
                        tags$li("Valeurs manquantes pouvant introduire des biais"),
                        tags$li("Erreurs potentielles de saisie ou de codage"),
                        tags$li("Absence de métadonnées détaillées sur la collecte"),
                        tags$li("Pas de contrôle qualité systématique des données")
                      )
                    )
                  )
                ),
                
                # Limite 4 : Variables mesurées
                div(
                  class = "col-md-6",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #FFF5F5; border-left: 3px solid #DC3545; border-radius: 0.375rem; height: 100%;",
                    h3(
                      style = "color: #DC3545; font-size: 1.2rem; margin-bottom: 0.75rem;",
                      icon("list", style = "margin-right: 0.5rem;"),
                      "Variables disponibles"
                    ),
                    p(
                      style = "color: #2C3E50; line-height: 1.8; margin: 0;",
                      "Les variables disponibles peuvent ne pas couvrir tous les aspects",
                      "pertinents de la problématique :",
                      tags$ul(
                        style = "margin-top: 0.5rem; padding-left: 1.5rem;",
                        tags$li("Facteurs confondants potentiellement non mesurés"),
                        tags$li("Variables intermédiaires non disponibles"),
                        tags$li("Absence de données longitudinales détaillées"),
                        tags$li("Limitations dans la mesure des facteurs comportementaux")
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
    # BIAIS POSSIBLES
    # ==========================================================================
    div(
      class = "container-fluid",
      style = "margin-bottom: 3rem;",
      
      div(
        class = "row justify-content-center",
        div(
          class = "col-lg-10 col-xl-9",
          
          div(
            class = "card",
            style = "border-left: 4px solid #FFC107;",
            
            div(
              class = "card-header",
              style = "background-color: #F8F9FA; border-bottom: 2px solid #FFC107;",
              h2(
                style = "margin: 0; color: #1E5A8E; font-size: 1.75rem; font-weight: 600;",
                icon("exclamation-circle", style = "margin-right: 0.5rem; color: #FFC107;"),
                "Biais Possibles"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 2rem;",
              
              div(
                class = "row",
                
                # Biais de sélection
                div(
                  class = "col-md-6",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #FFFBF0; border-left: 3px solid #FFC107; border-radius: 0.375rem; height: 100%;",
                    h3(
                      style = "color: #856404; font-size: 1.2rem; margin-bottom: 0.75rem;",
                      icon("filter", style = "margin-right: 0.5rem;"),
                      "Biais de sélection"
                    ),
                    p(
                      style = "color: #2C3E50; line-height: 1.8; margin-bottom: 0.5rem;",
                      "Les patients inclus dans l'étude peuvent ne pas être représentatifs",
                      "de la population générale :"
                    ),
                    p(
                      style = "color: #6C757D; font-size: 0.95rem; margin: 0; font-style: italic;",
                      "Exemple : sélection de patients avec accès aux soins, biais géographique,",
                      "ou critères d'inclusion/exclusion non documentés."
                    )
                  )
                ),
                
                # Biais de confusion
                div(
                  class = "col-md-6",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #FFFBF0; border-left: 3px solid #FFC107; border-radius: 0.375rem; height: 100%;",
                    h3(
                      style = "color: #856404; font-size: 1.2rem; margin-bottom: 0.75rem;",
                      icon("sitemap", style = "margin-right: 0.5rem;"),
                      "Biais de confusion"
                    ),
                    p(
                      style = "color: #2C3E50; line-height: 1.8; margin-bottom: 0.5rem;",
                      "Des facteurs confondants non mesurés peuvent influencer",
                      "les associations observées :"
                    ),
                    p(
                      style = "color: #6C757D; font-size: 0.95rem; margin: 0; font-style: italic;",
                      "Exemple : âge, statut socio-économique, antécédents familiaux,",
                      "ou autres facteurs génétiques/environnementaux non pris en compte."
                    )
                  )
                ),
                
                # Biais de mesure
                div(
                  class = "col-md-6",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #FFFBF0; border-left: 3px solid #FFC107; border-radius: 0.375rem; height: 100%;",
                    h3(
                      style = "color: #856404; font-size: 1.2rem; margin-bottom: 0.75rem;",
                      icon("ruler", style = "margin-right: 0.5rem;"),
                      "Biais de mesure"
                    ),
                    p(
                      style = "color: #2C3E50; line-height: 1.8; margin-bottom: 0.5rem;",
                      "Les variables peuvent être mesurées avec une imprécision",
                      "ou des erreurs systématiques :"
                    ),
                    p(
                      style = "color: #6C757D; font-size: 0.95rem; margin: 0; font-style: italic;",
                      "Exemple : auto-déclaration des comportements (tabac, alcool),",
                      "variabilité dans les mesures biologiques, ou erreurs de classification."
                    )
                  )
                ),
                
                # Biais de publication
                div(
                  class = "col-md-6",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #FFFBF0; border-left: 3px solid #FFC107; border-radius: 0.375rem; height: 100%;",
                    h3(
                      style = "color: #856404; font-size: 1.2rem; margin-bottom: 0.75rem;",
                      icon("file-alt", style = "margin-right: 0.5rem;"),
                      "Biais de publication"
                    ),
                    p(
                      style = "color: #2C3E50; line-height: 1.8; margin-bottom: 0.5rem;",
                      "Les données disponibles peuvent être influencées par",
                      "des biais de sélection dans la littérature :"
                    ),
                    p(
                      style = "color: #6C757D; font-size: 0.95rem; margin: 0; font-style: italic;",
                      "Exemple : tendance à publier des résultats significatifs,",
                      "ou données provenant d'études avec des designs spécifiques."
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
    # ABSENCE DE CAUSALITÉ
    # ==========================================================================
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
              h2(
                style = "margin: 0; color: #1E5A8E; font-size: 1.75rem; font-weight: 600;",
                icon("info-circle", style = "margin-right: 0.5rem; color: #6C757D;"),
                "Absence de Causalité"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 2rem;",
              
              div(
                style = "background-color: #F8F9FA; border-left: 4px solid #6C757D; padding: 1.5rem; border-radius: 0.375rem; margin-bottom: 1.5rem;",
                p(
                  style = "color: #2C3E50; font-size: 1.1rem; line-height: 1.8; margin-bottom: 1rem; font-weight: 500;",
                  icon("exclamation-triangle", style = "margin-right: 0.5rem; color: #6C757D;"),
                  tags$strong("Important :"),
                  " Les analyses présentées dans ce dashboard sont de nature",
                  tags$strong(" observationnelle et corrélationnelle"),
                  ". Elles ne permettent",
                  tags$em(" pas "),
                  "d'établir de relations causales entre les variables."
                )
              ),
              
              div(
                class = "row",
                
                # Corrélation vs causalité
                div(
                  class = "col-md-6",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #FFFFFF; border: 1px solid #E9ECEF; border-radius: 0.375rem; height: 100%;",
                    h3(
                      style = "color: #1E5A8E; font-size: 1.2rem; margin-bottom: 0.75rem;",
                      icon("link", style = "margin-right: 0.5rem;"),
                      "Corrélation ≠ Causalité"
                    ),
                    p(
                      style = "color: #2C3E50; line-height: 1.8; margin: 0;",
                      "Une association statistique entre deux variables ne signifie",
                      "pas nécessairement qu'une cause l'autre. Les associations",
                      "observées peuvent être dues à :",
                      tags$ul(
                        style = "margin-top: 0.5rem; padding-left: 1.5rem;",
                        tags$li("Facteurs confondants non mesurés"),
                        tags$li("Relations inverses (effet inverse)"),
                        tags$li("Causalité commune (variable tierce)"),
                        tags$li("Coïncidence ou artefact statistique")
                      )
                    )
                  )
                ),
                
                # Limitations méthodologiques
                div(
                  class = "col-md-6",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #FFFFFF; border: 1px solid #E9ECEF; border-radius: 0.375rem; height: 100%;",
                    h3(
                      style = "color: #1E5A8E; font-size: 1.2rem; margin-bottom: 0.75rem;",
                      icon("flask", style = "margin-right: 0.5rem;"),
                      "Limitations méthodologiques"
                    ),
                    p(
                      style = "color: #2C3E50; line-height: 1.8; margin: 0;",
                      "Pour établir la causalité, il faudrait idéalement :",
                      tags$ul(
                        style = "margin-top: 0.5rem; padding-left: 1.5rem;",
                        tags$li("Études expérimentales randomisées"),
                        tags$li("Contrôle des facteurs confondants"),
                        tags$li("Évaluation de la temporalité"),
                        tags$li("Mécanismes biologiques établis"),
                        tags$li("Réplication dans différentes populations")
                      )
                    )
                  )
                )
              ),
              
              # Avertissement
              div(
                style = "background-color: #E7F3FF; border-left: 4px solid #17A2B8; padding: 1.5rem; border-radius: 0.375rem; margin-top: 1rem;",
                p(
                  style = "color: #0C5460; margin: 0; font-size: 0.95rem;",
                  icon("lightbulb", style = "margin-right: 0.5rem;"),
                  tags$strong("Recommandation :"),
                  " Les résultats doivent être interprétés avec prudence et",
                  "considérés comme générant des hypothèses plutôt que",
                  "comme établissant des relations causales définitives."
                )
              )
            )
          )
        )
      )
    ),
    
    # ==========================================================================
    # AMÉLIORATIONS FUTURES
    # ==========================================================================
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
              h2(
                style = "margin: 0; color: #1E5A8E; font-size: 1.75rem; font-weight: 600;",
                icon("lightbulb", style = "margin-right: 0.5rem; color: #28A745;"),
                "Améliorations Futures"
              )
            ),
            
            div(
              class = "card-body",
              style = "padding: 2rem;",
              
              div(
                class = "row",
                
                # Amélioration 1 : Données
                div(
                  class = "col-md-6",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #F0F9F4; border-left: 3px solid #28A745; border-radius: 0.375rem; height: 100%;",
                    h3(
                      style = "color: #155724; font-size: 1.2rem; margin-bottom: 0.75rem;",
                      icon("database", style = "margin-right: 0.5rem;"),
                      "Amélioration des données"
                    ),
                    p(
                      style = "color: #2C3E50; line-height: 1.8; margin: 0;",
                      tags$ul(
                        style = "padding-left: 1.5rem;",
                        tags$li("Augmentation de la taille de l'échantillon"),
                        tags$li("Collecte de données longitudinales"),
                        tags$li("Inclusion de variables supplémentaires (facteurs confondants)"),
                        tags$li("Validation externe sur des données indépendantes"),
                        tags$li("Métadonnées détaillées sur la collecte"),
                        tags$li("Contrôle qualité systématique")
                      )
                    )
                  )
                ),
                
                # Amélioration 2 : Méthodologie
                div(
                  class = "col-md-6",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #F0F9F4; border-left: 3px solid #28A745; border-radius: 0.375rem; height: 100%;",
                    h3(
                      style = "color: #155724; font-size: 1.2rem; margin-bottom: 0.75rem;",
                      icon("cogs", style = "margin-right: 0.5rem;"),
                      "Amélioration méthodologique"
                    ),
                    p(
                      style = "color: #2C3E50; line-height: 1.8; margin: 0;",
                      tags$ul(
                        style = "padding-left: 1.5rem;",
                        tags$li("Analyses de sensibilité pour évaluer la robustesse"),
                        tags$li("Méthodes de correction des biais (propensity score, etc.)"),
                        tags$li("Analyses de médiation pour comprendre les mécanismes"),
                        tags$li("Modèles de régression plus sophistiqués"),
                        tags$li("Validation croisée pour les modèles prédictifs"),
                        tags$li("Analyses de réplication")
                      )
                    )
                  )
                ),
                
                # Amélioration 3 : Visualisation
                div(
                  class = "col-md-6",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #F0F9F4; border-left: 3px solid #28A745; border-radius: 0.375rem; height: 100%;",
                    h3(
                      style = "color: #155724; font-size: 1.2rem; margin-bottom: 0.75rem;",
                      icon("chart-bar", style = "margin-right: 0.5rem;"),
                      "Amélioration de la visualisation"
                    ),
                    p(
                      style = "color: #2C3E50; line-height: 1.8; margin: 0;",
                      tags$ul(
                        style = "padding-left: 1.5rem;",
                        tags$li("Graphiques interactifs plus avancés"),
                        tags$li("Visualisations de l'incertitude (intervalles de confiance)"),
                        tags$li("Comparaisons multi-études"),
                        tags$li("Tableaux de bord personnalisables"),
                        tags$li("Export de rapports automatisés")
                      )
                    )
                  )
                ),
                
                # Amélioration 4 : Interprétation
                div(
                  class = "col-md-6",
                  style = "margin-bottom: 1.5rem;",
                  
                  div(
                    style = "padding: 1.5rem; background-color: #F0F9F4; border-left: 3px solid #28A745; border-radius: 0.375rem; height: 100%;",
                    h3(
                      style = "color: #155724; font-size: 1.2rem; margin-bottom: 0.75rem;",
                      icon("book-reader", style = "margin-right: 0.5rem;"),
                      "Amélioration de l'interprétation"
                    ),
                    p(
                      style = "color: #2C3E50; line-height: 1.8; margin: 0;",
                      tags$ul(
                        style = "padding-left: 1.5rem;",
                        tags$li("Contexte clinique et biologique détaillé"),
                        tags$li("Comparaison avec la littérature existante"),
                        tags$li("Interprétation des résultats en termes cliniques"),
                        tags$li("Recommandations pratiques basées sur les résultats"),
                        tags$li("Documentation des limitations pour chaque analyse")
                      )
                    )
                  )
                )
              ),
              
              # Note finale
              div(
                style = "background-color: #E7F3FF; border-left: 4px solid #17A2B8; padding: 1.5rem; border-radius: 0.375rem; margin-top: 1.5rem;",
                p(
                  style = "color: #0C5460; margin: 0; font-size: 1rem; line-height: 1.8;",
                  icon("rocket", style = "margin-right: 0.5rem;"),
                  tags$strong("Perspective :"),
                  " Ces améliorations permettraient de renforcer la validité,",
                  "la robustesse et l'applicabilité clinique des résultats.",
                  "Elles représentent des axes de développement prioritaires",
                  "pour les futures itérations de ce projet de recherche."
                )
              )
            )
          )
        )
      )
    ),
    
    # ==========================================================================
    # CONCLUSION
    # ==========================================================================
    div(
      class = "container-fluid",
      style = "margin-bottom: 3rem; padding: 2rem 0; background-color: #F8F9FA; border-top: 2px solid #E9ECEF;",
      
      div(
        class = "row justify-content-center",
        div(
          class = "col-lg-10 col-xl-9 text-center",
          
          h3(
            style = "color: #1E5A8E; margin-bottom: 1rem; font-size: 1.5rem;",
            "Conclusion"
          ),
          
          p(
            style = "color: #2C3E50; font-size: 1.1rem; line-height: 1.8; max-width: 800px; margin: 0 auto;",
            "Ce dashboard représente une étape importante dans l'exploration",
            "des relations entre facteurs génétiques, comportementaux et marqueurs",
            "du cancer du sein. Cependant, il est essentiel de reconnaître",
            "ses limitations et de les communiquer de manière transparente.",
            br(), br(),
            "Les résultats présentés doivent être interprétés avec prudence,",
            "en tenant compte des biais potentiels et de l'absence de",
            "démonstration de causalité. Ils constituent des pistes de recherche",
            "plutôt que des conclusions définitives.",
            br(), br(),
            "Les améliorations futures proposées permettront de renforcer",
            "la robustesse et la validité scientifique de ce travail."
          )
        )
      )
    )
  )
}
