# ==============================================================================
# THÈME SHINY - Configuration bslib pour un style académique et professionnel
# ==============================================================================
# 
# Ce fichier définit le thème Bootstrap personnalisé pour le dashboard Shiny.
# Style : élégant, moderne, académique
# Couleurs : bleu, gris, blanc (palette sobre et professionnelle)
# Typographie : lisible et adaptée à une présentation universitaire
#
# ==============================================================================

#' Obtenir le thème Shiny personnalisé pour le dashboard
#'
#' @return Un objet thème bslib configuré pour un style académique
#' @export
get_shiny_theme <- function() {
  bslib::bs_theme(
    # Version Bootstrap
    version = 5,
    
    # Palette de couleurs principale
    bg = "#FFFFFF",                    # Fond principal blanc
    fg = "#2C3E50",                    # Texte principal (gris foncé)
    
    # Couleurs primaires (bleu académique)
    primary = "#1E5A8E",               # Bleu foncé principal
    secondary = "#6C757D",             # Gris secondaire
    success = "#28A745",               # Vert (pour indicateurs positifs)
    info = "#17A2B8",                  # Bleu clair (pour informations)
    warning = "#FFC107",               # Jaune (pour avertissements)
    danger = "#DC3545",                # Rouge (pour alertes)
    
    # Couleurs de base
    base_font = bslib::font_google("Lato", wght = c(300, 400, 500, 700)),
    heading_font = bslib::font_google("Lato", wght = c(400, 500, 700)),
    code_font = bslib::font_google("Fira Code", wght = c(400, 500)),
    
    # Espacements
    spacer = "1rem",
    
    # Bordures
    border_radius = "0.375rem",        # Bordures légèrement arrondies
    
    # Options supplémentaires
    "enable-rounded" = TRUE,
    "enable-shadows" = TRUE,
    "enable-gradients" = FALSE,        # Pas de dégradés pour un style sobre
    
    # Couleurs personnalisées pour les composants
    "navbar-bg" = "#1E5A8E",           # Fond de la navbar (bleu foncé)
    "navbar-fg" = "#FFFFFF",           # Texte de la navbar (blanc)
    "sidebar-bg" = "#F8F9FA",          # Fond de la sidebar (gris très clair)
    "sidebar-fg" = "#2C3E50",          # Texte de la sidebar (gris foncé)
    "card-bg" = "#FFFFFF",             # Fond des cartes (blanc)
    "card-border-color" = "#E9ECEF"    # Bordure des cartes (gris clair)
  )
}

#' Obtenir les couleurs de la palette personnalisée
#'
#' @return Une liste nommée avec les couleurs du thème
#' @export
get_theme_colors <- function() {
  list(
    # Couleurs principales
    primary = "#1E5A8E",               # Bleu foncé principal
    primary_light = "#4A7BA7",         # Bleu moyen
    primary_dark = "#0F3D5C",          # Bleu très foncé
    
    # Couleurs secondaires
    secondary = "#6C757D",             # Gris secondaire
    secondary_light = "#ADB5BD",       # Gris clair
    secondary_dark = "#495057",        # Gris foncé
    
    # Couleurs de fond
    bg_white = "#FFFFFF",              # Blanc pur
    bg_light = "#F8F9FA",              # Gris très clair
    bg_medium = "#E9ECEF",             # Gris clair
    bg_dark = "#2C3E50",               # Gris foncé
    
    # Couleurs de texte
    text_primary = "#2C3E50",          # Texte principal
    text_secondary = "#6C757D",        # Texte secondaire
    text_muted = "#ADB5BD",            # Texte atténué
    text_light = "#FFFFFF",            # Texte clair (sur fond sombre)
    
    # Couleurs fonctionnelles
    success = "#28A745",               # Vert
    info = "#17A2B8",                  # Bleu clair
    warning = "#FFC107",               # Jaune
    danger = "#DC3545",                # Rouge
    
    # Couleurs pour graphiques
    chart_blue = "#1E5A8E",            # Bleu pour graphiques
    chart_blue_light = "#4A7BA7",      # Bleu clair pour graphiques
    chart_gray = "#6C757D",            # Gris pour graphiques
    chart_gray_light = "#ADB5BD",      # Gris clair pour graphiques
    chart_accent = "#17A2B8"           # Bleu accent pour graphiques
  )
}
