# ==============================================================================
# THÈME GGPLOT2 - Style cohérent avec le thème Shiny
# ==============================================================================
# 
# Ce fichier définit le thème ggplot2 personnalisé pour assurer une
# cohérence visuelle entre les graphiques et l'interface Shiny.
# Style : académique, sobre, professionnel
#
# ==============================================================================

# Ici, je charge le fichier theme_shiny.R en premier car il définit
# la fonction get_theme_colors() qui est utilisée dans ce fichier
source("R/theme_shiny.R")

#' Obtenir le thème ggplot2 personnalisé pour les graphiques
#'
#' @param base_size Taille de base de la police (par défaut 12)
#' @param base_family Famille de police (par défaut "Lato")
#' @return Un thème ggplot2 configuré
#' @export
theme_academique <- function(base_size = 12, base_family = "Lato") {
  # Charger les couleurs du thème
  colors <- get_theme_colors()
  
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Fond
      plot.background = ggplot2::element_rect(fill = colors$bg_white, color = NA),
      panel.background = ggplot2::element_rect(fill = colors$bg_white, color = NA),
      panel.grid.major = ggplot2::element_line(color = colors$bg_medium, size = 0.3),
      panel.grid.minor = ggplot2::element_line(color = colors$bg_light, size = 0.2),
      
      # Bordures
      panel.border = ggplot2::element_rect(fill = NA, color = colors$bg_medium, size = 0.5),
      axis.line = ggplot2::element_line(color = colors$text_primary, size = 0.5),
      
      # Texte
      text = ggplot2::element_text(color = colors$text_primary, family = base_family),
      plot.title = ggplot2::element_text(
        color = colors$text_primary,
        size = base_size + 4,
        face = "bold",
        hjust = 0,
        margin = ggplot2::margin(b = 10)
      ),
      plot.subtitle = ggplot2::element_text(
        color = colors$text_secondary,
        size = base_size + 1,
        hjust = 0,
        margin = ggplot2::margin(b = 15)
      ),
      plot.caption = ggplot2::element_text(
        color = colors$text_muted,
        size = base_size - 2,
        hjust = 1,
        margin = ggplot2::margin(t = 10)
      ),
      
      # Axes
      axis.title = ggplot2::element_text(
        color = colors$text_primary,
        size = base_size,
        face = "plain"
      ),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
      axis.text = ggplot2::element_text(
        color = colors$text_secondary,
        size = base_size - 1
      ),
      axis.ticks = ggplot2::element_line(color = colors$text_primary, size = 0.5),
      
      # Légende
      legend.title = ggplot2::element_text(
        color = colors$text_primary,
        size = base_size,
        face = "bold"
      ),
      legend.text = ggplot2::element_text(
        color = colors$text_secondary,
        size = base_size - 1
      ),
      legend.background = ggplot2::element_rect(
        fill = colors$bg_white,
        color = colors$bg_medium,
        size = 0.5
      ),
      legend.key = ggplot2::element_rect(fill = colors$bg_white, color = NA),
      legend.position = "bottom",
      legend.box = "horizontal",
      
      # Facettes (si utilisées)
      strip.background = ggplot2::element_rect(
        fill = colors$bg_light,
        color = colors$bg_medium,
        size = 0.5
      ),
      strip.text = ggplot2::element_text(
        color = colors$text_primary,
        size = base_size,
        face = "bold"
      ),
      
      # Marges
      plot.margin = ggplot2::margin(15, 15, 15, 15)
    )
}

#' Obtenir la palette de couleurs pour les graphiques
#'
#' @param n Nombre de couleurs souhaitées
#' @return Un vecteur de couleurs hexadécimales
#' @export
get_chart_palette <- function(n = 5) {
  colors <- get_theme_colors()
  
  # Palette par défaut (bleu/gris académique)
  default_palette <- c(
    colors$chart_blue,
    colors$chart_blue_light,
    colors$chart_gray,
    colors$chart_gray_light,
    colors$chart_accent
  )
  
  # Si plus de couleurs nécessaires, utiliser une palette étendue
  if (n <= length(default_palette)) {
    return(default_palette[1:n])
  } else {
    # Palette étendue avec variations de bleu et gris
    extended_palette <- c(
      colors$chart_blue,
      colors$chart_blue_light,
      colors$chart_accent,
      colors$chart_gray,
      colors$chart_gray_light,
      colors$primary_dark,
      colors$secondary_dark,
      colors$info
    )
    
    # Répéter ou interpoler si nécessaire
    if (n <= length(extended_palette)) {
      return(extended_palette[1:n])
    } else {
      # Utiliser une palette continue si beaucoup de couleurs nécessaires
      return(grDevices::colorRampPalette(c(colors$chart_blue, colors$chart_gray))(n))
    }
  }
}

#' Appliquer le thème académique par défaut à tous les graphiques ggplot2
#'
#' Cette fonction peut être appelée dans global.R pour appliquer le thème
#' à tous les graphiques de l'application.
#'
#' @export
set_default_ggplot_theme <- function() {
  ggplot2::theme_set(theme_academique())
}
