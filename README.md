# Dashboard Projet Croisé - Analyse des Facteurs Génétiques et Comportementaux

## 📋 Description

Dashboard Shiny professionnel pour l'analyse de l'impact des facteurs génétiques et comportementaux sur les marqueurs du cancer du sein. Le dashboard utilise directement la base de données `db.csv` du projet GitHub.

## 🚀 Installation et Lancement

### Prérequis

- R (version 4.0 ou supérieure)
- RStudio (recommandé)

### Bibliothèques R requises

```r
install.packages(c(
  "shiny",
  "shinyjs",
  "bslib",
  "dplyr",
  "ggplot2",
  "DT",
  "plotly",
  "FactoMineR",
  "factoextra",
  "cluster",
  "glmnet",
  "tidyr"
))
```

### Lancement

1. **Depuis RStudio** :
   - Ouvrir le fichier `app.R`
   - Cliquer sur "Run App"

2. **Depuis la ligne de commande** :
   ```r
   shiny::runApp("app.R")
   ```

L'application se lance automatiquement sur `http://127.0.0.1:3838`

## 📊 Structure du Projet

```
Dashbord Projet Croisé/
├── app.R                          # Application principale
├── db.csv                         # Base de données (depuis GitHub)
├── .Rprofile                      # Configuration R
├── app.yaml                       # Configuration shinyapps.io
├── rsconnect-package.json         # Métadonnées rsconnect
├── R/                             # Modules backend
│   ├── constants.R               # Constantes et noms de colonnes
│   ├── theme_shiny.R             # Thème Shiny (bslib)
│   ├── theme_ggplot.R            # Thème ggplot2
│   ├── utils.R                   # Fonctions utilitaires
│   ├── detect_primers.R          # Détection d'amorces ADN
│   ├── time_series_analysis.R    # Analyse temporelle
│   └── ar_model.R                # Modèle autorégressif
├── modules/                       # Modules Shiny d'analyse
│   ├── mod_stats_descriptives.R  # Statistiques descriptives
│   ├── mod_correlations.R        # Analyse des corrélations
│   ├── mod_acp.R                 # Analyse en Composantes Principales
│   ├── mod_afc.R                 # Analyse Factorielle des Correspondances
│   ├── mod_famd.R                # Factor Analysis of Mixed Data
│   ├── mod_clustering.R          # Clustering non supervisé
│   ├── mod_regression.R          # Modèles supervisés (régression)
│   ├── mod_amorces_adn.R         # Détection d'amorces ADN
│   ├── mod_temporal.R            # Analyse temporelle
│   └── mod_ar_prediction.R       # Prédiction AR(1)
├── ui/                            # Pages UI
│   ├── page_accueil.R            # Page d'accueil
│   └── page_limites_perspectives.R # Page limites et perspectives
└── www/                           # Ressources statiques
    └── css/
        └── custom.css             # Styles personnalisés
```

## 🎯 Fonctionnalités

### Modules d'Analyse Disponibles

1. **Accueil** - Vue d'ensemble du projet avec indicateurs clés
2. **Statistiques Descriptives** - Moyennes, médianes, distributions, boxplots
3. **Corrélations** - Matrices Pearson/Spearman avec heatmaps interactives
4. **ACP** - Analyse en Composantes Principales avec visualisations
5. **AFC** - Analyse Factorielle des Correspondances pour variables qualitatives
6. **FAMD** - Factor Analysis of Mixed Data (variables mixtes)
7. **Clustering** - K-means et clustering hiérarchique avec visualisation ACP
8. **Régression** - Modèles supervisés (régression linéaire, LASSO, Ridge)
9. **Amorces ADN** - Détection d'amorces génétiques avec tolérance aux erreurs
10. **Analyse Temporelle** - Tendances et évolution temporelle par trimestre
11. **Prédiction AR** - Modèle autorégressif AR(1) pour prédire les cas trimestriels

### Données

Le dashboard utilise directement `db.csv` qui contient :
- **Identifiants** : `id_global` - Identifiant unique
- **Géographie** : `nom_pays`, `pays` - Informations géographiques
- **Temporel** : `annee`, `trimestre` - Informations temporelles
- **Génétique** : `sequence_adn` - Séquences ADN pour détection d'amorces
- **Comportemental** : `smoke`, `alcohol`, `csp` - Facteurs comportementaux
- **Biomarqueurs** : `charge_virale`, `CD4`, `anticorps` - Biomarqueurs VIH
- **Cancer** : `CA_15_3`, `CA_15_3_apres` - Marqueurs du cancer du sein

## ⚡ Optimisations de Performance

- **Graphiques Plotly optimisés** : Configuration réduite pour un affichage rapide
- **Isolation des réactifs** : Utilisation de `isolate()` pour éviter les recalculs inutiles
- **Chargement des données** : Données chargées une seule fois au démarrage
- **Gestion d'erreur robuste** : Tous les modules gèrent les erreurs gracieusement

## 🌐 Déploiement sur shinyapps.io

### Configuration

1. Installer `rsconnect` :
   ```r
   install.packages("rsconnect")
   ```

2. Se connecter à shinyapps.io :
   ```r
   library(rsconnect)
   rsconnect::setAccountInfo(
     name = "votre-compte",
     token = "votre-token",
     secret = "votre-secret"
   )
   ```

3. Déployer l'application :
   ```r
   rsconnect::deployApp(
     appDir = ".",
     appName = "dashboard-projet-croise",
     account = "votre-compte"
   )
   ```

Voir `DEPLOIEMENT_SHINYAPPS.md` pour plus de détails.

## 📝 Documentation

- **README.md** - Ce fichier
- **CHANGELOG.md** - Historique des versions
- **DEPLOIEMENT_SHINYAPPS.md** - Guide de déploiement détaillé
- **INSTRUCTIONS_GIT.md** - Instructions pour Git/GitHub

## 🔧 Développement

### Structure Modulaire

L'application utilise une architecture modulaire Shiny avec :
- **Modules UI/Server** : Chaque module d'analyse est indépendant
- **Fonctions utilitaires** : Logique métier dans `R/`
- **Thèmes centralisés** : Styles cohérents dans `R/theme_*.R`

### Ajout d'un Nouveau Module

1. Créer `modules/mod_nouveau_module.R` avec `mod_nouveau_module_ui()` et `mod_nouveau_module_server()`
2. Charger le module dans `app.R`
3. Ajouter le lien de navigation dans `app.R`

## 📄 Licence

Ce projet fait partie du projet académique "Projet Croisé" - IUT d'Aurillac.

## 👥 Auteurs

Équipe du projet Cancer-DNASeq - ONG AidsSupport

## 🔗 Liens

- **GitHub** : https://github.com/OtinielGodwin/projet_croise
- **Dashboard en ligne** : (à configurer après déploiement)
