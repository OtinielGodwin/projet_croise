# ==============================================================================
# SCRIPT SIMPLIFIÉ DE DÉPLOIEMENT - COPIEZ-COLLEZ TOUT DANS RSTUDIO
# ==============================================================================

# Étape 1 : Installer et charger rsconnect
if (!require("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cran.rstudio.com/")
}
library(rsconnect)

# Étape 2 : Configurer le compte
cat("Configuration du compte chairmanaurel...\n")
rsconnect::setAccountInfo(
  name = 'chairmanaurel',
  token = '5336AC7A7F14DD44D7DE608E01E42623',
  secret = 'rbdQxCTAuI9KtE2+KIhs14spXjxRgB9mxWw3pTFQ'
)
cat("✅ Compte configuré !\n\n")

# Étape 3 : Vérifier la configuration
cat("Vérification des comptes configurés :\n")
print(accounts())
cat("\n")

# Étape 4 : Vérifier les fichiers
cat("Vérification des fichiers...\n")
cat("Répertoire actuel :", getwd(), "\n")
cat("app.R existe :", file.exists("app.R"), "\n")
cat("db.csv existe :", file.exists("db.csv"), "\n\n")

# Étape 5 : Déployer
cat("========================================\n")
cat("DÉPLOIEMENT EN COURS...\n")
cat("========================================\n\n")

rsconnect::deployApp(
  appDir = getwd(),
  appName = "dashboard-projet-croise",
  account = "chairmanaurel",
  server = "shinyapps.io",
  launch.browser = TRUE,
  forceUpdate = TRUE
)

cat("\n✅ DÉPLOIEMENT TERMINÉ !\n")
cat("URL : https://chairmanaurel.shinyapps.io/dashboard-projet-croise/\n")
