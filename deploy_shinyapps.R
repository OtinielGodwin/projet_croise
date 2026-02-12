# ==============================================================================
# SCRIPT DE DÉPLOIEMENT SUR SHINYAPPS.IO
# ==============================================================================
# 
# Ce script permet de configurer et déployer l'application sur shinyapps.io
# 
# INSTRUCTIONS :
# 1. Obtenez vos identifiants sur https://www.shinyapps.io/ :
#    - Allez dans Account > Tokens
#    - Cliquez sur "Show" pour révéler votre token et secret
# 
# 2. Exécutez ce script dans RStudio ou la console R :
#    source("deploy_shinyapps.R")
#
# 3. Suivez les instructions à l'écran
#
# ==============================================================================

# Installer rsconnect si nécessaire
if (!require("rsconnect", quietly = TRUE)) {
  cat("Installation de rsconnect...\n")
  install.packages("rsconnect")
}

library(rsconnect)

# ==============================================================================
# CONFIGURATION DU COMPTE
# ==============================================================================

cat("\n")
cat("========================================\n")
cat("CONFIGURATION DU COMPTE SHINYAPPS.IO\n")
cat("========================================\n\n")

# Demander les informations du compte
cat("Veuillez entrer vos identifiants shinyapps.io :\n\n")

account_name <- readline(prompt = "Nom de votre compte shinyapps.io : ")
token <- readline(prompt = "Token (copiez depuis Account > Tokens) : ")
secret <- readline(prompt = "Secret (copiez depuis Account > Tokens) : ")

# Configurer le compte
cat("\nConfiguration du compte...\n")
tryCatch({
  rsconnect::setAccountInfo(
    name = account_name,
    token = token,
    secret = secret
  )
  cat("✅ Compte configuré avec succès !\n\n")
}, error = function(e) {
  cat("❌ Erreur lors de la configuration du compte :", e$message, "\n")
  stop("Impossible de configurer le compte. Vérifiez vos identifiants.")
})

# ==============================================================================
# DÉPLOIEMENT DE L'APPLICATION
# ==============================================================================

cat("\n")
cat("========================================\n")
cat("DÉPLOIEMENT DE L'APPLICATION\n")
cat("========================================\n\n")

# Nom de l'application
app_name <- "dashboard-projet-croise"

# Vérifier que les fichiers nécessaires existent
required_files <- c("app.R", "db.csv")
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  cat("❌ Fichiers manquants :", paste(missing_files, collapse = ", "), "\n")
  stop("Veuillez vous assurer que tous les fichiers nécessaires sont présents.")
}

cat("Fichiers requis trouvés ✅\n")
cat("Nom de l'application :", app_name, "\n")
cat("Compte :", account_name, "\n\n")

# Demander confirmation
cat("Voulez-vous déployer l'application maintenant ? (o/n) : ")
confirm <- readline()

if (tolower(confirm) != "o" && tolower(confirm) != "oui") {
  cat("Déploiement annulé.\n")
  stop()
}

# Déployer l'application
cat("\nDéploiement en cours...\n")
cat("Cela peut prendre quelques minutes...\n\n")

tryCatch({
  rsconnect::deployApp(
    appDir = ".",
    appName = app_name,
    account = account_name,
    server = "shinyapps.io",
    launch.browser = TRUE
  )
  
  cat("\n")
  cat("========================================\n")
  cat("✅ DÉPLOIEMENT RÉUSSI !\n")
  cat("========================================\n\n")
  cat("Votre application est maintenant disponible sur shinyapps.io\n")
  cat("Vous pouvez la gérer depuis : https://www.shinyapps.io/admin/#/applications\n\n")
  
}, error = function(e) {
  cat("\n")
  cat("========================================\n")
  cat("❌ ERREUR LORS DU DÉPLOIEMENT\n")
  cat("========================================\n\n")
  cat("Message d'erreur :", e$message, "\n\n")
  cat("Vérifiez :\n")
  cat("1. Que tous les fichiers sont présents\n")
  cat("2. Que vos identifiants sont corrects\n")
  cat("3. Les logs dans le dashboard shinyapps.io\n\n")
  stop("Déploiement échoué.")
})
