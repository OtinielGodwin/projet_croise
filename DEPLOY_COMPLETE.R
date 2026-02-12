# ==============================================================================
# COPIEZ ET COLLEZ TOUT CE SCRIPT DANS LA CONSOLE R DE RSTUDIO
# ==============================================================================

# Charger rsconnect
library(rsconnect)

# Configurer le compte
rsconnect::setAccountInfo(
  name = 'chairmanaurel',
  token = '5336AC7A7F14DD44D7DE608E01E42623',
  secret = 'rbdQxCTAuI9KtE2+KIhs14spXjxRgB9mxWw3pTFQ'
)

# Vérifier la configuration
cat("Comptes configurés :\n")
print(accounts())

# Vérifier les fichiers
cat("\nRépertoire actuel :", getwd(), "\n")
cat("app.R existe :", file.exists("app.R"), "\n")
cat("db.csv existe :", file.exists("db.csv"), "\n\n")

# Déployer l'application
cat("Déploiement en cours...\n")
rsconnect::deployApp(
  appDir = getwd(),
  appName = "dashboard-projet-croise",
  account = "chairmanaurel",
  server = "shinyapps.io",
  launch.browser = TRUE,
  forceUpdate = TRUE
)

cat("\n✅ Déploiement terminé !\n")
