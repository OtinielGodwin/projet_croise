# Redéploiement rapide après correction
library(rsconnect)

rsconnect::deployApp(
  appDir = getwd(),
  appName = "dashboard-projet-croise",
  account = "chairmanaurel",
  server = "shinyapps.io",
  launch.browser = TRUE,
  forceUpdate = TRUE
)

cat("\n✅ Redéploiement terminé !\n")
