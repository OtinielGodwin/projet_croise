# 🚀 Instructions de Déploiement sur shinyapps.io

## Étape 1 : Obtenir vos identifiants

1. Connectez-vous à https://www.shinyapps.io/
2. Allez dans **Account** (en haut à droite) > **Tokens**
3. Cliquez sur **Show** pour révéler votre :
   - **Token** : Une longue chaîne de caractères
   - **Secret** : Une autre longue chaîne de caractères
4. Copiez ces deux valeurs (vous en aurez besoin)

## Étape 2 : Déployer l'application

### Option A : Utiliser le script automatique (Recommandé)

1. Ouvrez RStudio dans le projet
2. Exécutez dans la console R :
   ```r
   source("deploy_shinyapps.R")
   ```
3. Suivez les instructions à l'écran :
   - Entrez votre nom de compte shinyapps.io
   - Entrez votre token
   - Entrez votre secret
   - Confirmez le déploiement

### Option B : Déploiement manuel

1. Ouvrez RStudio dans le projet
2. Exécutez dans la console R :
   ```r
   # Installer rsconnect si nécessaire
   install.packages("rsconnect")
   
   # Charger la bibliothèque
   library(rsconnect)
   
   # Configurer votre compte (remplacez par vos valeurs)
   rsconnect::setAccountInfo(
     name = "VOTRE_NOM_COMPTE",
     token = "VOTRE_TOKEN",
     secret = "VOTRE_SECRET"
   )
   
   # Déployer l'application
   rsconnect::deployApp(
     appDir = ".",
     appName = "dashboard-projet-croise",
     account = "VOTRE_NOM_COMPTE",
     server = "shinyapps.io"
   )
   ```

## Étape 3 : Vérifier le déploiement

Une fois le déploiement terminé :

1. ✅ Vérifiez que l'application démarre sans erreur
2. ✅ Testez la navigation entre les pages
3. ✅ Vérifiez que les données `db.csv` se chargent correctement
4. ✅ Testez quelques analyses (statistiques, graphiques, etc.)

## 🔄 Mettre à jour l'application

Pour mettre à jour l'application après des modifications :

```r
library(rsconnect)

rsconnect::deployApp(
  appDir = ".",
  appName = "dashboard-projet-croise",
  account = "VOTRE_NOM_COMPTE"
)
```

## 🐛 Résolution de problèmes

### Erreur : "Application failed to start"
- Vérifiez les logs dans le dashboard shinyapps.io (Application > Logs)
- Vérifiez que toutes les dépendances sont installées
- Vérifiez que `db.csv` est bien présent dans le projet

### Erreur : "Out of memory"
- Le compte gratuit a une limite de 512 MB
- Optimisez le code ou passez à un compte payant

### Erreur : "Invalid account credentials"
- Vérifiez que votre token et secret sont corrects
- Régénérez un nouveau token si nécessaire

## 📊 Gérer votre application

Une fois déployée, vous pouvez gérer votre application depuis :
- **Dashboard** : https://www.shinyapps.io/admin/#/applications
- **Logs** : Voir les erreurs et messages de débogage
- **Metrics** : Voir les statistiques d'utilisation
- **Settings** : Modifier la configuration (mémoire, instances, etc.)

## 💡 Conseils

1. **Testez localement** avant de déployer
2. **Vérifiez les logs** en cas d'erreur
3. **Gardez vos identifiants secrets** (ne les commitez pas dans Git)
4. **Utilisez le script `deploy_shinyapps.R`** pour un déploiement facile
