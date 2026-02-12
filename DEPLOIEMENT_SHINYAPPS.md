# Guide de Déploiement sur shinyapps.io

## 📋 Prérequis

1. **Compte shinyapps.io** : Créer un compte gratuit sur https://www.shinyapps.io/
2. **R et RStudio** : Installés et configurés
3. **Package rsconnect** : Installé dans R

## 🔧 Configuration Initiale

### 1. Installer rsconnect

```r
install.packages("rsconnect")
```

### 2. Obtenir les identifiants de votre compte

1. Connectez-vous à https://www.shinyapps.io/
2. Allez dans **Account** > **Tokens**
3. Cliquez sur **Show** pour révéler votre token et secret

### 3. Configurer rsconnect

```r
library(rsconnect)

rsconnect::setAccountInfo(
  name = "votre-nom-compte",        # Remplacez par votre nom de compte
  token = "VOTRE_TOKEN",              # Remplacez par votre token
  secret = "VOTRE_SECRET"             # Remplacez par votre secret
)
```

## 🚀 Déploiement

### Méthode 1 : Depuis RStudio

1. Ouvrir le projet dans RStudio
2. Ouvrir le fichier `app.R`
3. Cliquer sur **Publish** dans la barre d'outils (icône bleue)
4. Sélectionner **Publish to shinyapps.io**
5. Choisir le compte et le nom de l'application
6. Cliquer sur **Publish**

### Méthode 2 : Depuis la console R

```r
library(rsconnect)

rsconnect::deployApp(
  appDir = ".",                      # Répertoire de l'application
  appName = "dashboard-projet-croise", # Nom de l'application (modifiable)
  account = "votre-nom-compte",       # Votre nom de compte
  server = "shinyapps.io"
)
```

## 📦 Fichiers de Configuration

Le projet contient déjà les fichiers nécessaires :

- **`app.yaml`** : Configuration de l'application (mémoire, instances, etc.)
- **`rsconnect-package.json`** : Métadonnées du package rsconnect
- **`.Rprofile`** : Configuration R pour le déploiement

### Configuration de la mémoire (app.yaml)

Par défaut, l'application est configurée avec :
- **Mémoire** : 512 MB (gratuit) ou 1 GB (payant)
- **Instances** : 1 instance

Pour modifier, éditez `app.yaml` :

```yaml
name: dashboard-projet-croise
runtime: shiny
memory: 512
instances: 1
```

## ✅ Vérification Post-Déploiement

Après le déploiement, vérifiez que :

1. ✅ L'application démarre sans erreur
2. ✅ Les données `db.csv` sont bien chargées
3. ✅ Tous les modules s'affichent correctement
4. ✅ Les graphiques se chargent rapidement
5. ✅ La navigation fonctionne

## 🔄 Mise à Jour

Pour mettre à jour l'application après des modifications :

```r
library(rsconnect)

rsconnect::deployApp(
  appDir = ".",
  appName = "dashboard-projet-croise",
  account = "votre-nom-compte"
)
```

L'application sera automatiquement mise à jour sur shinyapps.io.

## 🐛 Résolution de Problèmes

### Erreur : "Application failed to start"

- Vérifiez que toutes les dépendances sont listées dans `rsconnect-package.json`
- Vérifiez les logs dans le dashboard shinyapps.io

### Erreur : "Out of memory"

- Augmentez la mémoire dans `app.yaml` (nécessite un compte payant)
- Optimisez le code pour réduire l'utilisation mémoire

### Erreur : "File not found: db.csv"

- Vérifiez que `db.csv` est bien dans le répertoire racine
- Vérifiez que le fichier n'est pas dans `.gitignore`

## 📊 Monitoring

Une fois déployé, vous pouvez :

- **Voir les logs** : Dashboard shinyapps.io > Application > Logs
- **Voir les statistiques** : Dashboard shinyapps.io > Application > Metrics
- **Gérer les versions** : Dashboard shinyapps.io > Application > Versions

## 💡 Conseils

1. **Testez localement** avant de déployer
2. **Vérifiez les logs** en cas d'erreur
3. **Optimisez les graphiques** pour réduire le temps de chargement
4. **Utilisez un compte payant** pour plus de ressources si nécessaire

## 🔗 Ressources

- **Documentation shinyapps.io** : https://docs.rstudio.com/shinyapps.io/
- **Support** : https://support.rstudio.com/hc/en-us
