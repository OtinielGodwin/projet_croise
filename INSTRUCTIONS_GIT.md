# Instructions pour Git et GitHub

## 🔧 Initialisation du Dépôt Git

Si le projet n'est pas encore un dépôt Git, exécutez :

```bash
git init
git add .
git commit -m "Initial commit: Dashboard Projet Croisé avec db.csv"
```

## 🔗 Connexion au Dépôt GitHub

### Si le dépôt existe déjà sur GitHub :

```bash
git remote add origin https://github.com/OtinielGodwin/projet_croise.git
git branch -M main
git push -u origin main
```

### Si vous créez un nouveau dépôt :

1. Créer un nouveau dépôt sur GitHub (sans README, sans .gitignore)
2. Exécuter :
   ```bash
   git remote add origin https://github.com/OtinielGodwin/projet_croise.git
   git branch -M main
   git push -u origin main
   ```

## 📤 Push des Modifications

Après avoir fait des modifications :

```bash
# Vérifier les fichiers modifiés
git status

# Ajouter les fichiers modifiés
git add .

# Créer un commit
git commit -m "Description des modifications"

# Pousser sur GitHub
git push origin main
```

## 📥 Pull des Modifications

Pour récupérer les dernières modifications depuis GitHub :

```bash
git pull origin main
```

## 🔄 Workflow Recommandé

1. **Avant de commencer** : `git pull origin main`
2. **Faire vos modifications**
3. **Vérifier** : `git status`
4. **Ajouter** : `git add .`
5. **Commit** : `git commit -m "Description"`
6. **Push** : `git push origin main`

## ⚠️ Notes Importantes

- **db.csv** : Assurez-vous que `db.csv` est à jour avec la version du GitHub
- **Fichiers sensibles** : Ne commitez pas de fichiers contenant des informations sensibles
- **.gitignore** : Le fichier `.gitignore` exclut déjà les fichiers temporaires et les données sensibles
