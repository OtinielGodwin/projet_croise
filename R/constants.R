# ==============================================================================
# CONSTANTES - Noms de Colonnes et Configuration
# ==============================================================================
# 
# Ce fichier définit les constantes utilisées dans toute l'application.
# Les noms de colonnes sont standardisés pour faciliter la maintenance.
#
# ==============================================================================

# Noms de colonnes standardisés pour les données
# Ces constantes permettent de référencer les colonnes de manière cohérente

# Colonnes d'identification
COL_ID_GLOBAL <- "id_global"
COL_ID <- "id"
COL_PATIENT_ID <- "patient_id"
COL_GENE_ID <- "gene_id"

# Colonnes géographiques
COL_PAYS <- "pays"
COL_COUNTRY <- "country"
COL_NOM_PAYS <- "nom_pays"
COL_PAYS_CODE <- "pays_code"

# Colonnes temporelles
COL_ANNEE <- "annee"
COL_YEAR <- "year"
COL_TRIMESTRE <- "trimestre"
COL_DATE <- "date"
COL_PERIOD <- "period"

# Colonnes de séquences ADN
COL_SEQUENCE_ADN <- "sequence_adn"
COL_SEQUENCE <- "sequence"
COL_ADN <- "adn"
COL_DNA <- "dna"
COL_SEQ <- "seq"

# Colonnes comportementales
COL_SMOKE <- "smoke"
COL_ALCOHOL <- "alcohol"
COL_CSP <- "csp"

# Colonnes biomédicales
COL_CHARGE_VIRALE <- "charge_virale"
COL_CD4 <- "CD4"
COL_ANTICORPS <- "anticorps"

# Colonnes marqueurs du cancer
COL_CA_15_3 <- "CA_15_3"
COL_CA_15_3_APRES <- "CA_15_3_apres"

# Liste des colonnes d'identifiant à toujours conserver lors du filtrage
ID_COLUMNS <- c(
  COL_ID_GLOBAL,
  COL_ID,
  COL_PATIENT_ID,
  COL_GENE_ID,
  COL_PAYS,
  COL_COUNTRY,
  COL_NOM_PAYS,
  COL_ANNEE,
  COL_YEAR,
  COL_TRIMESTRE
)

# Liste des colonnes de pays possibles
COUNTRY_COLUMNS <- c(
  COL_PAYS,
  COL_COUNTRY,
  COL_NOM_PAYS,
  COL_PAYS_CODE
)

# Liste des colonnes de date possibles
DATE_COLUMNS <- c(
  COL_ANNEE,
  COL_YEAR,
  COL_DATE,
  COL_PERIOD,
  COL_TRIMESTRE
)

# Liste des colonnes de séquence ADN possibles
SEQUENCE_COLUMNS <- c(
  COL_SEQUENCE_ADN,
  COL_SEQUENCE,
  COL_ADN,
  COL_DNA,
  COL_SEQ
)
