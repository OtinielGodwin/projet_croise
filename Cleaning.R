if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")

library(tidyverse)
library(lubridate)

# 1. Configuration des dossiers
folders <- list(
  sequences = "data_sequences",
  social = "data_social",
  biomedical = "data_biomedical",
  cancer = "data_cancer"
)

# 2. Fonctions de lecture robustes
process_seq <- function(path) {
  lines <- read_lines(path)
  if (length(lines) == 0) return(NULL)
  id_indices <- which(str_starts(lines, ">"))
  map_df(seq_along(id_indices), function(i) {
    start <- id_indices[i]
    end <- if (i < length(id_indices)) id_indices[i+1] - 1 else length(lines)
    id <- str_remove(lines[start], ">") %>% str_trim()
    seq <- paste(lines[(start+1):end], collapse = "") %>% str_trim()
    tibble(id_global = id, sequence_adn = seq)
  })
}

process_tab <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    rename(id_global = 1) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(id_global = str_trim(id_global))
}

# 3. Chargement des données brutes
seq_df <- map_df(list.files(folders$sequences, recursive = TRUE, full.names = TRUE), ~tryCatch(process_seq(.x), error = function(e) NULL)) %>% distinct(id_global, .keep_all = TRUE)
soc_df <- map_df(list.files(folders$social, recursive = TRUE, full.names = TRUE), ~tryCatch(process_tab(.x), error = function(e) NULL)) %>% distinct(id_global, .keep_all = TRUE)
bio_df <- map_df(list.files(folders$biomedical, recursive = TRUE, full.names = TRUE), ~tryCatch(process_tab(.x), error = function(e) NULL)) %>% distinct(id_global, .keep_all = TRUE)
can_df <- map_df(list.files(folders$cancer, recursive = TRUE, full.names = TRUE), ~tryCatch(process_tab(.x), error = function(e) NULL)) %>% distinct(id_global, .keep_all = TRUE)

# 4. Chargement du référentiel pays
# On nettoie les espaces qui peuvent exister dans le CSV (ex: "AL " ou " AL")
pays_ref <- read_csv("code_pays.csv", show_col_types = FALSE) %>%
  rename(nom_pays = 1, pays = 2) %>%
  mutate(pays = str_trim(pays), nom_pays = str_trim(nom_pays))

# 5. Fusion et enrichissement
final_db <- seq_df %>%
  full_join(soc_df, by = "id_global") %>%
  full_join(bio_df, by = "id_global") %>%
  full_join(can_df, by = "id_global") %>%
  mutate(
    pays = str_extract(id_global, "[A-Z]{2}$"),
    date_raw = str_extract(id_global, "\\d{1,2}\\.\\d{1,2}\\.\\d{4}"),
    date_temp = dmy(date_raw),
    annee = year(date_temp),
    trimestre = paste0("Q", quarter(date_temp))
  ) %>%
  # On ajoute le nom complet du pays
  left_join(pays_ref, by = "pays") %>%
  # On organise pour que le nom du pays soit bien visible au début
  select(id_global, nom_pays, pays, annee, trimestre, sequence_adn, everything(), -date_temp, -date_raw)

# 6. Exportation finale
write_csv(final_db, "db.csv")