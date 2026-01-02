library(tidyverse)

# ===== 1. CHARGEMENT DES DONNÉES =====
# Remplacez par vos chemins de fichiers
# BPE_IDF_2023 <- read_csv("chemin/vers/BPE_IDF_2023.csv")
# BPE_Test_2016 <- read_csv("chemin/vers/BPE_Test_2016.csv")

# ===== 2. HARMONISATION DES COLONNES =====

# Harmoniser BPE 2023
BPE_2023_harmonise <- BPE_IDF_2023 %>%
  select(
    annee = Millésime,
    code_iris = `Code iris`,
    code_commune = `Code département et commune`,
    departement = Département,
    region = Région,
    type_equipement = Type,
    domaine = Domaine,
    sous_domaine = `Sous-domaine`,
    longitude = Longitude,
    latitude = Latitude,
    nom_commune = Commune
  ) %>%
  mutate(
    annee = 2023,
    code_iris = toupper(trimws(code_iris)),
    code_commune = trimws(code_commune),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  ) %>%
  filter(!is.na(code_iris))

# Harmoniser BPE 2016
BPE_2016_harmonise <- BPE_Test_2016 %>%
  mutate(
    # Extraire latitude et longitude depuis Geo Point
    # Format: "latitude, longitude" (ex: "48.887051437, 2.38482208196")
    latitude = as.numeric(trimws(sub(",.*", "", `Geo Point`))),
    longitude = as.numeric(trimws(sub(".*,", "", `Geo Point`)))
  ) %>%
  select(
    annee = an,
    code_iris = dciris,
    code_commune = depcom,
    departement = dep,
    region = reg,
    type_equipement = Équipement,
    domaine = Domaine,
    sous_domaine = `Sous-domaine`,
    latitude,
    longitude,
    nom_commune = `Nom Officiel Commune`
  ) %>%
  mutate(
    annee = 2016,
    code_iris = toupper(trimws(code_iris)),
    code_commune = trimws(code_commune)
  ) %>%
  filter(!is.na(code_iris))

# ===== 3. HARMONISATION DES NOMENCLATURES D'ÉQUIPEMENTS =====

# Vérifier les différences de nomenclature
cat("=== Types d'équipements 2023 (premiers 50) ===\n")
print(head(sort(unique(BPE_2023_harmonise$type_equipement)), 50))

cat("\n=== Types d'équipements 2016 (premiers 50) ===\n")
print(head(sort(unique(BPE_2016_harmonise$type_equipement)), 50))

# Table de correspondance si nomenclature a changé
# À adapter selon vos données après avoir vérifié les différences
# correspondance_equipements <- tibble(
#   code_ancien = c("A101", "A102", "A103"),  # Codes 2016
#   code_nouveau = c("A101", "A102", "A103"), # Codes 2023
#   libelle = c("Hypermarché", "Supermarché", "Supérette")
# )

# Appliquer si nécessaire (décommenter et adapter)
# BPE_2016_harmonise <- BPE_2016_harmonise %>%
#   left_join(correspondance_equipements, by = c("type_equipement" = "code_ancien")) %>%
#   mutate(type_equipement = coalesce(code_nouveau, type_equipement))

# ===== 4. GESTION DES ÉVOLUTIONS D'IRIS =====

# Pour identifier les IRIS qui ont changé entre 2016 et 2023
iris_2016 <- unique(BPE_2016_harmonise$code_iris)
iris_2023 <- unique(BPE_2023_harmonise$code_iris)

cat("\n=== Diagnostic IRIS ===\n")
cat("IRIS présents en 2016 :", length(iris_2016), "\n")
cat("IRIS présents en 2023 :", length(iris_2023), "\n")
cat("IRIS disparus :", length(setdiff(iris_2016, iris_2023)), "\n")
cat("IRIS nouveaux :", length(setdiff(iris_2023, iris_2016)), "\n")
cat("IRIS communs :", length(intersect(iris_2016, iris_2023)), "\n")

# ===== 5. AGRÉGATION PAR IRIS ET ANNÉE =====

# Compter les équipements par IRIS et type
BPE_2016_agregee <- BPE_2016_harmonise %>%
  group_by(annee, code_iris, code_commune, type_equipement, domaine) %>%
  summarise(
    nb_equipements = n(),
    .groups = "drop"
  )

BPE_2023_agregee <- BPE_2023_harmonise %>%
  group_by(annee, code_iris, code_commune, type_equipement, domaine) %>%
  summarise(
    nb_equipements = n(),
    .groups = "drop"
  )

# ===== 6. COMBINAISON DES DEUX MILLÉSIMES =====

BPE_combinee <- bind_rows(
  BPE_2016_agregee,
  BPE_2023_agregee
)

# ===== 7. CRÉATION D'UNE BASE LARGE POUR ANALYSE =====

# Format large : une ligne par IRIS avec nb équipements 2016 et 2023
BPE_evolution <- BPE_combinee %>%
  pivot_wider(
    names_from = annee,
    values_from = nb_equipements,
    names_prefix = "nb_",
    values_fill = 0
  ) %>%
  mutate(
    evolution_absolue = nb_2023 - nb_2016,
    evolution_relative = ifelse(nb_2016 > 0,
                                (nb_2023 - nb_2016) / nb_2016 * 100,
                                NA)
  )

# ===== 8. FILTRAGE ZONE PARISIENNE =====

# Départements Île-de-France
depts_idf <- c("75", "77", "78", "91", "92", "93", "94", "95")

BPE_IDF <- BPE_combinee %>%
  filter(substr(code_commune, 1, 2) %in% depts_idf)

# ===== 9. EXPORT DES BASES HARMONISÉES =====

# write_csv(BPE_2016_harmonise, "BPE_2016_harmonise.csv")
# write_csv(BPE_2023_harmonise, "BPE_2023_harmonise.csv")
# write_csv(BPE_combinee, "BPE_combinee_2016_2023.csv")
# write_csv(BPE_evolution, "BPE_evolution_2016_2023.csv")

# ===== 10. STATISTIQUES DESCRIPTIVES =====

cat("\n=== Résumé de la base combinée ===\n")
print(summary(BPE_combinee))

cat("\n=== Nombre d'équipements par domaine et année ===\n")
BPE_combinee %>%
  group_by(annee, domaine) %>%
  summarise(total_equipements = sum(nb_equipements), .groups = "drop") %>%
  arrange(domaine, annee) %>%
  print()

cat("\n=== Nombre total d'IRIS dans la base combinée ===\n")
cat("Total IRIS uniques :", length(unique(BPE_combinee$code_iris)), "\n")
