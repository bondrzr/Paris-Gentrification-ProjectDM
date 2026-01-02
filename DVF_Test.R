library(vroom)
Dvf_test <- vroom("DATA/dvf_14_18.csv")

names(Dvf_test)

library(dplyr)

library(data.table)

idf_deps <- c("75", "92", "93", "94", "91", "77", "78", "95")


dt <- fread(
  "dvf.csv",
  select = c("code_departement", "id_mutation", "date_mutation", "nature_mutation",
             "valeur_fonciere", "type_local", "surface_reelle_bati",
             "latitude", "longitude", "code_commune"),
  colClasses = list(character = "code_departement")
)[code_departement %in% idf_deps]

dvf <- dt[
  !is.na(surface_reelle_bati) &          # supprimer NA
    surface_reelle_bati > 9 &            # supprimer studios erronés / caves
    surface_reelle_bati < 500            # supprimer surfaces irréalistes
]

dvf[, prix_m2 := valeur_fonciere / surface_reelle_bati]


library(data.table)

idf <- c("75","92","93","94","91","77","78","95")

dvf <- fread("dvf.csv")[
  code_departement %in% idf &
    type_local %in% c("Appartement", "Maison") &
    !is.na(surface_reelle_bati) &
    surface_reelle_bati > 9 & surface_reelle_bati < 500 &
    valeur_fonciere > 5000 & valeur_fonciere < 5e6 &
    !is.na(longitude) & !is.na(latitude)
]


library(data.table)


idf <- c("75","92","93","94","91","77","78","95")


cols_keep <- c(
  "id_mutation",
  "date_mutation",
  "nature_mutation",
  "valeur_fonciere",
  "type_local",
  "code_type_local",
  "surface_reelle_bati",
  "nombre_pieces_principales",
  "surface_terrain",
  "code_postal",
  "code_commune",
  "nom_commune",
  "longitude",
  "latitude",
  "id_parcelle",
  "code_departement"
)


dvf <- fread(
  "dvf.csv",
  select = cols_keep
)[
  code_departement %in% idf &
    type_local %in% c("Appartement", "Maison") &
    !is.na(surface_reelle_bati) &
    surface_reelle_bati > 9 & surface_reelle_bati < 500 &
    valeur_fonciere > 5000 & valeur_fonciere < 5e6 &
    !is.na(longitude) & !is.na(latitude)
]


dvf[, prix_m2 := valeur_fonciere / surface_reelle_bati]

dvf2014 <- fread("~/Downloads/valeursfoncieres-2014.txt")
names(dvf2014)

idf_truc <<- fread("~/Downloads/R11_Ile_de_France/r11_mutation.csv")
names(idf_truc)

library(dplyr)
library(stringr)
library(sf)

df <- idf_truc %>%

  # 1. Filtrer ventes
  filter(libnatmut == "Vente") %>%

  # 2. Garder biens résidentiels
  filter(libtypbien %in% c("Maison", "Appartement")) %>%

  # 3. Nettoyage des prix / surfaces
  mutate(
    valeurfonc = as.numeric(valeurfonc),
    sbati      = as.numeric(sbati),
    prix_m2    = valeurfonc / sbati
  ) %>%

  filter(
    valeurfonc > 1000,
    sbati > 10,
    prix_m2 > 100,           # évite les valeurs absurdes
    prix_m2 < 40000
  ) %>%

  # 4. Colonnes utiles uniquement
  select(
    idmutation,
    datemut, anneemut, moismut,
    valeurfonc, prix_m2,
    libtypbien, codtypbien,
    sbati, sbatmai, sbatapt,
    nblocapt, nblocmai,
    coddep,
    l_codinsee,
    l_section, l_idpar,
    everything()
  )
library(dplyr)

idf_truc %>% count(libnatmut)
idf_truc %>% count(libtypbien)
str(idf_truc[, c("valeurfonc", "sbati")])

library(dplyr)

logements_labels <- c(
  "UN APPARTEMENT",
  "DEUX APPARTEMENTS",
  "APPARTEMENT INDETERMINE",
  "UNE MAISON",
  "DES MAISONS",
  "MAISON - INDETERMINEE",
  "BATI MIXTE - LOGEMENTS",          # optionnel
  "BATI MIXTE - LOGEMENT/ACTIVITE"   # optionnel
)

df1 <- idf_truc %>%
  filter(libnatmut == "Vente") %>%
  filter(libtypbien %in% logements_labels)

nrow(df1)
df1 %>% count(libtypbien)

summary(df1$sbati)
table(df1$sbati == 0)

df2 <- df1 %>%
  filter(!is.na(valeurfonc),
         !is.na(sbati),
         valeurfonc > 1000,
         sbati > 0) %>%        # PAS >10 au début
  mutate(prix_m2 = valeurfonc / sbati)

nrow(df2)
summary(df2$prix_m2)

df_idf <- df2 %>%
  filter(coddep %in% c("75","77","78","91","92","93","94","95"))


