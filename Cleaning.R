

library(janitor)
library(dplyr)

df <- read.delim("Valeursfoncieres-2014.txt", sep="|", header=TRUE, stringsAsFactors=FALSE)
names(df)
unique(df$Prefixe.de.section)
table(df$Prefixe.de.section)


df2 <- read.delim("ValeursFoncieres-2022.txt", sep="|", header=TRUE, stringsAsFactors=FALSE)

library(data.table)
dvf <- fread("dvf.csv")
names(dvf)""
unique(format(dvf$date_mutation, "%Y"))
min(dvf$date_mutation)
max(dvf$date_mutation)

BPE_Test <- read_csv(here("DATA", "BPE24.csv"))

ok_columns <- read_delim(here("DATA", "BPE24.csv"),
                         delim = ";")

BPE_IDF <- ok_columns |>
  dplyr::filter(DEP %in% c("75","92","93","94","78","91","95","77"))


Liste <- read_csv("~/Downloads/BPE24_varmod.csv")
Liste <- read_delim(("~/Downloads/BPE24_varmod.csv"),
                    delim = ";")


truc <- read.csv("DATA/DS_BPE_EVOLUTION_2024_data.csv")
truc <- read_delim(("DATA/DS_BPE_EVOLUTION_2024_data.csv"),
                   delim = ";")

Table <- fread("DATA/table_passage_1999_2022.csv")
