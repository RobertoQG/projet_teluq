
source("classe_outils.R")
library(DT)

##---------------------------------------------------------------------------
## Initialisation et lecuture des données

outils <- Outils$new(); rm(Outils)
cheminNerprun <- "O:/Teluq/donnees/nerprun_presence_absence_Mai2026.gpkg"
nerprun <- terra::vect(cheminNerprun)

total <- length(nerprun)

##---------------------------------------------------------------------------
##  Création de la table de fréquence et le pourcentage pour chaque groupe: présence ou absence

tab <- as.data.frame(xtabs(~ nerprun, data = nerprun)) |>
  dplyr::mutate(
    Pourcentage = round(100 * Freq / sum(Freq), 1)
  )

DT::datatable(
  tab,
  colnames = c("Classe", "Nombre", "%"),
  rownames = FALSE
)

##----------------------------------------------------------------------------
## Visualisation des données sur la carte

terra::plet(nerprun, "nerprun")
