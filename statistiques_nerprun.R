source("classe_outils.R")
library(DT)

##Description
## Script a pour but de visualiser le nombre de présences et absences dans le jeu des données
## ainsi que la visualisation spatiale

## ---------------------------------------------------------------------------
## Initialisation et lecuture des données

outils <- Outils$new()
rm(Outils)
cheminNerprun <- "O:/Teluq/donnees/nerprun_presence_absence_Mai2026.gpkg"
nerprun <- terra::vect(cheminNerprun)

total <- length(nerprun)
totalPresence <- tidyterra::filter(nerprun, nerprun == "presence") |>
    length()

totalAbsence <- tidyterra::filter(nerprun, nerprun == "absence") |>
    length()


## ----------------------------------------------------------------------------
## Visualisation des données sur la carte
terra::plet(nerprun, "nerprun")


## ------------------------------------------------------------------------------
## probabilités

probabilitePresence <- totalPresence / (totalPresence + total)
probatiliteAbsence <- totalAbsence / (totalAbsence + total)

## ---------------------------------------------------------------------------
##  Création de la table de fréquence et le pourcentage pour chaque groupe: présence ou absence

tab <- as.data.frame(xtabs(~nerprun, data = nerprun)) |>
    dplyr::mutate(
        Pourcentage = round(100 * Freq / sum(Freq), 1),
        Probabilite = round(Freq / sum(Freq), 3)
    )

DT::datatable(
    tab,
    colnames = c("Classe", "Nombre", "%", "Probatilité"),
    rownames = FALSE
)


##-------------------------------------------------------------------------------
## Statistiques TPI

