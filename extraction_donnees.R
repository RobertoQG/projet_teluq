
source("classe_outils.R")
library(magrittr)


##------------------------------------------------------------------------
## Initialisation
outils  <- Outils$new()

## -------------------------------------------------------------------------
## Lecture des données
dossier    <- "O:/Teluq/donnees"
listeChemins <- outils$lireListeFichierExt(dossier)
cheminNerprun <- "O:/Teluq/donnees/nerprun_presence_absence_Mai2026.gpkg"
nerprun <- terra::vect(cheminNerprun)

monRaster <- purrr::map(listeChemins, terra::rast) %>% terra::rast()

##---------------------------------------------------------------------------
## Extration des données
monExtraction <- terra::extract(monRaster, nerprun, ID = FALSE, bind = TRUE) %>%
  as.data.frame()

monDataFrame <- tidyr::pivot_longer(monExtraction, cols =  - nerprun, names_to = "variables")

library(ggplot2)

ggplot(monDataFrame, aes(x=variables, y=value, fill = nerprun)) + geom_boxplot(outlier.colour="gray", outlier.shape=16,
                                                                               outlier.size=2, notch=FALSE) +
  scale_fill_manual(values=c("navy", "orange")) +
  facet_wrap(~ variables, scale = "free") +
  theme_bw()






