library(outilsGeomCerfo)
library(magrittr)
outils <- obtenirOutils()

## ------------------------------------------------------------------------------
## Chemins
## ------------------------------------------------------------------------------
dossierRasters <- "F:/temps/donnees_twi"
dossierEnregistrement <- "D:/donnes_lidar_mnt_etc/donnees_TWI_30m"


lesChemins <- outils$lireListeFichierExt(dossier = dossierRasters)

total <- length(lesChemins)

outils$base$setNom <- "TWI"


processusTWI <- function(chemin) {
  enregistrement <- file.path(dossierEnregistrement, glue::glue("{outils$base$nomBaseSansExt(chemin)}_30m.tif"))
  if (file.exists(enregistrement)) {
    outils$base$ajusterCompteur(1)
    return(NULL)
  }

  monRaster <- terra::rast(chemin)
  monRasterAgg <- terra::aggregate(monRaster, fact = 30, fun = mean, na.rm = TRUE) %>%
    terra::project(., y = "EPSG:32198", method = "near", res = 30)

  terra::writeRaster(monRasterAgg, filename = enregistrement, overwrite = TRUE)
  rm(monRaster, monRasterAgg)
  outils$base$ajusterCompteur(1)

  cli::cli_alert_success("Fait {outils$base$getCompteur()} de {2337}")
  return(NULL)
}


purrr::walk(lesChemins, processusTWI, .progress = "processusTWI")


## ------------------------------------------------------------------------------
## Deuxieme partie creation de la mosaique
## ------------------------------------------------------------------------------

dossierEnregistrementFinal <- "U:/Projets_2023/23-1042-DRF_Model_spat_temp_nerprun/02_Realisation/4_Traitements/03_propagation/rasters_biophysiques_30m"
listeRasters <- outils$lireListeRaster(dossier = dossierEnregistrement)
listeRasters <- terra::sprc(listeRasters)
TWI <- terra::mosaic(listeRasters, fun = "max", filename = file.path(dossierEnregistrementFinal, "TWI_max_30m.tif"))
