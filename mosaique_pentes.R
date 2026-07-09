library(outilsGeomCerfo)
library(magrittr)
outils <- obtenirOutils()

## ------------------------------------------------------------------------------
## Chemins
## ------------------------------------------------------------------------------
dossierRasters <- "F:/temps/pentes"
dossierEnregistrement <- "D:/donnes_lidar_mnt_etc/pentes_30m"


lesChemins <- outils$lireListeFichierExt(dossier = dossierRasters)

outils$base$setNom <- "Pentes"


processusPentes <- function(chemin) {
    enregistrement <- file.path(dossierEnregistrement, glue::glue("{outils$base$nomBaseSansExt(chemin)}_30m.tif"))
    if (file.exists(enregistrement)) {
        outils$base$ajusterCompteur(1)
        return(NULL)
    }

    monRaster <- terra::rast(chemin)
    monRaster <- terra::ifel(monRaster > 100, 100, monRaster)
    monRasterAgg <- terra::aggregate(monRaster, fact = 15, fun = mean, na.rm = TRUE) %>%
      terra::project(., y = "EPSG:32198", method = "near", res = 30)
    
    terra::writeRaster(monRasterAgg, filename = enregistrement, overwrite = TRUE)
    rm(monRaster, monRasterAgg, monRectangle, refe, rasterFinal)
    outils$base$ajusterCompteur(1)

    cli::cli_alert_success("Fait {outils$base$getCompteur()} de {2337}")
    return(NULL)
}


purrr::walk(lesChemins, processusPentes, .progress = "processusPentes")


## ------------------------------------------------------------------------------
## Deuxieme partie creation de la mosaique
## ------------------------------------------------------------------------------

dossierEnregistrementFinal <- "U:/Projets_2023/23-1042-DRF_Model_spat_temp_nerprun/02_Realisation/4_Traitements/03_propagation/rasters_biophysiques_30m"
listeRasters <- outils$lireListeRaster(dossier = dossierEnregistrement)
listeRasters <- terra::sprc(listeRasters)
pentes <- terra::mosaic(listeRasters, fun = "max", filename = file.path(dossierEnregistrementFinal, "pentes_max_30m.tif"))
