library(outilsGeomCerfo)
library(magrittr)
outils <- obtenirOutils()

## ------------------------------------------------------------------------------
## Chemins
## ------------------------------------------------------------------------------
dossierRasters <- "F:/temps/donnees_mhc"
dossierEnregistrement <- "D:/donnes_lidar_mnt_etc/donnees_mhc_30"
lesChemins <- outils$lireListeFichierExt(dossier = dossierRasters)
outils$base$setNom <- "MHC"

total <- length(lesChemins)

lesChemins <- rev(lesChemins)



processusMCH <- function(chemin) {
    enregistrement <- file.path(dossierEnregistrement, glue::glue("{outils$base$nomBaseSansExt(chemin)}_30m.tif"))
    if (file.exists(enregistrement)) {
        outils$base$ajusterCompteur(1)
        return(NULL)
    }
    tryCatch(
        {
            monRaster <- terra::rast(chemin)
            leNom <- outils$base$nomBaseSansExt(chemin)
            monRasterAgg <- terra::aggregate(monRaster, fact = 30, fun = mean, na.rm = TRUE) %>%
                terra::project(., "EPSG:32198", method = "near", res = 30)
            monRasterAgg <- magrittr::set_names(monRasterAgg, leNom)
            terra::writeRaster(monRasterAgg, filename = enregistrement, overwrite = TRUE)
            rm(monRaster, monRasterAgg)
            outils$base$ajusterCompteur(1)
            cli::cli_alert_success("Fait {outils$base$getCompteur()} de {total}")
        },
        error = function(e) {
          msg <- glue::glue("{enregistrement} : {e$message}")
            outils$base$ajouterErreur(msg)
        },
        warning = function(w) {
            outils$base$ajouterWarning(w$message)
        },
        finally = function() {
            outils$base$nettoyerTempsTerra()
        }
    )
}

purrr::walk(lesChemins, processusMCH, .progress = "processusMHC")


## -----------------------------------------------------------------------------
## mosaique MHC
## -----------------------------------------------------------------------------

outils$base$garderElements(outils, dossierEnregistrement)
dossierEnregistrementFinal <- "U:/Projets_2023/23-1042-DRF_Model_spat_temp_nerprun/02_Realisation/4_Traitements/03_propagation/rasters_biophysiques_30m"

listeRasters <- outils$lireListeRaster(dossierEnregistrement)
laMosaique <- terra::sprc(listeRasters)
mhc <- terra::mosaic(laMosaique, fun = "max", filename = file.path(dossierEnregistrementFinal, "mhc_max_30m.tif"))

