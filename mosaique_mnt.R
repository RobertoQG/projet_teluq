library(outilsGeomCerfo)
library(magrittr)
outils <- obtenirOutils()


## ------------------------------------------------------------------------------
## Chemins
## ------------------------------------------------------------------------------
dossierRasters        <- "F:/temps/donnees_mnt"
dossierEnregistrement <- "D:/donnes_lidar_mnt_etc/donnees_mnt_30m"

lesChemins <- outils$lireListeFichierExt(dossier = dossierRasters)


lesChemins <- rev(lesChemins)

outils$base$setNom    <- "reechantillonage_MNT_30m"

processusMNT <- function(chemin) {
    tuileMNT <- outils$base$nomBaseSansExt(chemin)
    enregistrement <- file.path(dossierEnregistrement, glue::glue("{tuileMNT}_30m.tif"))
    if (file.exists(enregistrement)) {
        outils$base$ajusterCompteur(1)
        return(NULL)
    }
    monRasterAgg <- tryCatch(
        {
            ## on elimine les cellules en bas de 0
            ## agregation a 30 metres par la moyenne, projection avec la methode near
            monRaster <- terra::rast(chemin)
            monRaster <- terra::clamp(monRaster, lower = 0, values = TRUE)
            monRaster <-terra::aggregate(monRaster, fact = 30, fun = mean, na.rm = TRUE) %>%
                terra::project(., "EPSG:32198", method = "near", res = 30)
        },
        error = function(e) {
            outils$base$ajouterErreur(glue::glue("{tuileMNT} - aggregate/project : {e$message}"))
            NULL
        }
    )
    if (is.null(monRasterAgg)) {
        return(NULL)
    }
    tryCatch(
        {
            monRasterAgg <- magrittr::set_names(monRasterAgg, tuileMNT)
            terra::writeRaster(monRasterAgg, filename = enregistrement, overwrite = TRUE)
            rm(monRasterAgg, monRaster)
        },
        error = function(e) {
            outils$base$ajouterErreur(glue::glue("{tuileMNT} - renommer/write : {e$message}"))
        }
    )

    outils$base$ajusterCompteur(1)
    cli::cli_alert_success("Fait {outils$base$getCompteur()} de {length(lesChemins)}")
    return(NULL)
}

outils$base$garderElements(outils, processusMNT, lesChemins, dossierEnregistrement)

purrr::walk(lesChemins, processusMNT, .progress = "processusMNT")


## ------------------------------------------------------------------------------
## Deuxieme partie creation de la mosaique
## ------------------------------------------------------------------------------
dossierEnregistrementFinal <- "U:/Projets_2023/23-1042-DRF_Model_spat_temp_nerprun/02_Realisation/4_Traitements/03_propagation/rasters_biophysiques_30m"
listeRasters <- outils$lireListeRaster(dossier = dossierEnregistrement)


mesStatistiques <- outils$obtenirDFStatsRasters(listeRasters)

listeRasters <- terra::sprc(listeRasters)

MNT <- terra::mosaic(listeRasters, fun = "max", filename = file.path(dossierEnregistrementFinal, "MNT_max_30m.tif"))


