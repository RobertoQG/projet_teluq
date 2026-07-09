library(outilsGeomCerfo)
outils <- obtenirOutils()


dossier <- "U:/Projets_2023/23-1042-DRF_Model_spat_temp_nerprun/02_Realisation/1_Donnees_recues/03_propagation/donnees_ouranos"
dossierTemps <- "C:/Users/rquezada/Downloads"
extent <- terra::rast("U:/Projets_2023/23-1042-DRF_Model_spat_temp_nerprun/02_Realisation/4_Traitements/03_propagation/rasters_biophysiques_30m/TWI_fill.tif") %>%
    terra::ext()

dossierSortie <- "U:/Projets_2023/23-1042-DRF_Model_spat_temp_nerprun/02_Realisation/4_Traitements/03_propagation/rasters_climatiques_10km/future"


# La resolution est 10 km et pas 1 km

listeChemins <- outils$lireListeFichierExt(dossier = dossier, ext = ".zip")

variables <- c("tx_mean", "tn_mean", "tg_mean", "prcptot", "degree_days")
listeScenarios <- list("ssp245", "ssp370", "ssp585")
moisInteret <- c("MAM", "JJA", "SON")

lePattern <- glue::glue_collapse(moisInteret, sep = "|")

## ------------------------------------------------------------------------------
## Nettoyage des données
## ------------------------------------------------------------------------------
purrr::walk(listeChemins, unzip, exdir = dossierTemps)
purrr::walk(purrr::keep(outils$lireListeFichierExt(dossierTemps, ext = ".tiff"), stringr::str_detect, pattern = "reference"), file.remove) # on enlever reference
purrr::walk(purrr::discard(outils$lireListeFichierExt(dossierTemps, ext = ".tiff"), stringr::str_detect, pattern = "p50"), file.remove) # on enleve ce qui nest pas p50
purrr::walk(purrr::discard(outils$lireListeFichierExt(dossierTemps, ext = ".tiff"), stringr::str_detect, pattern = "2021-2050"), file.remove) # on enleve ce qui n'est pas dans le range
purrr::walk(purrr::discard(outils$lireListeFichierExt(dossierTemps, ext = ".tiff"), ~ stringr::str_detect(.x, lePattern) || stringr::str_detect(.x, "degree_days")), file.remove)

listeComplete <- outils$lireListeFichierExt(dossierTemps, ext = ".tiff") # on lis le ce qui reste


listeParScenarios <- purrr::map(listeScenarios, function(scen) {
    chemins_scen <- purrr::keep(listeComplete, ~ stringr::str_detect(.x, scen))
    purrr::map(variables, function(var) {
        purrr::keep(chemins_scen, ~ stringr::str_detect(.x, var))
    }) |>
        purrr::set_names(variables)
}) |>
    purrr::set_names(listeScenarios)


lectureRaster <- function(chemin) {
    terra::rast(chemin) |>
        terra::project(
            y = "EPSG:32198",
            method = "near",
            res = 10000
        )
}

rastersParScenarios <- purrr::map(
    listeParScenarios,
    ~ purrr::map(
        .x,
        ~ purrr::map(.x, lectureRaster)
    )
)

purrr::imap(rastersParScenarios, function(scenario_list, scenario_name) {
    purrr::imap(scenario_list, function(raster_list, var_name) {
        if (length(raster_list) == 0) {
            return(NULL)
        }
        collection <- terra::sprc(raster_list)
        # mosaïque
        monRaster <- if (var_name == "prcptot") terra::mosaic(collection, fun = "sum") else terra::mosaic(collection, fun = "mean") # rm.na est inclus dans cette version de terra
        monRaster <- terra::crop(monRaster, extent)
        enregistrement <- file.path(dossierSortie, paste0(var_name, ifelse(var_name == "prcptot", "_sum_", "_mean_"), scenario_name, "_future_ouranos_10km.tif"))
        terra::writeRaster(monRaster, filename = enregistrement, overwrite = TRUE)
    })
})

purrr::walk(listeComplete, file.remove)
