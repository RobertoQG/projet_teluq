source("classe_outils.R")

#' @noRd
#' @import R6
#' @import purrr
#' @importFrom terra extract global project
#' @author Roberto Quezada Garcia
#' @description Classe pour la preparation des donnees climatiques
#'
#
## -----------------------------------------------------------------------------
## Initialization
## ------------------------------------------------------------------------------
#'
outils <- Outils$new()
dossier <- "C:/Users/rquezada/donnees_ouranos"
dossierTemps <- "C:/Users/rquezada/Downloads"
extent <- terra::rast("./data/TWI_fill.tif") |>
    terra::ext()
dossierSortie <- "./data/future"

listeChemins <- outils$lireListeFichierExt(dossier = dossier, ext = ".zip")
variables <- c("tx_mean", "tn_mean", "tg_mean", "prcptot", "degree_days")
listeScenarios <- c("ssp245", "ssp370", "ssp585") # 3. c() au lieu de list()
moisInteret <- c("MAM", "JJA", "SON")
lePattern <- glue::glue_collapse(moisInteret, sep = "|")

## ------------------------------------------------------------------------------
## Nettoyage des données
## ------------------------------------------------------------------------------
purrr::walk(listeChemins, unzip, exdir = dossierTemps)

purrr::walk(
    purrr::keep(outils$lireListeFichierExt(dossierTemps, ext = ".tiff"), ~ stringr::str_detect(.x, "reference")), file.remove
)
purrr::walk(
    purrr::discard(outils$lireListeFichierExt(dossierTemps, ext = ".tiff"), ~ stringr::str_detect(.x, "p50")), file.remove
)
purrr::walk(
    purrr::discard(
        outils$lireListeFichierExt(dossierTemps, ext = ".tiff"),
        ~ stringr::str_detect(.x, "2021-2050")
    ), file.remove
)

#
purrr::walk(
    purrr::discard(outils$lireListeFichierExt(dossierTemps, ext = ".tiff"), ~ stringr::str_detect(.x, lePattern) | stringr::str_detect(.x, "degree_days")), file.remove
)

listeComplete <- outils$lireListeFichierExt(dossierTemps, ext = ".tiff")


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
        terra::project(y = "EPSG:32198", method = "near", res = 10000)
}

rastersParScenarios <- purrr::map(
    listeParScenarios, ~ purrr::map(.x, ~ purrr::map(.x, lectureRaster))
)


fonctions_mosaic <- c(
    tx_mean = "mean", tn_mean = "mean", tg_mean = "mean",
    prcptot = "sum", degree_days = "mean"
)


## ------------------------------------------------------------------------------
## Preparation des donnees et enregistrement
## -----------------------------------------------------------------------------
purrr::imap(rastersParScenarios, function(scenario_list, scenario_name) {
    purrr::imap(scenario_list, function(raster_list, var_name) {
        if (length(raster_list) == 0) {
            return(NULL)
        }

        collection <- terra::sprc(raster_list)
        fun_mosaic <- fonctions_mosaic[[var_name]] # "sum" ou "mean"
        monRaster <- terra::mosaic(collection, fun = fun_mosaic)
        monRaster <- terra::crop(monRaster, extent)

        suffixe <- if (fun_mosaic == "sum") "_sum_" else "_mean_"
        enregistrement <- file.path(dossierSortie, paste0(var_name, suffixe, scenario_name, "_future_ouranos_10km.tif"))
        terra::writeRaster(monRaster, filename = enregistrement, overwrite = TRUE)
    })
})

purrr::walk(listeComplete, file.remove)
