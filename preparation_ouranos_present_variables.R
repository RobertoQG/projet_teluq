
library(outilsGeomCerfo)
outils <- obtenirOutils()

dossier <- "U:/Projets_2023/23-1042-DRF_Model_spat_temp_nerprun/02_Realisation/1_Donnees_recues/03_propagation/donnees_ouranos"
dossierTemps <- "C:/Users/rquezada/Downloads"
dossierRefenceFuture <- "U:/Projets_2023/23-1042-DRF_Model_spat_temp_nerprun/02_Realisation/4_Traitements/03_propagation/rasters_climatiques_10km/future"

# La resolution est 10 km et pas 1 km
monRasterBase <- terra::rast(outils$lireListeFichierExt(dossierRefenceFuture)[[1]])

listeChemins <- outils$lireListeFichierExt(dossier = dossier, ext = ".zip")
purrr::walk(listeChemins, unzip, exdir = dossierTemps)

outils$base$garderElements(outils, dossierTemps, monRasterBase)

listeComplete <- outils$lireListeFichierExt(dossierTemps, ext = ".tiff")
listeChemins <- purrr::keep(listeComplete, stringr::str_detect, pattern = "reference_p50")
purrr::walk(purrr::discard(listeComplete, stringr::str_detect, pattern = "reference_p50"), file.remove)



purrr::walk(listeChemins, function(chemin){
  tryCatch({
      leNom <- outils$base$nomBaseSansExt(chemin)
  enregistrement <- file.path(dossierTemps, glue::glue("{leNom}_final.tif") )
  monRaster <- terra::rast(chemin) %>%
    terra::project(., y = "EPSG:32198", method = "near", res = 10000)
  monRasterFinal <- terra::resample(monRaster, monRasterBase, method = "near")
  
  terra::writeRaster(monRasterFinal, filename = enregistrement, overwrite = TRUE)
  rm(monRasterFinal)
  return(enregistrement)
  }, error = function(e){
    outils$base$ajouterErreur(e$message)
  }, warning = function(w){
    outils$base$ajouterWarning(w$message)
  })
})

purrr::walk(listeComplete, file.remove)




##------------------------------------------------------------------------------
## reduction des rasters
"
date: janvier 2026

Script pour la sélection des variables climatiques d'ouranos par mois
Il fait la réduction de la varibles par mois. On obtient deux résultats
soit la medianne ou la moyenne

On ne considère pas DJf (hiver)

✔ MAM = March–April–May → printemps (mars–avril–mai)
✔ JJA = June–July–August → été (juin–juillet–août)
✔ SON = September–October–November → automne (septembre–octobre–novembre)
✔ DJF = December–January–February → hiver (décembre–janvier–février)
"

outils$base$garderElements(outils, dossierTemps)

dossierSortie <- "U:/Projets_2023/23-1042-DRF_Model_spat_temp_nerprun/02_Realisation/4_Traitements/03_propagation/rasters_climatiques_10km/passe"

moisInteret <- c("MAM", "JJA", "SON")
variables <- c("tx_mean", "tn_mean", "tg_mean", "prcptot", "degree_days")
#sum pour la precipitation prcptot les rester des variables c'est la moyenne'

listeChemins <- outils$lireListeFichierExt(dossier = dossierTemps)

listeCheminsParVariable <- purrr::map(
  purrr::set_names(variables),          
  function(var) {
    purrr::keep(listeChemins, .p = function(chemin) {
      stringr::str_detect(chemin, pattern = var)
    })
  }
)

# rm.na est inclus dans cette version de terra
listeRasters <- purrr::imap(listeCheminsParVariable, function(listeChemins, nomVar) {
  rasters <- purrr::map(listeChemins, terra::rast)
  collection <- terra::sprc(rasters)
  if(nomVar == "prcptot"){
    terra::mosaic(collection, fun = "sum",  filename = file.path(dossierSortie, paste0(nomVar, "_sum_reference_ouranos_1991_2020_10km.tif")),overwrite = TRUE)
  } else {
      terra::mosaic(collection, fun = "mean",  filename = file.path(dossierSortie, paste0(nomVar, "_mean_reference_ouranos_1991_2020_10km.tif")),overwrite = TRUE)
  }
})

purrr::map(listeChemins, file.remove)







