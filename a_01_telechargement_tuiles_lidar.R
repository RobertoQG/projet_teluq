## ------------------------------------------------------------------------------
## Initialization des chemins et de l'instance TelechargementsLIDAR
## ------------------------------------------------------------------------------
#' @author Roberto Quezada Garcia
#' @description
#' Script pour le télécharment en paralle des données dérivés du lidar 
#' Modèle numerique de terrain, Indice d'humidité TWI, Modèle numerique de Canopé
#' 
library(magrittr)
library(future)
plan(multisession, workers = 8)

cheminFeuillets <- "./data/URL_feuillet_ProDer.shp"
enregistrement <- "C:/Users/rquezada/Downloads"
source("./classe_telechargement.R")

telech <- TelechargementsLIDAR$new()

## ------------------------------------------------------------------------------
## Téléchargements
## Note: téléchargements en parallèle (~2 400 fichiers pour tout le Québec)
## ------------------------------------------------------------------------------

## TWI
## ----------------------------------------------------------------------------
monShape <- sf::read_sf(cheminFeuillets) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(feuillet, twi_url) %>%
    dplyr::mutate(
        nom    = paste0("TWI_", feuillet, ".tif"),
        monUrl = paste0(gsub("/$", "", twi_url), "/", nom)
    ) %>%
    dplyr::select(nom, monUrl) %>%
    unique()

furrr::future_walk2(monShape$monUrl, monShape$nom, function(url, nom) {
    fichier <- file.path(enregistrement, nom)
    if (file.exists(fichier)) {
        return(invisible(NULL))
    }
    tryCatch(
        telech$telechargerFichier(url, enregistrement, nom),
        error = function(e) message("Erreur TWI ", nom, " : ", conditionMessage(e))
    )
}, .progress = TRUE)

## MHC
## ----------------------------------------------------------------------------
laSelection <- sf::read_sf(cheminFeuillets) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(feuillet, lidar_url) %>%
    dplyr::mutate(
        nom    = paste0("MHC_", feuillet, ".tif"),
        monUrl = paste0(gsub("/$", "", lidar_url), "/", nom)
    ) %>%
    dplyr::select(nom, monUrl) %>%
    unique()

furrr::future_walk2(laSelection$monUrl, laSelection$nom, function(url, nom) {
    fichier <- file.path(enregistrement, nom)
    if (file.exists(fichier)) {
        return(invisible(NULL))
    }
    tryCatch(
        telech$telechargerFichier(url, enregistrement, nom),
        # CORR: handler ajouté pour ne pas avaler silencieusement les erreurs
        error = function(e) message("Erreur MHC ", nom, " : ", conditionMessage(e))
    )
}, .progress = TRUE)

## MNT
## ----------------------------------------------------------------------------
laSelection <- sf::read_sf(cheminFeuillets) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(feuillet, lidar_url) %>%
    dplyr::mutate(
        nom    = paste0("MNT_", feuillet, ".tif"), # corrigé
        monUrl = paste0(gsub("/$", "", lidar_url), "/", nom)
    ) %>%
    dplyr::select(nom, monUrl) %>%
    unique()

furrr::future_walk2(laSelection$monUrl, laSelection$nom, function(url, nom) {
    fichier <- file.path(enregistrement, nom)
    if (file.exists(fichier)) {
        return(invisible(NULL))
    }
    tryCatch(
        telech$telechargerFichier(url, enregistrement, nom),
        error = function(e) message("Erreur MNT ", nom, " : ", conditionMessage(e))
    )
}, .progress = TRUE)
