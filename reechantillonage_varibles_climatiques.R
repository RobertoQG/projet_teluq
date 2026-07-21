# Chargement des outils CERFO
library(outilsGeomCerfo)

# Création de l'objet contenant les fonctions du package
outils <- obtenirOutils()

# Dossier contenant les rasters climatiques à convertir en 30 m
dossier <- "O:/Teluq/donnees/donnees_climatique"

# Raster de référence définissant la résolution, l'étendue et la projection
rasterReference <- "O:/Teluq/donnees/MNT_max_30m.tif"

# Lecture du raster de référence
monMNT <- terra::rast(rasterReference)

# Lecture de tous les rasters présents dans le dossier
listeFichiers <- outils$raster$lireListeRaster(dossier)

# Rééchantillonnage de chacun des rasters à la résolution du MNT
purrr::imap(listeFichiers, function(r, nom) {
    # Retrait du suffixe "_10km" du nom de fichier
    leNom <- stringr::str_remove(nom, pattern = "_10km")
    # Construction du chemin d'enregistrement du raster en 30 m
    enregistrement <- file.path(dossier, glue::glue("{leNom}_30m.tif"))
    # Rééchantillonnage par interpolation moyenne et enregistrement sur disque
    terra::resample(x = r, y = monMNT, method = "mean", threads = TRUE, filename = enregistrement)
})



