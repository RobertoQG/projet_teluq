library(outilsGeomCerfo)

outils <- obtenirOutils()

outils$base$setNom <- "TPI"

dossier <- "D:/donnes_lidar_mnt_etc/donnees_mnt_30m"
dossieEnregistrement <- "D:/donnes_lidar_mnt_etc/tpi_30m"

listeRasters <- outils$lireListeFichierExt(dossier = dossier)

outils$base$setDossier <- dossieEnregistrement
outils$base$setNom     <- "TPI_Nerprun"


purrr::walk(listeRasters, function(chemin, scale = 5) {
    tryCatch(
        {
            monRaster            <- terra::rast(chemin)
            cheminEnregistrement <- file.path(dossieEnregistrement, glue::glue("{stringr::str_split(outils$base$nomBaseSansExt(chemin), pattern = '_')[[1]][1]}_TPI_{scale}x{scale}_R.tif"))
            resTPI <- spatialEco::tpi(monRaster, scale = scale)
            terra::writeRaster(resTPI, filename = cheminEnregistrement, overwrite = TRUE)
            message <- glue::glue("
              /n=================================================================
              MNT Origine : {chemin}
              resolution: {terra::res(monRaster)}
              Fonction TIP: spatialEco::tpi
              Parametres: MNT, fenêtre de {scale}
              chemin d'enregistrement: {cheminEnregistrement}
               "
              )
            outils$base$ajouterInfo(message)
        },
        error = function(e) {
            cli::cli_alert_danger(e$message)
            outils$base$ajouterErreur(e$message)
        }
    )
})


