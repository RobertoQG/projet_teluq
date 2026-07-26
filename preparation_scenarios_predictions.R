library(outilsGeomCerfo)

## Pour la préparation des rasters des trois scenarios climatique pour la
## prediction final


# Initialisation ---------------------------------------------------------
outilsTest <- obtenirOutils()

cheminRasterStack <- "./data/stackVariables.tif"
cheminPoints       <- "./data/pointsNerprun.csv"
dossierPresent     <- "O:/Teluq/donnees"
dossierFuture      <- "O:/Teluq/donnees/future"
nomVariables       <- c("Precipitation", "TempMoyenne", "TempMin", "TempMax")

# Stack actuel et variables statiques -------------------------------------
monStack <- terra::rast(cheminRasterStack)
names(monStack)

statiques      <- c("MHC", "MNT", "pentes", "Perturbations", "TPI", "TWI")
rasterStatique <- tidyterra::select(monStack, dplyr::all_of(statiques))

# Liste des fichiers futurs et séparation par scénario ---------------------
cheminsFuture <- outilsTest$outils$lireListeFichierExt(dossierFuture)

chemins_ssp245 <- purrr::keep(cheminsFuture, ~ stringr::str_detect(.x, pattern = "ssp245"))
chemins_ssp370 <- purrr::keep(cheminsFuture, ~ stringr::str_detect(.x, pattern = "ssp370"))
chemins_ssp585 <- purrr::keep(cheminsFuture, ~ stringr::str_detect(.x, pattern = "ssp585"))

# Fonction de traitement d'un scénario --------------------------------------
traiterScenario <- function(chemins, nomVariables, rasterStatique, monStack, cheminSortie) {
  rasters <- purrr::map(chemins, terra::rast) |> terra::rast()
  rasters <- terra::resample(rasters, monStack)
  rasters <- magrittr::set_names(rasters, nomVariables)
  rasters <- c(rasters, rasterStatique)
  terra::writeRaster(rasters, filename = cheminSortie, overwrite = TRUE)
  return(rasters)
}

# Traitement des trois scénarios ---------------------------------------------
rasters_ssp245 <- traiterScenario(
  chemins        = chemins_ssp245,
  nomVariables   = nomVariables,
  rasterStatique = rasterStatique,
  monStack       = monStack,
  cheminSortie   = "./data/stackVariables_ssp245.tif"
)

rasters_ssp370 <- traiterScenario(
  chemins        = chemins_ssp370,
  nomVariables   = nomVariables,
  rasterStatique = rasterStatique,
  monStack       = monStack,
  cheminSortie   = "./data/stackVariables_ssp370.tif"
)

rasters_ssp585 <- traiterScenario(
  chemins        = chemins_ssp585,
  nomVariables   = nomVariables,
  rasterStatique = rasterStatique,
  monStack       = monStack,
  cheminSortie   = "./data/stackVariables_ssp585.tif"
)
  

