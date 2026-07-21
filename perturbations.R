library(outilsGeomCerfo)

# Chargement des outils CERFO
outils <- obtenirOutils()

# Nettoyage des fichiers temporaires et de la mémoire avant le traitement
outils$base$nettoyerTempsTerra()
outils$base$nettoyerMemoire()

# Configuration de terra :
# - utilisation maximale de 80 % de la RAM disponible
# - dossier temporaire sur un disque local plus rapide
terra::terraOptions(memfrac = 0.8, tempdir = "D:/temp_terra")

# Chemins des données
cheminEcoforestiere <- "V:/Projets_2025/25-1140-GFQ_adaptCC/03_Realisation/031_Donnees_client/CARTE_ECO_ORI_PROV_GDB/CARTE_ECO_ORI_PROV.gdb"
cheminRasterReference <- "O:/Teluq/donnees/MNT_max_30m.tif"
cheminEnregistrement <- "O:/Teluq/donnees/perturbations0.tif"

# Afficher les couches disponibles dans la géodatabase écoforestière
terra::vector_layers(cheminEcoforestiere)

# Charger la couche des peuplements écoforestiers et conserver uniquement
# le champ ORIGINE qui sera utilisé lors de la rasterisation
monVecteur <- terra::vect(cheminEcoforestiere, "PEE_ORI_PROV") %>%
    tidyterra::select(dplyr::all_of("ORIGINE"))

# Créer un raster de référence binaire :
# - les cellules valides prennent la valeur 1
# - les cellules NoData demeurent NoData
# Ce raster servira de gabarit pour la rasterisation.
rasterReference <- terra::ifel(    terra::rast(cheminRasterReference) >= 0,    1,    NA)

# Libérer la mémoire avant la rasterisation
outils$base$nettoyerMemoire()

# Conserver uniquement les objets nécessaires en mémoire
outils$base$garderElements(outils, rasterReference, monVecteur)

# Rasteriser la couche vectorielle en utilisant le champ ORIGINE
# et le raster de référence comme gabarit
terra::rasterize(    monVecteur,    rasterReference,    field = "ORIGINE",    filename = cheminEnregistrement)

# Recharger le raster créé
perturbations <- terra::rast(cheminEnregistrement)

## -----------------------------------------------------------------------------------
## Rééchantillonnage

# Aligner parfaitement le raster des perturbations sur le raster de référence
# (même résolution, emprise et grille)
pertubationsAligne <- terra::resample(    perturbations,    rasterReference)

# Enregistrer le raster aligné en remplaçant le fichier existant
terra::writeRaster(    pertubationsAligne,    filename = cheminEnregistrement,    overwrite = TRUE)
