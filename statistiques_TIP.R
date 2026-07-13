library(outilsGeomCerfo)
outils <- obtenirOutils()

dossierTPI <- "U:/Projets_2023/23-1042-DRF_Model_spat_temp_nerprun/02_Realisation/4_Traitements/03_propagation/test_tpi/resultats"
cheminPointsPresence <- "O:/Teluq/donnees/nerprun_presence_absence_Mai2026.gpkg"

monVecteur <- terra::vect(cheminPointsPresence) %>%
    tidyterra::mutate(., valeur = 1)

listeRasters <- outils$lireListeRaster(dossier = dossierTPI, index = TRUE)

outils$base$garderElements(outils, monVecteur, listeRasters)


dfFinal <- purrr::imap_dfr(listeRasters, function(monRaster, nom) {
    monRaster <- magrittr::set_names(monRaster, nom)
    monDF <- terra::extract(monRaster, monVecteur, ID = FALSE, bind = TRUE) %>%
        terra::as.data.frame()
    monDF <- magrittr::set_names(monDF, c("nerprun", "valeur", nom))
}) %>%
    tidyr::pivot_longer(., cols = -c("nerprun", "valeur"), names_to = "TPI") %>%
    tidyr::drop_na()


library(ggplot2)
library(formatGraphiquesCerfo)
library(ggrepel)

moyennes <- dplyr::group_by(dfFinal, nerprun, TPI) %>%
    dplyr::summarise(
        moyenne = mean(value, na.rm = TRUE),
        sd      = sd(value, na.rm = TRUE),
        obs     = sum(!is.na(valeur)),
        .groups = "drop"
    ) %>%
    dplyr::mutate(.,
        groupe = dplyr::case_when(
            stringr::str_detect(TPI, "3x3") ~ "3x3",
            stringr::str_detect(TPI, "5x5") ~ "5x5",
            stringr::str_detect(TPI, "7x7") ~ "7x7",
            TRUE ~ NA_character_
        )
    )

ggplot(moyennes, aes(y = moyenne, x = TPI, fill = nerprun)) +
    geom_bar(
        stat = "identity", color = "black",
        position = position_dodge(width = 0.9)
    ) +
    geom_text_repel(
        aes(label = obs),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 3.5,
        segment.color = "grey50",
        box.padding = 0.3,
        show.legend = FALSE
    ) +
    scale_fill_manual(values = c("darkorange", "cadetblue4")) +
    labs(
        title = "Valeur moyenne du TPI par raster et par taille de fenêtre",
        subtitle = "Les nombres affichés sur les barres correspondent au nombre d'observations (n)",
        #caption = "CERFO 2026",
        x = "Raster et taille de la fenêtre",
        y = "Valeur moyenne du TPI"
    ) +
    formatGraphiquesCerfo::graphiqueBlanc()



library(broom)

resultatsWilcox <- dfFinal %>%
  dplyr::mutate(nerprun = as.factor(nerprun)) %>%
  dplyr::group_by(TPI) %>%
  dplyr::group_modify(~ broom::tidy(wilcox.test(value ~ nerprun, data = .x))) %>%
  dplyr::ungroup()

resultatsWilcox



