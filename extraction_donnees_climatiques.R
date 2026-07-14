


source("classe_outils.R")
library(magrittr)


##------------------------------------------------------------------------
## Initialisation
outils  <- Outils$new()

## -------------------------------------------------------------------------
## Lecture des données
dossier    <- "O:/Teluq/donnees/donnees_climatique"
listeChemins <- outils$lireListeFichierExt(dossier)
cheminNerprun <- "O:/Teluq/donnees/nerprun_presence_absence_Mai2026.gpkg"
nerprun <- terra::vect(cheminNerprun)

monRaster <- purrr::map(listeChemins, terra::rast) %>% terra::rast()

## Extration des données
monExtraction <- terra::extract(monRaster, nerprun, ID = FALSE, bind = TRUE) %>%
  as.data.frame()

monDataFrame <- tidyr::pivot_longer(monExtraction, cols =  - nerprun, names_to = "variables")

library(ggplot2)

ggplot(monDataFrame, aes(x=variables, y=value, fill = nerprun)) + geom_boxplot(outlier.colour="gray", outlier.shape=16,
                                                                               outlier.size=2, notch=FALSE) +
  scale_fill_manual(values=c("navy", "orange")) +
  facet_wrap(~ variables, scale = "free") +
  theme_bw()

## -------------------------------------------------------------------------
## Test de Mann-Whitney (Wilcoxon) pour chaque variable
library(dplyr)
library(purrr)

variables_a_tester <- setdiff(names(monExtraction), "nerprun")

resultats_wilcox <- purrr::map_dfr(variables_a_tester, function(var) {
  
  formule <- as.formula(paste(var, "~ nerprun"))
  
  test <- wilcox.test(formule, data = monExtraction, exact = FALSE)
  
  # Taille d'effet r = Z / sqrt(N)
  n <- sum(!is.na(monExtraction[[var]]))
  z <- qnorm(test$p.value / 2, lower.tail = FALSE) * sign(diff(
    tapply(monExtraction[[var]], monExtraction$nerprun, median, na.rm = TRUE)
  ))
  effet_r <- z / sqrt(n)
  
  data.frame(
    variable   = var,
    W          = test$statistic,
    p_value    = test$p.value,
    n          = n,
    effet_r    = round(effet_r, 3),
    mediane_absence  = median(monExtraction[[var]][monExtraction$nerprun == "absence"], na.rm = TRUE),
    mediane_presence = median(monExtraction[[var]][monExtraction$nerprun == "presence"], na.rm = TRUE)
  )
})

resultats_wilcox <- resultats_wilcox %>%
  dplyr::mutate(significatif = ifelse(p_value < 0.05, "***", "ns"))

write.csv(resultats_wilcox, file = "variablesClimatiques.csv" )
