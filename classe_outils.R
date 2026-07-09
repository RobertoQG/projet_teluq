#' Classe R6 contenant les outils
#'
#' @noRd
#' @import R6
#' @import purrr
#' @importFrom terra extract global project
#' @author Roberto Quezada Garcia
#' @description Classe pour les processus generaux
#' 
Outils <- R6::R6Class(
    classname = "Outils",
    lock_objects = FALSE,
    public = list(
        initialize = function() {},
        obtenirNom = function(monTerra) {
            tools::file_path_sans_ext(basename(monTerra))
        },
        lireListePaths = function(dossier, nomSeulement = FALSE, enleverExtension = FALSE) {
            tryCatch(
                {
                    fichiers <- list.files(dossier, full.names = !nomSeulement)
                    if (enleverExtension) {
                        fichiers <- tools::file_path_sans_ext(fichiers)
                    }
                    return(fichiers)
                },
                error = function(e) {
                    msg <- glue::glue("Erreur dans lireListePaths : {e$message}")
                    cli::cli_alert_danger(msg)
                    return(NULL)
                },
                warning = function(w) {
                    msg <- glue::glue("Avertissement dans lireListePaths : {w$message}")
                    cli::cli_alert_warning(msg)
                    invokeRestart("muffleWarning")
                }
            )
        },
        lireListeFichierExt = function(dossier, ext = ".tif", recursive = FALSE) {
            tryCatch(
                {
                    list.files(
                        dossier,
                        pattern    = paste0("\\.", sub("^\\.", "", ext), "$"),
                        full.names = TRUE,
                        recursive  = recursive
                    )
                },
                error = function(e) {
                    msg <- glue::glue("Erreur dans lireListeFichierExt : {e$message}")
                    cli::cli_alert_danger(msg)
                    return(NULL)
                }
            )
        },
        transformerQuebecLambert = function(monTerra) {
            tryCatch(
                {
                    terra::project(monTerra, private$.quebecLambert$crs)
                },
                error = function(e) {
                    msg <- glue::glue("Erreur dans transformerQuebecLambert : {e$message}")
                    cli::cli_alert_danger(msg)
                    return(NULL)
                }
            )
        },
        obtenirStats = function(x) {
            terra::global(x, c("sum", "mean", "sd", "min", "max"), na.rm = TRUE)
        }
    ),
    private = list(
        .quebecLambert = list(crs = "EPSG:32198")
    )
)
