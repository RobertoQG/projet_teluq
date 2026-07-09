
#' @import R6
#' @noRd
#' @author Roberto Quezada Garcia
#' @description
#' Pour le telechargement apartir d'un chemin
TelechargementsLIDAR <- R6::R6Class("TelechargementsLIDAR",
                                    public = list(
                                      base = NULL,
                                      initialize = function(){
                                      },
                                      #' Fonction pour telecharger un fichier
                                      #'
                                      #' @param chemin chemin complet du fichier a telecharger
                                      #' @param dossierEnregistrement dossier d'enregistrement
                                      #' @param nom nom du fichier avec extention
                                      #' @noRd
                                      #' @returns void
                                      telechargerFichier = function(chemin, dossierEnregistrement, nom){
                                        dest <- file.path(dossierEnregistrement, nom)
                                        curl::curl_download(chemin, dest)
                                      }
                                    ),
                                    private = list()
)



