#' R6 Super Class pour donnees occupations et saturation des parkings
#'
#' @description
#' utilisee ensuite par classe Occupation et Saturation
Election <- R6::R6Class(
  "Election",
  public = list(
    #' @field typeElection type d'election
    typeElection = "",
    
    #' @field anneeElection annee de l'election
    anneeElection = "",
    
    #' @field donneesElection donnees de l'election
    donneesElection = "",
    
    #' @field inseeElection code INSEE de la commune
    inseeElection = "",
    
    #' @field candidat avec palette de couleur
    candidatsElection = "",
    
    #' @field palette de couleurs pour les candidats
    couleursCandidats = "",
    
    #' @description
    #' Create a new occupation object.
    #' @param typeElection typeElection
    #' @param anneeElection rangeEnd
    #' @param donneesElection rangeStep
    #' @param inseeElection aggregation_unit
    #' @param candidatsElection plageHoraire
    #' @param couleursCandidats liste des parkings analyses
    #' @return A new `Occupation` object.
    
    initialize = function(typeElection = NULL, anneeElection = NULL, donneesElection = NULL, inseeElection = NULL, candidatsElection = NULL, couleursCandidats = NULL) {
      self$typeElection <- typeElection
      self$anneeElection <- anneeElection
      self$donneesElection <- donneesElection
      self$inseeElection <- inseeElection
      self$candidatsElection <- candidatsElection
      self$couleursCandidats <- couleursCandidats
    }
  )
)

createR6Election <- function(w, x, y, z) {
  
  donneesElection <- w %>% 
    filter(type_election %in% x & annee_election %in% y & code_insee %in% z) 
  
  Election$new(
    typeElection = x,
    anneeElection = y,
    inseeElection = z,
    donneesElection = donneesElection,
    candidatsElection = donneesElection %>% 
      select(nom_candidat) %>% 
      distinct(),
    couleursCandidats = NULL
  )
}

# https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
