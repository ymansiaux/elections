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
    
    #' @field xtradataParameters parametre de filtre pour xtradata
    xtradataParameters = "",
    
    #' @field donneesElection donnees de l'election
    donneesElection = "",
    
    #' @field inseeElection code INSEE de la commune
    inseeElection = "",
    
    #' @field candidat avec palette de couleur
    candidatsElection = "",
    
    #' @field palette de couleurs pour les candidats
    couleursCandidats = "",
    
    #' @field resultats de la requete
    resultatsRequete = "",
    
    #' @field résultats globaux commune
    resultatsGlobauxCommune = "",
    
    #' @field résultats par LV
    resultatsLV = "",
    
    #' @field résultats par BV
    resultatsBV = "",
    
    #' @description
    #' Create a new occupation object.
    #' @param typeElection typeElection
    #' @param anneeElection rangeEnd
    #' @param donneesElection rangeStep
    #' @param inseeElection aggregation_unit
    #' @param candidatsElection plageHoraire
    #' @param couleursCandidats liste des parkings analyses
    #' @return A new `Occupation` object.
    
    initialize = function(typeElection = NULL, xtradataParameters = NULL, anneeElection = NULL, donneesElection = NULL, 
                          inseeElection = NULL, candidatsElection = NULL, couleursCandidats = NULL,
                          resultatsRequete = NULL, resultatsGlobauxCommune = NULL, resultatsLV = NULL, resultatsBV = NULL) {
      self$typeElection <- typeElection
      self$anneeElection <- anneeElection
      self$xtradataParameters <- xtradataParameters
      self$donneesElection <- donneesElection
      self$inseeElection <- inseeElection
      self$candidatsElection <- candidatsElection
      self$couleursCandidats <- couleursCandidats
      self$resultatsRequete <- resultatsRequete
      self$resultatsGlobauxCommune <- resultatsGlobauxCommune
      self$resultatsLV <- resultatsLV
      self$resultatsBV <- resultatsBV
    }
    
  )
)

createR6Election <- function(dat, type_elections, annee_evenement, code_insee) {
  
    donneesElection <- dat %>% 
      filter(type_election %in% type_elections & year(as_datetime(date_evenement)) %in% annee_evenement & insee %in% code_insee) 
    
    # if(type_elections == "Présidentielle" & annee_evenement == 2017) browser()
    
    # filter(dat, year(as_datetime(date_evenement)) %in% annee_evenement)
    filterXtradata <- list(
      "type_election" = unique(donneesElection$type_election),
      "insee" = unique(donneesElection$insee),
      "nom_election" = list("$in" = as.list(unique(donneesElection$nom_election)))
    )
    
    Election$new(
      typeElection = type_elections,
      anneeElection = annee_evenement,
      xtradataParameters = filterXtradata,
      inseeElection = code_insee,
      donneesElection = NULL,
      candidatsElection = NULL,
      couleursCandidats = NULL,
      resultatsRequete = NULL, 
      resultatsGlobauxCommune = NULL, 
      resultatsLV = NULL, 
      resultatsBV = NULL
    )
    
  }


# https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
