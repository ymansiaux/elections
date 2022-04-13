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
    
    #' @field résultats abstention commune
    resultatsAbstention = "",
    
    #' @field résultats par LV
    resultatsLV = "",
    
    #' @field résultats par BV
    resultatsBV = "",
    
    #' @field résultats abstention BV
    resultatsAbstentionBV = "",
    
    #' @field résultats abstention LV
    resultatsAbstentionLV = "",
    
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
                          resultatsRequete = NULL, resultatsGlobauxCommune = NULL, resultatsAbstention = NULL,
                          resultatsLV = NULL, resultatsBV = NULL, resultatsAbstentionBV = NULL, resultatsAbstentionLV = NULL) {
      self$typeElection <- typeElection
      self$anneeElection <- anneeElection
      self$xtradataParameters <- xtradataParameters
      self$donneesElection <- donneesElection
      self$inseeElection <- inseeElection
      self$candidatsElection <- candidatsElection
      self$couleursCandidats <- couleursCandidats
      self$resultatsRequete <- resultatsRequete
      self$resultatsGlobauxCommune <- resultatsGlobauxCommune
      self$resultatsAbstention <- resultatsAbstention
      self$resultatsLV <- resultatsLV
      self$resultatsBV <- resultatsBV
      self$resultatsAbstentionBV <- resultatsAbstentionBV
      self$resultatsAbstentionLV <- resultatsAbstentionLV
      
    },
    
    download_data = function() {
      
      download <- try(xtradata_requete_features(
        key = Sys.getenv("XTRADATA_KEY"),
        typename = "EL_RESULTAT_A",
        filter = self$xtradataParameters,
        showURL = TRUE
      ))
      
      if (inherits(download, "try-error")) {
        self$donneesElection <- NULL
      } else {
        self$donneesElection <- download %>% 
          mutate(
            date_election = as_datetime(date_evenement, tz = "Europe/Paris"),
            annee_election = year(date_election)) %>%
          mutate(prenom = get_first_name(nom_candidat)) %>%
          mutate(nom = get_last_name(nom_candidat, prenom)) %>%
          mutate(nom = ifelse(nom == "", prenom, nom)) %>%
          mutate(nom_candidat_short = str_sub(nom_candidat,1,10)) %>%
          mutate(across(starts_with("nb_"), as.numeric)) %>%
          select(-date_evenement, -cdate, -mdate) %>%
          rename(nb_voix = valeur, code_insee = insee) %>%
          clean_names()
      }
    },
    
    compute_resultats_globaux_commune = function(donneesElection) {
      
      self$resultatsGlobauxCommune <- compute_resultats_elections(data = donneesElection,
                                                                  type = "participation",
                                                                  grouping_vars = c(
                                                                    "nom_election", "type_election", "annee_election",
                                                                    "numero_tour", "nom_candidat", "nom", "nom_candidat_short"
                                                                  ),
                                                                  truncate_results = TRUE, n_first_results = 8
      )
      
    },
    
    compute_abstention_commune = function(donneesElection) {
      self$resultatsAbstention <- compute_resultats_elections(data = donneesElection,
                                                              type = "abstention",
                                                              grouping_vars = c(
                                                                "nom_election", "type_election", "annee_election", "numero_tour")) 
      
    },
    
    compute_resultats_par_BV = function(donneesElection) {
      
      if("id_bureau" %in% colnames(donneesElection)) {
        self$resultatsBV <- compute_resultats_elections(data = donneesElection, 
                                    type = "participation", 
                                    grouping_vars = c(
                                      "nom_election", "type_election", "annee_election", 
                                      "numero_tour", "nom_candidat", "nom", "nom_candidat_short",
                                      "id_bureau"
                                    )
        )   %>% 
          mutate_at(vars("id_bureau"),as.character)

      }
      
    },
    
    compute_resultats_par_LV = function(donneesElection) {
      
      if("nom_lieu" %in% colnames(donneesElection)) {
        self$resultatsLV <- compute_resultats_elections(data = donneesElection, 
                                                        type = "participation", 
                                                        grouping_vars = c(
                                                          "nom_election", "type_election", "annee_election", 
                                                          "numero_tour", "nom_candidat", "nom", "nom_candidat_short",
                                                          "nom_lieu", "id_lieu"
                                                        )
        )    %>% 
          mutate_at(vars("nom_lieu"),as.character)
      }
      
    },
    
    compute_abstention_par_BV = function(donneesElection) {
      
      if("id_bureau" %in% colnames(donneesElection)) {
        self$resultatsAbstentionBV <- compute_resultats_elections(data = donneesElection, 
                                                        type = "abstention", 
                                                        grouping_vars = c(
                                                          "nom_election", "type_election", "annee_election", 
                                                          "numero_tour",
                                                          "id_bureau"
                                                        )
        )   %>% 
          mutate_at(vars("id_bureau"),as.character)
        
      }
      
    },
    
    compute_abstention_par_LV = function(donneesElection) {
      
      if("nom_lieu" %in% colnames(donneesElection)) {
        self$resultatsAbstentionLV <- compute_resultats_elections(data = donneesElection, 
                                                        type = "abstention", 
                                                        grouping_vars = c(
                                                          "nom_election", "type_election", "annee_election", 
                                                          "numero_tour", 
                                                          "nom_lieu", "id_lieu"
                                                        )
        )    %>% 
          mutate_at(vars("nom_lieu"),as.character)
      }
      
    }
    
  )
)

createR6Election <- function(dat, type_elections, annee_evenement, code_insee) {
  
  donneesElection <- dat %>% 
    filter(type_election %in% type_elections & year(as_datetime(date_evenement)) %in% annee_evenement & insee %in% code_insee) 
  
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


### GERER NSE dans GLUE
# as_string(ensym(x))
# deparse(substitute(expr = x))
# glue_data(
#   data,
#   glue("{[as_name(enquo(x))]}", .open = "[", .close = "]")
#   )


# https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
