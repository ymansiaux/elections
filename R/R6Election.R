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
    
    #' @field candidatsElection candidat avec palette de couleur
    candidatsElection = "",
    
    #' @field couleursCandidats palette de couleurs pour les candidats
    couleursCandidats = "",
    
    #' @field resultatsGlobauxCommune résultats globaux commune
    resultatsGlobauxCommune = "",
    
    #' @field resultatsAbstention résultats abstention commune
    resultatsAbstention = "",
    
    #' @field resultatsLV résultats par LV
    resultatsLV = "",
    
    #' @field resultatsBV résultats par BV
    resultatsBV = "",
    
    #' @field resultatsAbstentionBV résultats abstention BV
    resultatsAbstentionBV = "",
    
    #' @field resultatsAbstentionLV résultats abstention LV
    resultatsAbstentionLV = "",
    
    #' @field cartoBV fichier sf de localisation des bureaux de vote
    cartoBV = "",
    
    #' @field cartoLV fichier sf de localisation des lieux de vote
    cartoLV = "",
    
    #' @description
    #' Create a new occupation object.
    #' @param typeElection typeElection
    #' @param xtradataParameters xtradataParameters
    #' @param anneeElection anneeElection
    #' @param donneesElection donneesElection
    #' @param inseeElection inseeElection
    #' @param candidatsElection candidatsElection
    #' @param couleursCandidats couleursCandidats
    #' @param resultatsGlobauxCommune resultatsGlobauxCommune
    #' @param resultatsAbstention resultatsAbstention
    #' @param resultatsLV resultatsLV
    #' @param resultatsBV resultatsBV
    #' @param resultatsAbstentionBV resultatsAbstentionBV
    #' @param resultatsAbstentionLV resultatsAbstentionLV
    #' @param cartoBV cartoBV
    #' @param cartoLV cartoLV
    #' @return A new `Election` object.
    
    initialize = function(typeElection = NULL, xtradataParameters = NULL, anneeElection = NULL, donneesElection = NULL, 
                          inseeElection = NULL, candidatsElection = NULL, couleursCandidats = NULL,
                          resultatsGlobauxCommune = NULL, resultatsAbstention = NULL,
                          resultatsLV = NULL, resultatsBV = NULL, resultatsAbstentionBV = NULL, resultatsAbstentionLV = NULL,
                          cartoBV = NULL, cartoLV = NULL) {
      self$typeElection <- typeElection
      self$anneeElection <- anneeElection
      self$xtradataParameters <- xtradataParameters
      self$donneesElection <- donneesElection
      self$inseeElection <- inseeElection
      self$candidatsElection <- candidatsElection
      self$couleursCandidats <- couleursCandidats
      self$resultatsGlobauxCommune <- resultatsGlobauxCommune
      self$resultatsAbstention <- resultatsAbstention
      self$resultatsLV <- resultatsLV
      self$resultatsBV <- resultatsBV
      self$resultatsAbstentionBV <- resultatsAbstentionBV
      self$resultatsAbstentionLV <- resultatsAbstentionLV
      self$cartoBV <- cartoBV
      self$cartoLV <- cartoLV
      
    },
    
    #' @description
    #' Interroge le WS features
    download_data = function() {
      
      download_resultats <- try(xtradata_requete_features(
        key = Sys.getenv("XTRADATA_KEY"),
        typename = "EL_RESULTAT_A",
        filter = self$xtradataParameters,
        showURL = TRUE
      ))
      
      if (inherits(download_resultats, "try-error")) {
        self$donneesElection <- NULL
      } else {
        self$donneesElection <- download_resultats %>% 
          mutate(
            date_election = as_datetime(date_evenement, tz = "Europe/Paris"),
            annee_election = year(date_election)) %>%
          filter(annee_election == self$anneeElection) %>% 
          mutate(prenom = get_first_name(nom_candidat)) %>%
          mutate(nom = get_last_name(nom_candidat, prenom)) %>%
          mutate(nom = ifelse(nom == "", prenom, nom)) %>%
          mutate(nom_candidat_short = str_sub(nom_candidat,1,10)) %>%
          mutate(across(starts_with("nb_"), as.numeric)) %>%
          select(-date_evenement, -cdate, -mdate) %>%
          rename(nb_voix = valeur, code_insee = insee) %>%
          clean_names()
      }
      
      # ON NE RECUPERE LES DONNEES DE BV ET LV QUE POUR BORDEAUX POUR LE MOMENT
      if(self$xtradataParameters$insee == "33063" & !is.null(self$donneesElection)) {
        download_sf_bv <- try(xtradata_requete_features(
          key = Sys.getenv("XTRADATA_KEY"),
          typename = "EL_BUREAUVOTE_S",
          filter = list(insee = self$xtradataParameters$insee),
          backintime = self$donneesElection$date_election[1],
          showURL = TRUE
        ))
        
        download_sf_lv <- try(xtradata_requete_features(
          key = Sys.getenv("XTRADATA_KEY"),
          typename = "EL_LIEUVOTE_P",
          filter = list(insee = self$xtradataParameters$insee),
          backintime = self$donneesElection$date_election[1],
          showURL = TRUE
        ))
        
        if (!inherits(download_sf_bv, "try-error") & nrow(download_sf_bv)>0) self$cartoBV <- download_sf_bv
        if (!inherits(download_sf_lv, "try-error") & nrow(download_sf_lv)>0) self$cartoLV <- download_sf_lv
      }
      
    },
    
    #' @description
    #' Calcule les résultats globaux pour la commune
    #' @param donneesElection donnees issue du WS features
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
    
    #' @description
    #' Calcule les résultats d'abstention pour la commune
    #' @param donneesElection donnees issue du WS features
    compute_abstention_commune = function(donneesElection) {
      self$resultatsAbstention <- compute_resultats_elections(data = donneesElection,
                                                              type = "abstention",
                                                              grouping_vars = c(
                                                                "nom_election", "type_election", "annee_election", "numero_tour")) 
      
    },
    
    #' @description
    #' Calcule les résultats par BV
    #' @param donneesElection donnees issue du WS features
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
    
    #' @description
    #' Calcule les résultats par LV
    #' @param donneesElection donnees issue du WS features
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
    
    #' @description
    #' Calcule les résultats d'abstention par BV
    #' @param donneesElection donnees issue du WS features
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
    
    #' @description
    #' Calcule les résultats d'abstention par LV
    #' @param donneesElection donnees issue du WS features
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
    "insee" = unique(donneesElection$insee)#,
    # "date_evenement" = list("$in" = as.list(unique(donneesElection$date_evenement ))
    # )
  )
  
  Election$new(
    typeElection = type_elections,
    anneeElection = annee_evenement,
    xtradataParameters = filterXtradata,
    inseeElection = code_insee
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
