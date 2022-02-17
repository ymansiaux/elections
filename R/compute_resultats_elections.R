compute_resultats_elections <- function(data, type = "participation", ...) {
  
  if(type == "participation") {

    data %>% 
      fgroup_by(...) %>% 
      fsummarise(nb_voix = fsum(nb_voix),
                nb_expr = fsum(nb_expr)) %>% 
      fmutate(pct = nb_voix / nb_expr)
    

  } else if(type == "abstention") {
    
    data %>% 
      fgroup_by(...) %>% 
      fsummarise(nb_inscrits = fsum(nb_inscrits),
                nb_votants = fsum(nb_votants)) %>% 
      fmutate(pct = nb_votants / nb_inscrits)
    
  }
}