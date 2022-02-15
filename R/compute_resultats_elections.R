compute_resultats_elections <- function(data, type = "participation", ...) {
  
  if(type == "participation") {

    data %>% 
      group_by(...) %>% 
      summarise(nb_voix = fsum(nb_voix),
                nb_expr = fsum(nb_expr)) %>% 
      mutate(pct = nb_voix / nb_expr)
    

  } else if(type == "abstention") {
    
    data %>% 
      group_by(...) %>% 
      summarise(nb_inscrits = fsum(nb_inscrits),
                nb_votants = fsum(nb_votants)) %>% 
      mutate(pct = nb_votants / nb_inscrits)
    
  }
}