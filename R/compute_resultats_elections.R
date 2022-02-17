compute_resultats_elections <- function(data, type = "participation", grouping_vars) {
  
  if(type == "participation") {
    
    data %>% 
      group_by(!!!syms(grouping_vars)) %>% 
      summarise(nb_voix = sum(nb_voix, na.rm = TRUE),
                nb_expr = sum(nb_expr, na.rm = TRUE)) %>% 
      mutate(pct = nb_voix / nb_expr) %>% 
      ungroup()
    
    
  } else if(type == "abstention") {
    
    data %>% 
      group_by(!!!syms(grouping_vars)) %>% 
      summarise(nb_inscrits = sum(nb_inscrits, na.rm = TRUE),
                nb_votants = sum(nb_votants, na.rm = TRUE)) %>% 
      mutate(pct = nb_votants / nb_inscrits) %>% 
      ungroup()
  }
}
