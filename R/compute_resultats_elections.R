compute_resultats_elections <- function(data, type = "participation", grouping_vars, truncate_results = FALSE, n_first_results = NULL) {
  
  if(type == "participation") {
    d <- data %>% 
      group_by(!!!syms(grouping_vars)) %>% 
      summarise(nb_voix = sum(nb_voix, na.rm = TRUE),
                nb_exprimes = sum(nb_exprimes, na.rm = TRUE)) %>% 
      mutate(pct = nb_voix / nb_exprimes) %>% 
      ungroup()
    
    if(truncate_results) {
      if("numero_tour" %in% colnames(d)) {
        d <- d %>% group_by(numero_tour)
      }
      
      d <- d %>% 
        slice_max(order_by = pct, n = n_first_results) %>% 
        ungroup()
    }
    
    d
    
    
  } else if(type == "abstention") {
    
    data %>% 
      group_by(!!!syms(grouping_vars)) %>% 
      summarise(nb_inscrits = sum(nb_inscrits, na.rm = TRUE),
                nb_votants = sum(nb_votants, na.rm = TRUE)) %>% 
      mutate(pct = 1 - (nb_votants / nb_inscrits)) %>% 
      ungroup()
  }
}
