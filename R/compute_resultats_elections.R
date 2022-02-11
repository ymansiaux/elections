compute_resultats_elections <- function(data, type = "participation", group_by_vars) {
  
  if(type == "participation") {
    
    copy(data) %>% 
      .[, .(
        nb_voix = sum(nb_voix, na.rm = TRUE),
        nb_expr = sum(nb_expr, na.rm = TRUE)
      ),
      by = group_by_vars
      ] %>% 
      .[, pct := nb_voix / nb_expr] 
    
  } else if(type == "abstention") {
    
    copy(data) %>% 
      .[, .(
        nb_inscrits = sum(nb_inscrits, na.rm = TRUE),
        nb_votants = sum(nb_votants, na.rm = TRUE)
      ),
      by = group_by_vars
      ] %>% 
      .[, pct := 1- nb_votants / nb_inscrits] 
  }
}
