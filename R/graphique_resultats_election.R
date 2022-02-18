graphique_resultats_election <- function(data, x, y, fill, facet = TRUE, facet_var = NULL) {
  
  g <- data %>%
    ggplot(aes(x = as.factor({{x}}), y = {{y}}, fill = as.factor({{fill}}))) +
    geom_col() +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_viridis(discrete = TRUE) +
    create_theme()
  
  # if(any(!is.na(data$numero_tour))) {
  # if((!is.null(facet_var))) {
  #   g <- g + facet_wrap(vars({{facet_var}}), scales = "free") 
  # }
  if(facet) {
    
    g <- g + facet_wrap(vars(as.factor({{facet_var}})), scales = "free") 
  }
  # 
  g
  
}