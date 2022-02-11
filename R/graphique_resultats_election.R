graphique_resultats_election <- function(data, x, y, fill) {
  
  g <- data %>%
    ggplot(aes(x = as.factor(get(x)), y = get(y), fill = as.factor(get(fill)))) +
    geom_col() +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_viridis(discrete = TRUE) +
    create_theme()
  
  if(any(!is.na(data$numero_tour))) {
    g <- g + facet_wrap(vars(numero_tour), scales = "free") 
  }
  
  g
  
}