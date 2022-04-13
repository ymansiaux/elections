graphique_resultats_election <- function(data, x, y, fill, 
                                         facet = TRUE, facet_var = NULL,
                                         theme_fun = NULL,
                                         title = "", subtitle = "", caption = "", 
                                         xlab = "", ylab = "", legend_name = "",
                                         scale_fill_function = NULL) {
  
  # browser()
  candidat <- as_string(ensym(fill))
  pct <- as_string(ensym(y))
  data$tooltip <- glue_data(
    data,
    glue("{[candidat]} {sprintf('%.2f',100*[pct])}%", .open = "[", .close = "]")
  )
  g <- data %>%
    # mutate(tooltip = glue_data(
    #   .,
    #   glue("{[candidat]} {sprintf('%.2f',100*[pct])}%", .open = "[", .close = "]")
    # )) %>% 
    ggplot(aes(x = as.factor({{x}}), y = {{y}}, fill = as.factor({{fill}}))) +
    geom_col_interactive(aes(data_id = {{x}}, tooltip = tooltip)) +
    scale_y_continuous(labels = scales::percent) 
  
  if(!is.null(scale_fill_function)) {
    
    g <- g +
      scale_fill_function
    
  } else {
    
    g <- g + 
      scale_fill_viridis(discrete = TRUE, direction = 1, begin = 0.2, end = 1)
    
  }
  
  
  if(!is.null(theme_fun)) {
    
    g <- g + theme_fun 
  }
  
  
  if(facet) {
    
    g <- g + facet_wrap(vars(as.factor({{facet_var}})), scales = "free_x") 
  }
  
  g +
    labs(title = title, subtitle = subtitle, caption = caption, fill = legend_name) +
    xlab(xlab) +
    ylab(ylab)
  
}