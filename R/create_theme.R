
palette <-  c(
  "#2ec7c9",
  "#b6a2de",
  "#5ab1ef",
  "#ffb980",
  "#d87a80",
  "#8d98b3",
  "#e5cf0d",
  "#97b552",
  "#95706d",
  "#dc69aa",
  "#07a2a4",
  "#9a7fd1",
  "#588dd5",
  "#f5994e",
  "#c05050",
  "#59678c",
  "#c9ab00",
  "#7eb00a",
  "#6f5553",
  "#c14089"
)

theme_elections <- function() {
  my_font <- "Playfair Display"
  bg_color <- "white"
  
  
  title_color <- "#008acd"
  subtitle_color <- "#aaaaaa"
  axis_tick_color <- rgb(51, 51, 51, maxColorValue = 255)
  axis_text_color <- axis_tick_color
  
  theme(text = element_text(family = my_font),
        axis.title = element_blank(),
        axis.text = element_text(color = axis_text_color),
        axis.text.x = element_text(size = 11, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 11),
        axis.ticks = element_line(color = subtitle_color, size = .5),
        axis.ticks.length.x = unit(.7, "lines"),
        axis.ticks.length.y = unit(.7, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(20, 0, 20, 0),
        plot.background = element_rect(fill = bg_color, color =  bg_color),
        panel.background = element_rect(fill = bg_color, color =  bg_color),
        plot.title = element_text(color = title_color, size = 18, family = my_font),
        plot.subtitle = element_markdown(color = subtitle_color, size = 13),
        plot.title.position = "plot",
        plot.caption.position = "plot")
  # ,
  #       legend.position = "none")
}

theme_bdxmetro_dark_mod <- function(regular_font_family = "Roboto", light_font_family = "Roboto Light", 
                                    main_text_color = "white", secondary_text_color = "#9f9f9f", 
                                    plot_title_family = light_font_family, plot_title_color = main_text_color, 
                                    plot_title_size = 20, plot_subtitle_family = light_font_family, 
                                    plot_subtitle_color = main_text_color, plot_subtitle_size = 12, 
                                    axis_title_family = regular_font_family, axis_title_color = main_text_color, 
                                    axis_title_size = 11, axis_text_family = regular_font_family, 
                                    axis_text_color = secondary_text_color, axis_text_size = 9, 
                                    legend_title_family = regular_font_family, legend_title_color = main_text_color, 
                                    legend_title_size = 11, legend_text_family = regular_font_family, 
                                    legend_text_color = main_text_color, legend_text_size = 9, 
                                    legend_background_color = "#101010", legend_key_color = "#101010", 
                                    facet_text_family = regular_font_family, facet_text_color = "white", 
                                    facet_text_size = 12, facet_background = "#101010", facet_background_border = secondary_text_color, 
                                    plot_caption_family = regular_font_family, plot_caption_color = secondary_text_color, 
                                    plot_caption_size = 9, panel_background_color = "#101010", 
                                    panel_grid_color = "#292929", plot_background_color = "#101010", 
                                    plot_background_border_color = "#101010", ...) {
  
  theme(plot.title = element_text(family = plot_title_family, 
                                  color = plot_title_color, size = plot_title_size), 
        
        plot.subtitle = element_text(family = plot_subtitle_family, 
                                     color = plot_subtitle_color, size = plot_subtitle_size), 
        
        axis.title = element_text(family = axis_title_family, 
                                  color = axis_title_color, size = axis_title_size), 
        
        axis.text = element_text(family = axis_text_family, color = axis_text_color, 
                                 size = axis_text_size), 
        
        axis.ticks = element_blank(), 
        
        legend.title = element_text(family = legend_title_family, 
                                    color = legend_title_color, size = legend_title_size), 
        
        legend.text = element_text(family = legend_text_family, 
                                   color = legend_text_color, size = legend_text_size), 
        
        legend.background = element_rect(fill = legend_background_color), 
        
        legend.key = element_rect(color = legend_key_color, fill = legend_key_color), 
        
        strip.text = element_text(family = facet_text_family, 
                                  color = facet_text_color, size = facet_text_size), 
        
        strip.background = element_rect(fill = facet_background, 
                                        color = facet_background_border), 
        
        plot.caption = element_text(family = plot_caption_family, 
                                    color = plot_caption_color, size = plot_caption_size),
        
        panel.background = element_rect(fill = panel_background_color), 
        
        panel.grid = element_line(color = panel_grid_color), 
        
        plot.background = element_rect(fill = plot_background_color, 
                                       color = plot_background_border_color),
        ...)
}
