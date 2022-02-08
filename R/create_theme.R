
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

create_theme <- function() {
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