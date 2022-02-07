#' observer_1_election UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import ggplot2
#' @importFrom scales percent
mod_observer_1_election_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    actionButton(ns("pause"), "pause"),
    
    selectizeInput(
      inputId = ns("type_elections"),
      label = "Type d'election",
      choices = NULL,
      multiple = FALSE,
      options = list(deselectBehavior = "top")
    ),
    
    selectizeInput(
      inputId = ns("annee_elections"),
      label = "Annee de l'election",
      choices = NULL,
      multiple = FALSE,
      options = list(deselectBehavior = "top")
    )
    # ,
    
    # radioButtons(inputId = ns("tour_election
    
  )
}

#' observer_1_election Server Functions
#'
#' @noRd 
mod_observer_1_election_server <- function(id, data_elections){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$pause, browser())
    
    ######
    type_elections <- reactive({
      req(data_elections$data)
      sort(
        unique(
          data_elections$data$type_election
        )
      )
    }) #%>% debounce(100)
    
    annee_elections <- reactive({
      req(data_elections$data)
      req(input$type_elections)
      
      sort(
        unique(
          unlist(
            data_elections$data[type_election %in% input$type_elections, "annee_election"])
        )
      )
    })# %>% debounce(1000)
    
    
    observe({
      updateSelectizeInput(session,
                           inputId = "type_elections",
                           choices = type_elections(),
                           server = TRUE
      )
    })
    
    observeEvent(input$type_elections, {
      updateSelectizeInput(session,
                           inputId = "annee_elections",
                           choices = annee_elections(),
                           server = TRUE
      )
    })
    
    election_selectionnee <- reactive({
      
      req(input$annee_elections)
      req(input$type_elections)
      
      data_elections$data[annee_election %in% input$annee_elections & type_election %in% input$type_elections]
      
    }) 
    
    results_by_tour_by_candidate <- reactive({
      
      election_selectionnee() %>% 
        .[, .(
          nb_voix = sum(nb_voix, na.rm = TRUE),
          nb_expr = sum(nb_expr, na.rm = TRUE)
        ),
          by = list(nom_election, type_election, annee_election, numero_tour, nom_candidat)
          ] %>% 
        .[, pct := nb_voix / nb_expr] %>% 
        .[, nom := get_last_name(nom_candidat)]
  
    })
    
    get_last_name <- function(full_name) {
      str_extract(pattern = "^[:alpha:]{1,}", string = full_name)
    }
    
    get_first_name <- function(full_name, last_name) {
      gsub(pattern = last_name, replacement = "", x = full_name) %>% gsub(pattern = " ", replacement = "", x = .)
    }
    
    # 
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
    
    my_font <- "Playfair Display"
    bg_color <- "white"
    
    sysfonts::font_add_google(name = my_font, family = my_font)
    showtext::showtext_auto()
    
    title_color <- "#008acd"
    subtitle_color <- "#aaaaaa"
    axis_tick_color <- rgb(51, 51, 51, maxColorValue = 255)
    axis_text_color <- axis_tick_color
    
    theme(text = element_text(family = my_font),
          axis.title = element_blank(),
          axis.text = element_text(color = axis_text_color),
          axis.text.x = element_text(size = 11),
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
          plot.caption.position = "plot",
          legend.position = "none")
    
    results_by_tour_by_candidate() %>%
      ggplot(aes(x = as.factor(nom), y = pct, fill = as.factor(nom_candidat))) +
      geom_col() +
      scale_y_continuous(labels = scales::percent) +
      facet_wrap(vars(numero_tour), scales = "free") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      scale_fill_manual(values = palette)
    
    # pivoter les axes des x
    # faire un joli theme
    # changer les couleurs
    # interactif
    
  })
}

## To be copied in the UI
# mod_observer_1_election_ui("observer_1_election_ui_1")

## To be copied in the server
# mod_observer_1_election_server("observer_1_election_ui_1")
