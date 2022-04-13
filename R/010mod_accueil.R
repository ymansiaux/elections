#' accueil UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_accueil_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    # https://mdbootstrap.com/docs/standard/extended/gallery/
    fluidRow(
      column(width = 12,
             
             tags$div(
               id = "carouselMultiItemExample",
               class = "carousel slide carousel-dark text-center",
               `data-mdb-ride` = "carousel",
               
               tags$div(
                 class = "carousel-inner py-4",
                 
                 tags$div(
                   class = "carousel-item active",
                   
                   tags$div(
                     class = "container",
                     
                     tags$div(
                       class = "row",
                       
                  tags$div(
                         class = "col-4 d-none d-sm-block",
                         style = "min-height: 47vh;",                         
                         
                         tags$div(
                           class = "card card-block ",
                           style = "height: 100%; display:flex; flex-direction:column;",

                           tags$div(
                             style = "
                             background:black; 
                             flex:1;
                             background-image: url(www/img/vote1.jpg);
                             background-repeat: no-repeat;
                             background-size: cover;
                             "),                        
                           
                           tags$div(style = "flex:1;",
                               
                               tags$div(
                                 class = "card-body card_content_flex_style",
                               
                               tags$div(class = "card_title_flex_style",
                                        tags$h5(
                                          class = "card-title",
                                          "R\u00e9sultats globaux"
                                        )
                               ),
                               
                               tags$div(
                                 class = "card_text_flex_style",
                                 tags$p(
                                   class = "card-text",
                                   "Observer les r\u00e9sultats complets d\'une \u00e9lection : voix obtenues pour chaque liste ou candidat, abstention"
                                 )),
                               
                               tags$div(
                                 class = "card_button_flex_style",
                                 tags$a(
                                   href = "#!",
                                   class = "btn btn-primary",
                                   "Go",
                                   onclick="$(\'li:eq(1) a\').tab(\'show\');"
                                 )
                               )
                             )
                           )
                         )
                         
                       ),
                       
                    tags$div(
                         class = "col-4 d-none d-sm-block",
                         style = "min-height: 47vh;",
                         
                         
                         tags$div(
                           class = "card card-block ",
                           style = "height: 100%; display:flex; flex-direction:column;",                       
                          
                           tags$div(
                             style = "
                             background:black; 
                             flex:1;
                             background-image: url(www/img/vote2.jpg);
                             background-repeat: no-repeat;
                             background-size: cover;
                             "),                        
                           
                           tags$div(style = "flex:1;",
                               
                               tags$div(
                                 class = "card-body card_content_flex_style",
                               
                               tags$div(class = "card_title_flex_style",
                                        tags$h5(
                                          class = "card-title",
                                          "R\u00e9sultats pour un lieu de vote"
                                        )
                               ),
                               
                               tags$div(
                                 class = "card_text_flex_style",
                                 tags$p(
                                   class = "card-text",
                                    "R\u00e9sultats d\u00e9taill\u00e9s pour un lieu de vote et les bureaux qui y sont rattach\u00e9s"
                                 )),
                               
                               tags$div(
                                 class = "card_button_flex_style",
                                 tags$a(
                                   href = "#!",
                                   class = "btn btn-primary",
                                   "Go",
                                   onclick="$(\'li:eq(2) a\').tab(\'show\');"
                                 )
                               )
                             )
                           )
                         )
                         
                       ),
                       
                       tags$div(
                         class = "col-4 d-none d-sm-block",
                         style = "min-height: 47vh;",
                         
                         
                         tags$div(
                           class = "card card-block ",
                           style = "height: 100%; display:flex; flex-direction:column;",

                           tags$div(
                             style = "
                             background:black; 
                             flex:1;
                             background-image: url(www/img/vote3.jpg);
                             background-repeat: no-repeat;
                             background-size: cover;
                             "),                        
                           
                           tags$div(style = "flex:1;",
                               
                               tags$div(
                                 class = "card-body card_content_flex_style",
                                 
                                 tags$div(class = "card_title_flex_style",
                                          tags$h5(
                                            class = "card-title",
                                            "R\u00e9sultats d\'un candidat"
                                          )
                                 ),
                                 
                                 tags$div(
                                   class = "card_text_flex_style",
                                   tags$p(
                                     class = "card-text",
                                     "R\u00e9sultats d\u00e9taill\u00e9s d\'un candidat dans les diff\u00e9rents lieux et bureaux de vote"
                                   )),
                                 
                                 tags$div(
                                   class = "card_button_flex_style",
                                   tags$a(
                                     href = "#!",
                                     class = "btn btn-primary",
                                     "Go",
                                     onclick="$(\'li:eq(3) a\').tab(\'show\');"
                                   )
                                 )
                               )
                           )
                         )                         
                       )
                     )
                   )
                 )
               )
             )
      )
    )      
  )
  
}


#' accueil Server Functions
#'
#' @noRd 
mod_accueil_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
  })
}

## To be copied in the UI
# mod_accueil_ui("accueil_ui_1")

## To be copied in the server
# mod_accueil_server("accueil_ui_1")
