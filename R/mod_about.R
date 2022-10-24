#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(



    mainPanel(
      tags$br(),tags$br(), tags$h4(strong("Developers")),
      tags$br(),
      "Moritz Meyer, Senior Economist, Poverty & Equity GP - World Bank",
      tags$br(),
      tags$br(),

      "Zeeshan Haider, Consultant, Poverty & Equity GP - World Bank",tags$br(),
      tags$br(),

      "Lander Bosch, Young Professional, Poverty & Equity GP - World Bank", tags$br(),

    )

  )
}

#' about Server Functions
#'
#' @noRd
mod_about_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
#

## To be copied in the server
# mod_about_server("about_1")
