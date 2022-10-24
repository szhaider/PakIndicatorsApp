#' feedback UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_feedback_ui <- function(id){
  ns <- NS(id)
  tagList(

    mainPanel(
      br(),
      h4(strong("Please use the link below to register your valued feedback")),
      br(),
      tags$a(href="https://forms.office.com/Pages/DesignPageV2.aspx?subpage=design&token=00c72e08-32ea-4c7b-ac7b-30930d30b274&id=wP6iMWsmZ0y1bieW2PWcNpMxHt0laMlJu7t4ksK2dhVUOVJJUDI3SFRWOTFBRkVKOU5YRjhBV040Si4u",
             "Register Feedback",
             target="_blank")
    )

  )
}

#' feedback Server Functions
#'
#' @noRd
mod_feedback_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
#

## To be copied in the server
#
