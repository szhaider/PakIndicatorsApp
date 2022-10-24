#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom  shinythemes shinytheme
#' @noRd
app_ui <- function() {  #request
  tagList(
    # Leave this function for adding external resources
     golem_add_external_resources(),
    # Your application UI logic
     # fluidPage(
    tagList(

    shiny::navbarPage(
      title = "Pakistan Indicators",  #Navbar not show up without title
    # fluid = TRUE,
    # theme = shinythemes::shinytheme("journal"),  #This is nice
    # theme = shinythemes::shinytheme("yeti"),

    header=tags$style(HTML("
                                        .container-fluid{
                                          padding: 3px !important;
                                        }
                                         .navbar{
                                         margin-bottom: 0px !important;
                                         margin-left: 1px !important;
                                        }")),
    shiny::tabPanel(
      "INTERACTIVE MAPS",
      mod_main_maps_ui("main_maps_1")
                   ),
    shiny::tabPanel(
      "COMPARISON MAPS",
      mod_comparison_maps_ui("comparison_maps_1")
                  ),
    shiny::tabPanel(
      "GRAPHS",
      tabsetPanel(
    tabPanel(
      "BAR-CHARTS",
      mod_main_charts_ui("main_charts_1")
                    ),
    tabPanel(
      "SCATTER-PLOTS",
      mod_scatterplots_ui("scatterplots_1")
    ),
    shiny::tabPanel(
      "TIME-TREND",
      mod_time_series_charts_ui("time_series_charts_1")
    )
    )
    ),
    shiny::tabPanel(
    "TABLES",
    mod_main_tables_ui("main_tables_1")
    ),
    tabPanel("ABOUT",

    tabsetPanel(
      id="about_app",

    # tabPanel("ABOUT",
    # mod_about_ui("about_1")
    #          ),
    tabPanel("DOCUMENTATION",
    mod_document_ui("document_1")
    ),
    tabPanel("FEEDBACK",
    mod_feedback_ui("feedback_1")
    )
      )
        )
          )

            )
)

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {

addResourcePath('www', system.file('app/www', package = 'PakIndicatorsApp')
                )

tags$head(
  golem::activate_js(),
  golem::favicon(),
  tags$script(type="text/javascript", src = "wb_img.js")
)

  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "PakIndicatorsApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
