#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_main_maps_server("main_maps_1")
  mod_comparison_maps_server("comparison_maps_1")
  mod_main_charts_server("main_charts_1")
  mod_time_series_charts_server("time_series_charts_1")
  mod_main_tables_server("main_tables_1")
}
