#' main_maps UI Function
#'
#' @description Main Maps on the landing page.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList  navbarPage div observeEvent reactive tabPanel sidebarLayout sidebarPanel
#' @importFrom shiny selectInput tags verbatimTextOutput absolutePanel req observe
#' @importFrom leaflet leaflet leafletOutput renderLeaflet addProviderTiles setView
#' @importFrom leaflet leafletOptions addPolygons labelOptions highlightOptions
#' @importFrom  leaflet tileOptions leafletProxy addLegend clearControls
#' @importFrom htmltools HTML tags
#' @importFrom glue glue
#' @importFrom dplyr filter
#' @import shiny
#'
#'
mod_main_maps_ui <- function(id){
  ns <- NS(id)
  tagList(

    # shiny::navbarPage(
    #
    #  title = "Pakistan Indicators",
    #
    #
    #
    # header=tags$style(HTML("
    #                                   .container-fluid{
    #                                     padding: 3px !important;
    #                                   }
    #
    #
    #                                   .navbar{
    #                                    margin-bottom: 0px !important;
    #                                    margin-left: 1px !important;
    #                                    margin-right: 1px !important;
    #                                    padding: 0px !important;
    #                                   }")),

    # shiny::tabPanel("INTERACTIVE MAPS",


                    # tags$head(tags$style(HTML("#main_map {height:100%, width:100%;}"))),


                    # width = "100%",
                    # height = "100%"


                    # br(),



tagList(
                    tags$style(type = "text/css", "#main_maps_1-main_map {height: calc(97vh - 100px) !important;}"),
                    # tags$style(type = 'text/css', '#id-main_map {height: calc(97vh - 100px) !important;}', style= 'padding:0px;'),
                    leaflet::leafletOutput(ns("main_map")),

                    shiny::verbatimTextOutput(ns("source_main")),
                    tags$head(tags$style("#main_maps_1-source_main {color:black; font-size:12px; font-style:italic;
                     overflow-y:scroll; max-height: 120px; background: #ffe6cc;}")),


                    shiny::absolutePanel(id = "controls", class = "panel panel-default", fixed= TRUE,
                                         draggable = TRUE, bottom = "auto", right = "auto", left = 70, top = 85,
                                         width = 230, height = "auto",
                                         style = "background-color: white;
                                                   opacity: 0.85;
                                                   padding: 20px 20px 20px 20px;
                                                   margin: auto;
                                                   border-radius: 5pt;
                                                   box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                                   padding-bottom: 2mm;
                                                   padding-top: 1mm;",
                                         tags$strong(tags$em(h5("Instructions: "))),
                                         tags$strong(tags$em(h6("First, Select the desired domain"))),
                                         tags$strong(tags$em(h6("The indicators available in the second tab will be updated based on the selected domain"))),
                                         tags$strong(tags$em(h6("Finally, select the required year and survey"))),
                                         # tags$strong(tags$em(h6("Note: Some indicators are available for multiple survey rounds for the same year"))),

                                         shiny::selectInput(ns("family"),
                                                            "Select Domain:",
                                                            choices = unique(Pak_Indicators_Data$domain),
                                                            selected = "Household Welfare"),

                                         shiny::selectInput(ns("stat"),
                                                            "Select Indicator: ",
                                                            choices = unique(Pak_Indicators_Data$indicator)),

                                         shiny::selectInput(ns("time"),
                                                            "Select Year:",
                                                            choices = unique(Pak_Indicators_Data$year_1),
                                                            # selected = median(Pak_Indicators_Data$year),
                                                            width = "100%",
                                                            multiple = FALSE)

                    )
    )
)
  # )
   # )
}

#' main_maps Server Functions
#'
#' @noRd
mod_main_maps_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #Lealfet static options
    output$main_map <- leaflet::renderLeaflet({
      # message("rendering local map")
      leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.20, zoomDelta = 0.20)) %>%
        leaflet::addProviderTiles(provider =  "CartoDB.Voyager", group = "CARTO") %>%
        leaflet::setView(lng=69.5, lat = 30, zoom = 5)
    })


    #Main Map
    #selecting domain
    dom_map <- shiny::reactive({
      # req(input$family)
      Pak_Indicators_Data %>%
        dplyr::filter(domain == input$family)
    })
    #
    #
    #Updating indicators based on seelcted domain
    shiny::observeEvent(dom_map(),{
      req(input$family)
      choices <- unique(dom_map()$indicator)
      shiny::updateSelectInput(
        session = getDefaultReactiveDomain(),
        "stat",
        choices = choices)
    })

    #Selected indicator
    ind_selected <- shiny::reactive({
      req(input$family)
      dom_map() %>%
        dplyr::filter(indicator == input$stat)
    })

    #Updating years based on selected indicator
    shiny::observeEvent(ind_selected(), {
      shiny::req(input$stat)
      updated_years_m <- ind_selected() %>%
        dplyr::filter(!is.na(value))
      shiny::updateSelectInput(
        session = getDefaultReactiveDomain(),
        "time",
        choices =sort(unique(updated_years_m$year_1), decreasing = T)
      )
    })

    #making a reactive function for dataset to be used based on User's selection
    map_data <- shiny::reactive({
      Pak_Indicators_Data %>%
        dplyr::filter(indicator == input$stat,
                      year_1 == input$time)
    })

    #Labelling for the Map
    labels_map <- shiny::reactive({
      paste0(glue::glue("<b>District</b>: { Pak_Shapfiles$district } </br>"),
             glue::glue("<b>Indicator: </b>"), " ",
             glue::glue("{ round(map_data()$value, 2) }"), " ", glue("{map_data()$units}"),
             sep = "") %>%
        lapply(htmltools::HTML)
    })

    #Color Scheme
    pal <- reactive ({
      leaflet::colorBin(palette =  c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c'),
                        bins= 5,
                        na.color = "grey",
                        domain = NULL,
                        map_data()[,"value"],
                        pretty = F,
                        reverse=F
      )

    })

    # Pal_legend
    pal_leg <- reactive ({
      leaflet::colorBin(palette = c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c'),
                        bins= 5,
                        na.color = "grey",
                        domain =(map_data()[,"value"]),
                        pretty = F,
                        reverse=F
      )
    })


    #Dynamic leaflet

    shiny::observeEvent(input$time,{

      # shiny::observe({
      #
      #   req(input$family)

      leaflet::leafletProxy("main_map",
                            data=Pak_Shapfiles,
                            deferUntilFlush = TRUE) %>%

        leaflet::clearShapes() %>%
        leaflet::addPolygons(label= labels_map(),
                             labelOptions = leaflet::labelOptions(
                               style = list("font-weight"= "normal",
                                            padding= "3px 8px",
                                            "color"= "black"),
                               textsize= "10px",
                               direction = "auto",
                               opacity = 0.9

                             ),
                             fillColor =  ~pal()(map_data()$value),
                             fillOpacity = 1,
                             stroke = TRUE,
                             color= "white",
                             weight = 1,
                             opacity = 0.9,
                             fill = TRUE,
                             dashArray = c(5,5),

                             smoothFactor = 0.8,
                             highlightOptions = leaflet::highlightOptions(weight= 2.5,
                                                                          color = "darkgrey",
                                                                          fillOpacity = 1,
                                                                          opacity= 1,
                                                                          bringToFront = TRUE),
                             group = "Polygons")


      leaflet::leafletProxy("main_map", data= map_data()) %>%
        leaflet::clearControls() %>%
        leaflet::addLegend("bottomright",
                           pal= pal_leg(),
                           values= map_data()$value,
                           title = "Indicator",
                           opacity= 1,
                           labFormat = leaflet::labelFormat(
                             between = " : ",
                             digits = 2))

    })

    # Message on updation of the PCA Model
    shiny::observe({
      req(input$time)
      shiny::showNotification("Map is being rendered based on the selection",
                              type="message",
                              duration = 3)
    })

  })
}



## To be copied in the UI
# mod_main_maps_ui("main_maps_1")

## To be copied in the server
# mod_main_maps_server("main_maps_1")
