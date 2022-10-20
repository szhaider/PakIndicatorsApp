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
#' @importFrom shinyscreenshot screenshot
#'
mod_main_maps_ui <- function(id){
  ns <- NS(id)
  tagList(

tagList(
                    tags$style(type = "text/css", "#main_maps_1-main_map {height: calc(97vh - 100px) !important;}"),
                    leaflet::leafletOutput(ns("main_map")),

                    shiny::verbatimTextOutput(ns("source_main_map")),
                    tags$head(tags$style("#main_maps_1-source_main_map {color:black; font-size:12px; font-style:italic;
                     max-height: 120px; background: #ffe6cc;}")),

                    shiny::absolutePanel(id = "controls_bin", class = "panel panel-default", fixed= TRUE,
                                         draggable = FALSE,  right = 15, top = 65,
                                         width = 250, height = "auto",
                                         style = "background-color: white;
                                                   opacity: 0.85;
                                                   padding: 20px 20px 20px 20px;
                                                   margin: auto;
                                                   border-radius: 5pt;
                                                   box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                                   padding-bottom: 2mm;
                                                   padding-top: 1mm;",

                                         shiny::numericInput(ns("bins"),
                                                             label = "Choose number of Bins",
                                                             min = 3,
                                                             max= 10,
                                                             value = 5,
                                                             step=1)
                    ),


                    shiny::absolutePanel(id = "controls", class = "panel panel-default", fixed= TRUE,
                                         draggable = TRUE, bottom = "auto", right = "auto", left = 70, top = 85,
                                         width = 260, height = "auto",
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
                                                            selected = "Household Welfare"
                                                            ),

                                         shiny::selectInput(ns("stat"),
                                                            "Select Indicator: ",
                                                            choices = unique(Pak_Indicators_Data$indicator)),

                                         shiny::selectInput(ns("time"),
                                                            "Select Year:",
                                                            choices = unique(Pak_Indicators_Data$year_1),
                                                            # selected = median(Pak_Indicators_Data$year),
                                                            width = "100%",
                                                            multiple = FALSE,

                                                            ),

                                         downloadButton(ns("mapdata"), "Data", class= "btn-sm"),
                                         actionButton(ns("screenshot"), "Image",class="btn-sm", icon=icon("camera")),
                                         actionButton(ns("help_map"), "Help", icon= icon('question-circle'), class ="btn-sm"),
                                         br(),

                    )
    )
)

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
        dplyr::filter(!is.na(value))    #<<<<<<<<<<
      shiny::updateSelectInput(
        session = getDefaultReactiveDomain(),
        "time",
        choices =sort(unique(updated_years_m$year_1), decreasing = T)
      )
    })

    #making a reactive function for dataset to be used based on User's selection
    map_data <- shiny::reactive({
      Pak_Indicators_Data %>%
        dplyr::filter(
                      domain == input$family,
                      indicator == input$stat,
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
    pal_new <- reactive({
      req(unique(map_data()$context) %in% c("negative", "positive"))
      if (unique(map_data()$context) == "negative"){
        rev(grDevices::colorRampPalette(colors = c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c'), space = "Lab")(input$bins))
      } else {
        grDevices::colorRampPalette(colors = c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c'), space = "Lab")(input$bins)
      }
    })

    #breaks defined
    breaks <- reactive({
      req(unique(map_data()$context) %in% c("negative", "positive"))
        stats::quantile(map_data()$value, seq(0, 1, 1 / (input$bins)), na.rm = TRUE) %>%
          unique()
    })

    pal <- reactive ({
      leaflet::colorBin(palette = pal_new(),
               bins= breaks(),
               na.color = "grey",
               domain = NULL,
               pretty = F,
               reverse=T)
    })

    # Pal_legend
    pal_leg <- reactive ({
      leaflet::colorBin(palette = pal_new(),
               bins= breaks(),
               na.color = "grey",
               domain = map_data()[,"value"],
               pretty = FALSE,
               reverse=T
      )
    })

    #Dynamic leaflet
    # shiny::observeEvent(input$time,{

       shiny::observe({

      req(map_data())

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
                          title =
                          if(unique(map_data()$units)!=""){
                          paste0("Indicator", " ","(", unique(map_data()$units), ")")
                           }else{
                           "Indicator"
                           },
                           opacity= 1,
                           labFormat = leaflet::labelFormat(
                             between = " : ",
                             digits = 2))

    })

    # Message on updation of the MAPS
    shiny::observe({
      req(input$time)
      shiny::showNotification("Map is being rendered based on the selection",
                              type="message",
                              duration = 3)
    })

    #Source of the slected indicator
    output$source_main_map <- shiny::renderText({
      paste(" Source: ", glue("{ unique(map_data()$source) }"),
            "\n",
            "Definition: ", glue("{ unique(map_data()$definition) }"))
    })

    #Screenshot
    shiny::observeEvent(input$screenshot,{
      shinyscreenshot::screenshot(filename = glue::glue("{ input$stat }", "_", { "input$time" }),
                                  id = "main_map", scale = 0.90, timer = 1)
    })

    #Download data underlying the shown map
    output$mapdata <- shiny::downloadHandler(
      filename = function(){
        paste(glue::glue("{ input$stat }"), "_", glue::glue("{ input$time }"), ".csv")
      },
      content = function(file){
        write.csv(map_data() %>% dplyr::select(-context, -positive, -negative, district1), file)
      }
    )

#Main Interactive Maps
    ##############################################.
    #### Modal  ----
    ###############################################.
    shiny::observeEvent(input$help_map, {
      shiny::showModal(modalDialog(
        title = "How to use these maps",
        p("These maps give district (admin2) level estimates of various socioeconomic indicators over the selected filters"),
        p("All indicators are rounded to 2 decimal points"),
        # p("All Natural Hazards Indicators are rounded to 3 decimal points"),
        p("Expect the color mapping to reverse with the context of  the selected indicators - e.g. Poverty (High) = Red whereas; Access to improved toilet facilities (High) = Blue"),
        size = "m", easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
    })


  })



}



## To be copied in the UI
# mod_main_maps_ui("main_maps_1")

## To be copied in the server
# mod_main_maps_server("main_maps_1")
