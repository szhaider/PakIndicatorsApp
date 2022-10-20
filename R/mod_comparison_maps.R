#' comparison_maps UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import leaflet
#' @import shiny
mod_comparison_maps_ui <- function(id){
  ns <- NS(id)
  tagList(
    tagList(

      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width = 3,
          style = "background-color: white;
                               opacity: 0.85;
                               padding: 20px 20px 20px 20px;

                               border-radius: 5pt;
                               box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                               padding-bottom: 2mm;
                               padding-top: 1mm;",

          shiny::selectInput(ns("domain_map1"),
                             "Choose Domain for MAP1",
                             choices = unique(Pak_Indicators_Data$domain),
                             selected = "Household Welfare"),
          shiny::selectInput(ns("indicator_map1"),
                             "Choose Indicator for MAP1",
                             choices = unique(Pak_Indicators_Data$indicator)),

          shiny::selectInput(ns("year_map1"),
                             "Choose Year for MAP1",
                             choices = unique(Pak_Indicators_Data$year_1),
                             # selected = ,
          ),

          # hr(),
          shiny::selectInput(ns("domain_map2"),
                             "Choose Domain for MAP2",
                             choices = unique(Pak_Indicators_Data$domain),
                             selected = "Household Welfare"),

          shiny::selectInput(ns("indicator_map2"),
                             "Choose Indicator for MAP2",
                             choices = unique(Pak_Indicators_Data$indicator)),

          shiny::selectInput(ns("year_map2"),
                             "Choose Year for MAP2",
                             choices = unique(Pak_Indicators_Data$year_1),
                             # selected = ,
          ),

          shiny::actionButton(ns("help_comp"),
                              "Help",
                              icon= icon('question-circle'),
                              class ="btn-sm"),
        ),

        shiny::mainPanel(
          width = 9,
          shiny::fluidRow(
            shiny::column(width = 6,
                          offset = 0,
                          style =
                            'padding-bottom:0px;
                                       padding-left:0px;
                                       padding-right:0px;
                                       margin-left:-10px;
                                       position: relative;',
                          tags$style(type = 'text/css', '#comparison_maps_1-double_map_1 {height: calc(85vh - 50px) !important;}'),
                          leaflet::leafletOutput(ns("double_map_1"), width = "100%", height = "400px"),

            ),
            shiny::column(width = 6,
                          offset = 0,
                          style =
                            'padding-bottom:0px;
                                       padding-left:2px;
                                       padding-right:2px;
                                       margin-left:0px;
                                       margin-right:5px;
                                       position: relative;',
                          tags$style(type = 'text/css', '#comparison_maps_1-double_map_2 {height: calc(85vh - 50px) !important;}'),
                          leaflet::leafletOutput(ns("double_map_2"), width = "100%", height = "400px")

            )),

          shiny::fluidRow(
            shiny::column(6,
                          offset = 0,
                          style =
                            "padding-top:1px;
                                       padding-bottom:0px;
                                        padding-left:0px;
                                       padding-right:0px;
                                       position: relative;",
                          tags$head(tags$style(
                            "#comparison_maps_1-source_comp1{color:black;
                                       margin-left:-10px;
                                       font-size:12px; font-style:italic; max-height: 110px; background: #ffe6cc; }")),
                          tags$style(type = 'text/css', '#comparison_maps_1-source_comp1 {height: calc(50vh - 50px) !important;}'),
                          shiny::verbatimTextOutput(ns("source_comp1")),

            ),
            shiny::column(6,
                          offset = 0,
                          style =
                            'padding-bottom:0px;
                                    padding-top:1px;
                                     padding-left:2px;
                                     padding-right:2px;

                                     margin-left:0px;
                                     position: relative;',
                          tags$head(tags$style(
                            "#comparison_maps_1-source_comp2{color:black;
                                       font-size:12px; font-style:italic; max-height: 110px; background: #ffe6cc; }")),
                          tags$style(type = 'text/css', '#comparison_maps_1-source_comp2 {height: calc(50vh - 50px) !important;}'),
                          shiny::verbatimTextOutput(ns("source_comp2"))

            ))
        )
      )
    )

  )
}

#' comparison_maps Server Functions
#'
#' @noRd
mod_comparison_maps_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #Lealfet static options comp map 1
    output$double_map_1 <- leaflet::renderLeaflet({
      # message("rendering local map")
      leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.20, zoomDelta = 0.20)) %>%
        leaflet::addProviderTiles(provider =  "CartoDB.Voyager", group = "CARTO") %>%
        leaflet::setView(lng=69.5, lat = 30, zoom = 5)%>%
        leaflet.minicharts::syncWith("combined_map")
    })

    #Lealfet static options comp map 2
    output$double_map_2 <- leaflet::renderLeaflet({
      # message("rendering local map")
      leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.20, zoomDelta = 0.20)) %>%
        leaflet::addProviderTiles(provider =  "CartoDB.Voyager", group = "CARTO") %>%
        leaflet::setView(lng=69.5, lat = 30, zoom = 5)%>%
        leaflet.minicharts::syncWith("combined_map")
    })

    #Comp Map 1
    #selecting domain
    dom_map_1 <- shiny::reactive({
      # req(input$family)
      Pak_Indicators_Data %>%
        dplyr::filter(domain == input$domain_map1)
    })

    #Comp Map 2
    #selecting domain
    dom_map_2 <- shiny::reactive({
      # req(input$family)
      Pak_Indicators_Data %>%
        dplyr::filter(domain == input$domain_map2)
    })

    #Updating indicators based on seelcted domain - com map 1
    shiny::observeEvent(dom_map_1(),{
      req(input$domain_map1)
      choices_1 <- unique(dom_map_1()$indicator)
      shiny::updateSelectInput(
        session = getDefaultReactiveDomain(),
        "indicator_map1",
        choices = choices_1)
    })

    #Updating indicators based on seelcted domain - com map 2
    shiny::observeEvent(dom_map_2(),{
      req(input$domain_map2)
      choices_2 <- unique(dom_map_2()$indicator)
      shiny::updateSelectInput(
        session = getDefaultReactiveDomain(),
        "indicator_map2",
        choices = choices_2)
    })


    #Selected indicator - com map 1
    ind_selected_1 <- shiny::reactive({
      req(input$domain_map1)
      dom_map_1() %>%
        dplyr::filter(indicator == input$indicator_map1)
    })

    #Selected indicator - com map 2
    ind_selected_2 <- shiny::reactive({
      req(input$domain_map2)
      dom_map_2() %>%
        dplyr::filter(indicator == input$indicator_map2)
    })

    #Updating years based on selected indicator - comp map1
    shiny::observeEvent(ind_selected_1(), {
      shiny::req(input$indicator_map1)
      updated_years_m_1 <- ind_selected_1() %>%
        dplyr::filter(!is.na(value))    #<<<<<<<<<<
      shiny::updateSelectInput(
        session = getDefaultReactiveDomain(),
        "year_map1",
        choices =sort(unique(updated_years_m_1$year_1), decreasing = T)
      )
    })

    #Updating years based on selected indicator - comp map2
    shiny::observeEvent(ind_selected_2(), {
      shiny::req(input$indicator_map2)
      updated_years_m_2 <- ind_selected_2() %>%
        dplyr::filter(!is.na(value))    #<<<<<<<<<<
      shiny::updateSelectInput(
        session = getDefaultReactiveDomain(),
        "year_map2",
        choices =sort(unique(updated_years_m_2$year_1), decreasing = T)
      )
    })

    #making a reactive function for dataset to be used based on User's selection - Map 1
    map_data_1 <- shiny::reactive({
      Pak_Indicators_Data %>%
        dplyr::filter(
          domain == input$domain_map1,
          indicator == input$indicator_map1,
          year_1 == input$year_map1)
    })

    #making a reactive function for dataset to be used based on User's selection - Map 2
    map_data_2 <- shiny::reactive({
      Pak_Indicators_Data %>%
        dplyr::filter(
          domain == input$domain_map2,
          indicator == input$indicator_map2,
          year_1 == input$year_map2)
    })

    #Labelling for the Map 1
    labels_map_1 <- shiny::reactive({
      paste0(glue::glue("<b>District</b>: { Pak_Shapfiles$district } </br>"),
             glue::glue("<b>Indicator: </b>"), " ",
             glue::glue("{ round(map_data_1()$value, 2) }"), " ", glue("{map_data_1()$units}"),
             sep = "") %>%
        lapply(htmltools::HTML)
    })

    #Labelling for the Map 2
    labels_map_2 <- shiny::reactive({
      paste0(glue::glue("<b>District</b>: { Pak_Shapfiles$district } </br>"),
             glue::glue("<b>Indicator: </b>"), " ",
             glue::glue("{ round(map_data_2()$value, 2) }"), " ", glue("{map_data_2()$units}"),
             sep = "") %>%
        lapply(htmltools::HTML)
    })

    #Color Scheme - Map 1
    pal_new_1 <- reactive({
      req(unique(map_data_1()$context) %in% c("negative", "positive"))
      if (unique(map_data_1()$context) == "negative"){
        rev(grDevices::colorRampPalette(colors = c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c'), space = "Lab")(5))
      } else {
        grDevices::colorRampPalette(colors = c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c'), space = "Lab")(5)
      }
    })

    #Color Scheme - Map2
    pal_new_2 <- reactive({
      req(unique(map_data_2()$context) %in% c("negative", "positive"))
      if (unique(map_data_2()$context) == "negative"){
        rev(grDevices::colorRampPalette(colors = c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c'), space = "Lab")(5))
      } else {
        grDevices::colorRampPalette(colors = c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c'), space = "Lab")(5)
      }
    })

    # Palette_map1
    pal_1 <- reactive ({
      leaflet::colorBin(palette = pal_new_1(),
                        bins= 5,
                        na.color = "grey",
                        domain = NULL,
                        pretty = F,
                        reverse=T)
    })

    # Palette_map12
    pal_2 <- reactive ({
      leaflet::colorBin(palette = pal_new_2(),
                        bins= 5,
                        na.color = "grey",
                        domain = NULL,
                        pretty = F,
                        reverse=T)
    })


    # Pal_legend - map1
    pal_leg_1 <- reactive ({
      leaflet::colorBin(palette = pal_new_1(),
                        bins= 5,
                        na.color = "grey",
                        domain = map_data_1()[,"value"],
                        pretty = FALSE,
                        reverse=T
      )
    })

    # Pal_legend - map2
    pal_leg_2 <- reactive ({
      leaflet::colorBin(palette = pal_new_2(),
                        bins= 5,
                        na.color = "grey",
                        domain = map_data_2()[,"value"],
                        pretty = FALSE,
                        reverse=T
      )
    })

    #Dynamic leaflet Map 1

    shiny::observe({

      req(map_data_1())

      leaflet::leafletProxy("double_map_1",
                            data=Pak_Shapfiles,
                            deferUntilFlush = FALSE) %>%

        leaflet::clearShapes() %>%
        leaflet::addPolygons(label= labels_map_1(),
                             labelOptions = leaflet::labelOptions(
                               style = list("font-weight"= "normal",
                                            padding= "3px 8px",
                                            "color"= "black"),
                               textsize= "10px",
                               direction = "auto",
                               opacity = 0.9

                             ),
                             fillColor =  ~pal_1()(map_data_1()$value),
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


      leaflet::leafletProxy("double_map_1", data= map_data_1()) %>%
        leaflet::clearControls() %>%
        leaflet::addLegend("bottomright",
                           pal= pal_leg_1(),
                           values= map_data_1()$value,
                           title =
                             if(unique(map_data_1()$units)!=""){
                               paste0("Indicator", " ","(", unique(map_data_1()$units), ")")
                             }else{
                               "Indicator"
                             },
                           opacity= 1,
                           labFormat = leaflet::labelFormat(
                             between = " : ",
                             digits = 2))

    })

    # Message on updation of the MAP 1
    shiny::observe({
      req(input$year_map1)
      shiny::showNotification("Map is being rendered based on the selection",
                              type="message",
                              duration = 3)
    })

    #Dynamic leaflet Map2

    shiny::observe({

      req(map_data_2())

      leaflet::leafletProxy("double_map_2",
                            data=Pak_Shapfiles,
                            deferUntilFlush = FALSE) %>%

        leaflet::clearShapes() %>%
        leaflet::addPolygons(label= labels_map_2(),
                             labelOptions = leaflet::labelOptions(
                               style = list("font-weight"= "normal",
                                            padding= "3px 8px",
                                            "color"= "black"),
                               textsize= "10px",
                               direction = "auto",
                               opacity = 0.9

                             ),
                             fillColor =  ~pal_2()(map_data_2()$value),
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


      leaflet::leafletProxy("double_map_2", data= map_data_2()) %>%
        leaflet::clearControls() %>%
        leaflet::addLegend("bottomright",
                           pal= pal_leg_2(),
                           values= map_data_2()$value,
                           title =
                             if(unique(map_data_2()$units)!=""){
                               paste0("Indicator", " ","(", unique(map_data_2()$units), ")")
                             }else{
                               "Indicator"
                             },
                           opacity= 1,
                           labFormat = leaflet::labelFormat(
                             between = " : ",
                             digits = 2))

    })

    # Message on updation of the MAP 2
    shiny::observe({
      req(input$year_map2)
      shiny::showNotification("Map is being rendered based on the selection",
                              type="message",
                              duration = 3)
    })

    #Source of the slected indicator Map1
    output$source_comp1 <- shiny::renderText({
      paste(" Source: ", glue("{ unique(map_data_1()$source) }"),
            "\n",
            "Definition: ", glue("{ unique(map_data_1()$definition) }"))
    })

    #Source of the slected indicator Map2
    output$source_comp2 <- shiny::renderText({
      paste(" Source: ", glue("{ unique(map_data_2()$source) }"),
            "\n",
            "Definition: ", glue("{ unique(map_data_2()$definition) }"))
    })
    ##############################################.
    #### Modal  ----
    ###############################################.
    shiny::observeEvent(input$help_comp, {
      shiny::showModal(modalDialog(
        title = "How to use these maps",
        p("These maps offer comparison (spatial correlations) between selected Pakistan indicators"),
        # p("MAP1 refers to"),
        p("These maps give district (admin2) level estimates of Pakistan indicators over the selected filters"),
        p("All indicators are rounded to 2 decimal points"),
        # p("All Natural Hazards Indicators are rounded to 3 decimal points"),
        p("Expect the color mapping to reverse with the context of  the selected indicators - e.g. Poverty (High) = Red whereas; Access to improved toilet facilities (High) = Blue"),
        size = "m", easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
    })


  })
}

## To be copied in the UI
# mod_comparison_maps_ui("comparison_maps_1")

## To be copied in the server
# mod_comparison_maps_server("comparison_maps_1")
