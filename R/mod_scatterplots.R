#' scatterplots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @importFrom  tidyr pivot_wider
#' @importFrom ggrepel geom_text_repel
#' @importFrom glue glue



mod_scatterplots_ui <- function(id){
  ns <- NS(id)
  tagList(


    sidebarLayout(
      sidebarPanel(width = 3,
                   style = "background-color: white;
                                               opacity: 0.85;
                                               padding: 20px 20px 20px 20px;
                                               margin: auto;
                                               border-radius: 5pt;
                                               box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                               padding-bottom: 2mm;
                                               padding-top: 1mm;",

                   selectInput(ns("time_s"), "Choose Year:",
                               choices =
                                 # yrs,
                                   Pak_Indicators_Data %>%
                                   dplyr::distinct(year) %>%
                                   arrange(desc(year)) %>%
                                   slice(-4),
                               selected = 2018,
                   ),
                   selectInput(ns("stat_s1"),
                               "Choose Indicator 1: ",
                               choices = unique(Pak_Indicators_Data$indicator)),
                   selectInput(ns("stat_s2"),
                               "Choose Indicator 2: ",
                               choices = unique(Pak_Indicators_Data$indicator)),

                   # actionButton(ns("prov_s"),
                   #              "Provincial Disaggregation",
                   #              class = "btn-sm"),
                   # br(),
                   br(),
                   downloadButton(ns("downloadplot3"), "
                                  Download Plot",
                                  class = "btn-sm")
      ),
      mainPanel(

        # h3("Scatterplot of Selected Indicators"),
        plotOutput(ns("plot3"),
                   click = "plot_click_s",

                   height = "450px",
                   width = 900),
        br(),
        verbatimTextOutput(ns("source_compgraph_1")),
        tags$head(tags$style("#scatterplots_1-source_compgraph_1{color:black; font-size:12px; font-style:italic;
overflow-y:scroll; max-height: 120px; background: #ffe6cc;}")),
        verbatimTextOutput(ns("source_compgraph_2")),
        tags$head(tags$style("#scatterplots_1-source_compgraph_2{color:black; font-size:12px; font-style:italic;
overflow-y:scroll; max-height: 120px; background: #ffe6cc;}"))
      ))
  )
}

#' scatterplots Server Functions
#'
#' @noRd
mod_scatterplots_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #Scatter Plot

    # User first choose year, and only those indicators show up in both indicator filters, for which data is available
    # So that empty plots in few cases won't show up

    #Selecting Year

    d_s0 <- reactive({
      Pak_Indicators_Data %>%
        filter(year == input$time_s) %>%
        arrange(desc(year))
    })

    #Updating indicator 1
    observeEvent(d_s0(),{
      updated_indicator1 <- d_s0() %>%
        filter(!is.na(value))
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "stat_s1",
        choices = unique(updated_indicator1$indicator),
        selected = unique(updated_indicator1$indicator)[2]
      )
    })

    #Updating Indicator 2

    observeEvent(d_s0(),{
      updated_indicator2 <- d_s0() %>%
        filter(!is.na(value))
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "stat_s2",
        choices = unique(updated_indicator2$indicator),
        selected = unique(updated_indicator2$indicator)[4]
      )
    })

    #Scatter plot function
    gr_3 <- function(){
      Pak_Indicators_Data %>%
        select(-domain, -source, -definition, -units, -indicator_1,-year_1, -positive, -negative, -context) %>%
        filter(
          # province == input$prov_s,
          year == input$time_s,
          province != "Islamabad") %>%
        filter(indicator %in%  c(input$stat_s1,input$stat_s2)) %>%
        pivot_wider(names_from =  indicator, values_from =  value) %>%
        ggplot(aes(.data[[input$stat_s1]], .data[[input$stat_s2]]))+
        geom_point(position = "jitter", size= 1) +
        # geom_text_repel(aes(label = district)) +
        geom_smooth(method = "lm") +
        scale_x_continuous(labels = scales::number_format() )+
        scale_y_continuous(labels = scales::number_format())+
        labs(title = glue("Year", " ", "{ input$time_s }:", " ", "Districts for Selected Indicators", " - ", "(Linear Model)"),
             # subtitle = "Linear Model",
             x=glue::glue("Indicator 1: ","{ input$stat_s1 }"),
             y=glue::glue("Indicator 2: ", "{ input$stat_s2 }"),
             color="Province") +
        theme(plot.title = element_text(hjust=0.5, face = "bold"))+
        theme(axis.text.x = element_text(size = 14, face = "bold"))+
        theme(axis.text.y = element_text(size = 9))+theme(
          axis.line = element_line(color='black'),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())
    }

    #Rendering scatter plot
    output$plot3 <- renderPlot({
      req(input$stat_s1)
      req(input$stat_s2)
      print(gr_3())
    })

    # output$info3 <- renderText({
    #   req(input$plot_click_s$x, input$plot_click_s$y)
    #   glue("{ input$stat_s1 }", " ", "="," ",  round(input$plot_click_s$x, 2), "\n {input$stat_s2 }", " ", "=", " ", round(input$plot_click_s$y ,2))
    # })

    #Download scatter plot
    output$downloadplot3 <- downloadHandler(
      filename = function(){
        paste0("plot_", glue("{ input$stat_s1 }","_", "{ input$stat_s2 }", "_", "{ input$time_s }"), ".png")
      },
      content = function(file){
        device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
        ggsave(file, gr_3(), device = device)
      }

    )

    #When province disaggregatio tab is clicked

    # observeEvent(input$prov_s,{
    #   gr_3p <- function(){
    #     Pak_Indicators_Data %>%
    #       select(-domain, -source, -definition, -units, -indicator_1,-year_1, -negative, -positive, -context) %>%
    #       filter(
    #         # province == input$prov_s,
    #         year == input$time_s,
    #         province != "Islamabad") %>%
    #       filter(indicator %in%  c(input$stat_s1,input$stat_s2)) %>%
    #       pivot_wider(names_from =  indicator, values_from =  value) %>%
    #       # spread(indicator, value) %>%
    #       ggplot(aes(.data[[input$stat_s1]], .data[[input$stat_s2]], color = province))+
    #       geom_point(position = "jitter", size= 1) +
    #       # geom_text_repel(aes(label = district)) +
    #       geom_smooth(method = "lm") +
    #       scale_x_continuous(labels = scales::number_format() )+
    #       scale_y_continuous(labels = scales::number_format())+
    #       labs(title = glue("Year", " ", "{ input$time_s }:", " ", "Districts for Selected Indicators"),
    #            subtitle = "Linear Model",
    #            x=input$stat_s1,
    #            y=input$stat_s2,
    #            color= "Province")
    #   }
    #
    #   #Rendering new plot afterwards
    #   output$plot3 <- renderPlot({
    #     print(gr_3p())
    #   })

      #Download new scatterplot after provincial disaggregation
      # output$downloadplot3 <- downloadHandler(
      #   filename = function(){
      #     paste0("plot_", glue("{ input$stat_s1 }","_", "{ input$stat_s2 }", "_", "{ input$time_s }"), ".png")
      #   },
      #   content = function(file){
      #     device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      #     ggsave(file, gr_3p(), device = device)
      #   }
      #
      # )

    # })

  })
}

## To be copied in the UI
#

## To be copied in the server
#
