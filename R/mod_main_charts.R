#' main_charts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot geom_col aes labs ggsave
#' @importFrom grDevices png
#' @importFrom stringr str_detect
#' @importFrom dplyr filter mutate distinct
#' @importFrom glue glue
#' @importFrom shiny NS tagList
#' @import shiny

mod_main_charts_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 3,
                   style = "background-color: white;
                               opacity: 0.85;
                               padding: 20px 20px 20px 20px;
                               margin: auto;
                               border-radius: 5pt;
                               box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                               padding-bottom: 2mm;
                               padding-top: 1mm;",

                   shiny::selectInput(ns("family"),
                                      "Choose Domain:",
                               choices = unique(Pak_Indicators_Data$domain)),

                   shiny::selectInput(ns("stat"),
                               "Choose Indicator: ",
                               choices = unique(Pak_Indicators_Data$indicator)),

                   shiny::selectInput(ns("time"),
                                      "Choose Year:",
                               choices = unique(Pak_Indicators_Data$year_1)),

                   shiny::selectInput(ns("prov"),
                                      "Select Province: ",
                               choices = Pak_Indicators_Data %>% distinct(province) %>%
                                 filter(province != "Federal Capital Territory")
                   ),
                   # br(),
                   shiny::downloadButton(ns("downloadplot"), "Download Plot", class ="btn-sm")

      ),
      mainPanel(

        # h4("District-wise Comparison"),
        plotOutput(ns("plot1"),
                   width = '800px',
                   height = '550px'),

        verbatimTextOutput(ns("source_graph")),
        tags$head(tags$style("#main_charts_1-source_graph{color:black; font-size:12px; font-style:italic;
overflow-y:scroll; max-height: 120px; background: #ffe6cc;}")),
      ))
  )
}

#' main_charts Server Functions
#'
#' @noRd
mod_main_charts_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #Selecting domain
    d_g <- reactive({
      Pak_Indicators_Data %>%
        dplyr::filter(domain == input$family)
    })

    #Updating indicators based on domain
    shiny::observeEvent(d_g(),{
      choices <- unique(d_g()$indicator)
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "stat",
        choices = choices)
    })

    #selecting indicator
    d_g1 <- reactive({
      Pak_Indicators_Data %>%
        dplyr::filter(indicator == input$stat)
    })

    #Updating years based on indicators
    shiny::observeEvent(d_g1(), {
      updated_years <- d_g1() %>%
        dplyr::filter(!is.na(value))
      shiny::updateSelectInput(
        session = getDefaultReactiveDomain(),
        "time",
        choices = sort(unique(updated_years$year_1), decreasing = T)
      )
    })

    #Selecting year
    d_g2 <- reactive({
      d_g1() %>%
        dplyr::filter(year_1 == input$time)
    })

    #Updating province based on selected year
    shiny::observeEvent(d_g2(), {
      updated_prov_plot <- d_g2() %>%
        dplyr::filter(!is.na(value),
               province != "Islamabad")
      shiny::updateSelectInput(
        session = getDefaultReactiveDomain(),
        "prov",
        choices = unique(updated_prov_plot$province)
      )
    })

    #Graph reactive
    gr_1 <- function(){
      Pak_Indicators_Data %>%
        dplyr::filter(province == input$prov,
               indicator == input$stat,
               year_1 == input$time) %>%
        dplyr::filter(province != "Islamabad") %>%
        # dplyr::filter(!str_detect(district, "FR"),
        #        !str_detect(district, "Agency"),
        dplyr::filter(!is.na(value)) %>%
        dplyr::mutate(district = fct_reorder(district, value)) %>%
        ggplot2::ggplot(aes(value, district )) +
        ggplot2::geom_col(na.rm= T,  alpha=0.5, fill = "seagreen")+
        # geom_text(aes(label = round(value,1)),
        #       alpha= 0.9, size=3.2,
        #       vjust=1, hjust = 1,
        #       nudge_y= 0.4,
        #       color="white")+
        labs(y= "", x = input$stat)+
        ggtitle(input$prov)+
        theme(plot.title = element_text(hjust=0.5, face = "bold"))+
        theme(axis.text.x = element_text(size = 14, face = "bold"))+
        theme(axis.text.y = element_text(size = 9))+theme(
          axis.line = element_line(color='black'),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())

    }

    #Rendering graph
    output$plot1 <- shiny::renderPlot({
      print(gr_1())
    })

    #Download grpah
    output$downloadplot <- shiny::downloadHandler(
      filename = function(){
        paste0("plot_", glue::glue("{ input$stat }",  "_", "{ input$stat }", "_", "{ input$time }"), ".png")
      },
      content = function(file){
        device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
        ggplot2::ggsave(file, gr_1(), device = device)
      }

    )

  })
}

## To be copied in the UI
# mod_main_charts_ui("main_charts_1")

## To be copied in the server
# mod_main_charts_server("main_charts_1")
