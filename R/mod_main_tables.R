#' main_tables UI Function
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
#' @import glue
#' @import DT
#'
#'
mod_main_tables_ui <- function(id){
  ns <- NS(id)
  tagList(

    sidebarLayout(
      sidebarPanel(width = 3,
                   style = "background-color: white;
                                 opacity: 0.90;
                                 padding: 20px 20px 20px 20px;
                                 margin: auto;
                                 border-radius: 5pt;
                                 box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                 padding-bottom: 2mm;
                                 padding-top: 1mm;",


                   selectInput(ns("family"),
                               "Choose Domain:",
                               choices = unique(Pak_Indicators_Data$domain),
                               selected = "Household Welfare"),

                   selectInput(ns("stat"),
                               "Choose Indicator: ",
                               choices = unique(Pak_Indicators_Data$indicator)),

                   selectInput(ns("time"),
                               "Choose Year:",
                               choices = unique(Pak_Indicators_Data$year_1)),


                   conditionalPanel(condition =  sprintf("input['%s'] == '0'", ns("national")) ,
                   selectInput(ns("prov"),
                               "Select Province: ",
                               choices = unique(Pak_Indicators_Data$province))
                   ),
                   verbatimTextOutput(ns("table_note")),
                   tags$head(tags$style("#main_tables_1-table_note{color:black; font-size:12px; font-style:italic;
overflow-y:scroll; max-height: 120px; background: #ffe6cc;}")),
                   br(),
                   actionButton(ns("national"),
                                "Click for National Level Table",
                                class = "btn-sm")
      ),

      mainPanel(
        # column(width = 12,
               DT::DTOutput(ns("table")),   #width =900
               downloadButton(ns("downloadtable"),
                              "Download Table",
                              class = "btn-sm")
        # )
      )
    )
  )
}

#' main_tables Server Functions
#'
#' @noRd
mod_main_tables_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    #Selected domain
    d_t <- reactive({
      Pak_Indicators_Data %>%
        dplyr::filter(domain == input$family)
    })

    #Updating indicators based on that
    observeEvent(d_t(),{
      choices <- unique(d_t()$indicator)
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "stat",
        choices = choices)
    })

    #selecting indicator
    d_t1 <- reactive({
      Pak_Indicators_Data %>%
        dplyr::filter(indicator == input$stat)
    })

    #Updating years based on the previous selection
    observeEvent(d_t1(), {
      updated_years_t <- d_t1() %>%
        dplyr::filter(!is.na(value))
      updateSelectizeInput(
        session = getDefaultReactiveDomain(),
        "time",
        choices = sort(unique(updated_years_t$year_1), decreasing = T)
      )
    })

    #making reactive dataset
    d2 <- reactive({
      d_t1() %>%
        dplyr::filter(indicator == input$stat,
                      year_1 == input$time,
                      province == input$prov,
                      !is.na(value))
    })

    #making reactive dataset
    d2_2 <- reactive({
      d_t1() %>%
        dplyr::filter(indicator == input$stat,
                      year_1 == input$time,
                      !is.na(value))
    })

    #Table specification
    table1 <- reactive({
      d2() %>%
        select(-domain, -source, -definition, -units, -indicator_1,
               -year_1, -positive, -negative, -context, -district1) %>%
        select(year, province, district, indicator, value) %>%
        arrange(province, district) %>%
        rename(
               Year    = year,
               Province = province,
               District = district,
               Indicator = indicator,
               Value = value)
    })

    #Rendering table
    output$table <- DT::renderDT({
      DT::datatable(table1(),
                    options= list(pageLength=25))
    })

    #For national level table, for 1 go download
    observeEvent(input$national,{
      table2 <- reactive({
        d2_2() %>%
          select(-domain, -source, -definition, -units, -indicator_1,
                   -year_1, -positive, -negative, -context,  -district1) %>%
          select(year, province, district, indicator, value) %>%  arrange(province, district) %>%
          rename(
                 Year     = year,
                 Province = province,
                 District = district,
                 Indicator = indicator,
                 Value = value)
      })
      #rendering national level table
      output$table <- DT::renderDT({
        suppressWarnings(
        DT::datatable(table2(),
                      options= list(pageLength=25))
        )
      })
      #Download national level table
      output$downloadtable <- downloadHandler(
        filename = function(){
          paste0("table_", glue("{ input$stat }", "_", "{ input$time }"), ".csv")
        },
        content = function(file){
          write.csv(table2(), file)
        }

      )
    })

    #download main table
    output$downloadtable <- downloadHandler(
      filename = function(){
        paste0("table_", glue("{ input$stat }", "_", "{ input$time }"), ".csv")
      },
      content = function(file){
        write.csv(table1(), file)
      })
  })
}


