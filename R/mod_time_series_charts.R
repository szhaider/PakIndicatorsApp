#' time_series_charts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @importFrom dplyr filter pull distinct select
#'
#' @importFrom shiny NS tagList
mod_time_series_charts_ui <- function(id){
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

                   selectInput(ns("family"),
                               "Choose Domain:",
                               choices = Pak_Indicators_Data %>%
                                 distinct(domain) %>%
                                 filter(domain!= "Childhood Development",
                                        domain!= "Gender Equality",
                                        domain!= "Food Insecurity",
                                        domain!= "Malnutrition",
                                        domain!= "Social Protection",
                                        domain!= "Utilities",
                                        domain!= "Vaccination",
                                        domain!= "Maternal Newborn Care",
                                        domain!= "Demographics") %>%
                                 pull(domain)),

                   selectInput(ns("stat"),
                               "Choose Indicator: ",
                               choices = unique(Pak_Indicators_Data$indicator)),

                   selectizeInput(ns("dist"),
                                  "Select District: ",
                                  choices = Pak_Indicators_Data %>%
                                      mutate(district = fct_reorder(district1,
                                                                    value,
                                                                    .desc=T)) %>%
                                      filter(!is.na(value),
                                             indicator != "Population Census") %>%
                                      select(district) %>%
                                      distinct(),
                                  selected = c("Balochistan , Awaran" ,
                                               "Balochistan , Chagai",
                                               "Balochistan , Gwadar" ,
                                               "Balochistan , Khuzdar",
                                               "Balochistan , Quetta"),
                                  multiple = TRUE)
      ),
      mainPanel(

        h3("Time Trend of Selected Indicators"),
        plotOutput(ns("plot2"),
                     height = "400px", width = 900) ,

        verbatimTextOutput(ns("source_timeseries")),
        tags$head(tags$style("#time_series_charts_1-source_timeseries{color:black; font-size:12px; font-style:italic;
overflow-y:scroll; max-height: 120px; background: #ffe6cc;}"))

      ))
  )
}

#' time_series_charts Server Functions
#'
#' @noRd
mod_time_series_charts_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Plot Time trend of indicators in selected districts
    #selecting domain
    d_tt0 <- reactive({
      Pak_Indicators_Data %>%
        filter(source != "NNS",
               source != "MICS",
               domain == input$family)
    })
    #Updating indicators
    observeEvent(d_tt0(),{
      choices1 <- unique(d_tt0()$indicator)
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "stat",
        choices = choices1)
    })

    #Selecting indicator
    d_tt1 <- reactive({
      d_tt0() %>%
        filter(indicator == input$stat)
    })

    #Updating provinces based on the indicator selection
    observeEvent(d_tt1(),{
      updated_prov_t <-  d_tt1() %>%
        select(-domain,-indicator_1, -units, -source, -definition, -positive, -negative, -context) %>%
        filter(!is.na(value),
               province != "Federal Capital Territory") %>%
        distinct(province, year) %>%
        group_by(province) %>%
        filter(n()>=2) %>%
        select(-year)
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "prov",
        choices = updated_prov_t)
    })

    #Time series chart function
    gr_2 <- function(){
      req(input$dist)
      Pak_Indicators_Data %>%
        filter(source != "NNS",
               source != 'MICS',
               domain == input$family,
               indicator == input$stat,
               district1 %in% input$dist) %>%
        filter(province != "Islamabad") %>%
        filter(
          # !str_detect(district1, "FR"),
          #      !str_detect(district1, "Agency"),
               indicator!= "Population Census",
               !is.na(value)) %>%
        ggplot(aes(year, value, color = district1)) +
        geom_line(size= 0.75) +
        geom_point(size=1.25) +
        expand_limits(y=0)+
        labs(x= "Years", y= input$stat,
             color= "District")
    }

    #Rendering chart time series
    output$plot2 <- renderPlot({
      gr_2()
    })



  })
}

## To be copied in the UI
# mod_time_series_charts_ui("time_series_charts_1")

## To be copied in the server
# mod_time_series_charts_server("time_series_charts_1")
