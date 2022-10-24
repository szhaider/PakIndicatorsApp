#' document UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom sf st_write
#' @importFrom zip zip
#' @import shiny
#' @importFrom dplyr select
#'
mod_document_ui <- function(id){
  ns <- NS(id)
  tagList(


    mainPanel(
      br(),
      h4(strong("This App covers district level spatial disparities in Pakistan for various Social and Welfare Indicators")),
      br(),
      h4(strong("The App can be used to explore the changes  in socio-economic indicators over time")),
      tags$hr(),
      fluidRow(downloadButton(ns("bulkdownload"), "Download Complete Dataset",
                              class = "btn-success"),
               downloadButton(ns("shapefile"), "Download Pakistan Shapefile",
                              class = "btn-success"),
               downloadButton(ns("glossary"), "Download Data Glossay", class= "btn-success"),
               hr(),
               h4(strong("Micro Data Web Links")),
               br(),
               tags$a(href= "https://microdata.worldbank.org/index.php/catalog?sort_by=rank&sort_order=desc&sk=pakistan", "Data Link for Pakistan", target="_blank"), br(),
               div(tags$em(span("World Bank's Micro Data Repository")), style="color:black"),
               tags$hr(),
               h4(strong("GitHub Repository")),
               br(),
               tags$a(href= "https://github.com/szhaider/PakIndicatorsApp.git", "Repo Link", target="_blank"), br(),
               # div(tags$em(span("Will be on Github repo")), style="color:red"),

      ),
      tags$hr(),

    )
  )
}

#' document Server Functions
#'
#' @noRd
mod_document_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    #Download BUlk data
    data_bulk <- reactive({
      Pak_Indicators_Data %>%
        dplyr::select(province, district, year, indicator, value, domain, source, units)
    })

    output$bulkdownload <- downloadHandler(
      filename = "Pak_Indicators_Data.csv",
      content = function(file){
        utils::write.csv(data_bulk(), file)
      }

    )


    #to Download shapefile 2022 in zip
    output$shapefile <- downloadHandler(
      filename = "PakistanShapefile2022.zip",
      content = function(file){
        if(length(Sys.glob("Pak_Shapfiles.*"))>0){
          file.remove(Sys.glob("Pak_Shapfiles.*"))
        }
        st_write(Pak_Shapfiles, dsn = "Pak_Shapfiles.shp", layer= "Pak_Shapfiles" ,driver= "ESRI Shapefile", overwrite_layer = T)
        # writeOGR(pak_shp1, dsn = "pakistan_shape.shp", layer= "pakistan_shape" ,driver= "ESRI Shapefile", overwrite_layer = T)

        zip(zipfile = 'PakistanShapefile2022.zip', files= Sys.glob("Pak_Shapfiles.*"))
        file.copy("PakistanShapefile2022.zip", file)
        if(length(Sys.glob("Pak_Shapfiles.*"))>0){
          file.remove(Sys.glob("Pak_Shapfiles.*"))
        }
      }
    )


    #Download dataset glossary in pdf
    output$glossary <- downloadHandler(
      filename = "Glossary.pdf",
      content = function(file){
        file.copy("inst/app/www/definitions.pdf", file)
      }
    )


  })
}

## To be copied in the UI
#

## To be copied in the server
#
