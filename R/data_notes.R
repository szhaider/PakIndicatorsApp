
#Data Help

#' PAKISTAN Indicators data
#'
#' Pakistan Indicators finalized data, excluding AJK and GB
#'
#'
#' @format ## `Pak_Indicators_Data`
#' A data frame (long format) with 270128 rows and 15 columns:
#' \describe{
#'   \item{district}{District name}
#'   \item{province}{Province Name}
#'   \item{indicator} {Particular indicator}
#'   \item{year} {Year of the survey}
#'   \item{source} {source of the survey}
#'   \item{value}{Indicator Value}
#'   \item{definition} {Definition of the indicator}
#'   \item{unit} {Units of the indicator}
#'   ...
#' }
#' @source Author's calculations based on the respective surveys
"Pak_Indicators_Data"


#' PAKISTAN SHAPEFILE 2022 data
#'
#' Pakistan Shapefiles excluding AJK and GB
#'
#'
#' @format ## `Pak_Shapfiles`
#' A data frame with 136 rows and 5 columns:
#' \describe{
#'   \item{district}{District name}
#'   \item{province}{Province Name}
#'   \item{geometry}{Geometries}
#'   ...
#' }
#' @source <https://data.humdata.org/dataset/cod-ab-pak?>
"Pak_Shapfiles"


