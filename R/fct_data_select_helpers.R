#' data_select_helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @import dplyr
#' @importFrom forcats fct_reorder

#' @noRd

#
# data('Pak_Indicators_Data')
#
# district_unique <-
#   Pak_Indicators_Data %>%
#   mutate(district = fct_reorder(district1,
#                                 value,
#                                 .desc=T)) %>%
#   filter(!is.na(value),
#          indicator != "Population Census") %>%
#   select(district) %>%
#   distinct()
