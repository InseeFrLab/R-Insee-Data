#' Get data from INSEE series idbank
#'
#' @details Get data from INSEE series idbank
#'
#' @param ... one or several series key (idbank)
#' @param startPeriod start date of data
#' @param endPeriod end date of data
#' @param firstNObservations get the first N observations for each key series (idbank)
#' @param lastNObservations get the last N observations for each key series (idbank)
#' @examples
#' \donttest{
#'
#' #example 1 : import price index of industrial products and turnover index : manufacture of wood
#' data = get_insee_idbank("001558315", "010540726")
#'
#' #example 2 : unemployment data
#'
#' library(tidyverse)
#'
#' idbank_list = get_idbank_list()
#'
#' df_idbank_list_selected =
#'   idbank_list %>%
#'   filter(nomflow == "CHOMAGE-TRIM-NATIONAL") %>%  #unemployment dataset
#'   filter(dim5 == 0) %>% #men and women
#'   mutate(title = get_insee_title(idbank))
#'
#' idbank_list_selected = df_idbank_list_selected %>% pull(idbank)
#'
#' unem = get_insee_idbank(idbank_list_selected)
#'
#' #example 3 : French GDP growth rate
#'
#' library(tidyverse)
#'
#' idbank_list = get_idbank_list()
#'
#' df_idbank_list_selected =
#'   idbank_list %>%
#'   filter(nomflow == "CNT-2014-PIB-EQB-RF") %>%  # Gross domestic product balance
#'   filter(dim1 == "T") %>% #quarter
#'   filter(dim4 == "PIB") %>% #GDP
#'   filter(dim6 == "TAUX") %>% #rate
#'   filter(dim10 == "CVS-CJO") #SA-WDA, seasonally adjusted, working day adjusted
#'
#' idbank = df_idbank_list_selected %>% pull(idbank)
#'
#' data = get_insee_idbank(idbank)
#'
#' #plot
#' ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
#' geom_col() +
#' ggtitle("French GDP growth rate, quarter-on-quarter, sa-wda") +
#' labs(subtitle = sprintf("Last updated : %s", data$TIME_PERIOD[1]))
#' }
#'
#' @export
get_insee_idbank <- function(...,
                             startPeriod = NULL,
                             endPeriod = NULL,
                             firstNObservations = NULL,
                             lastNObservations = NULL){

  insee_bdm_series_link = Sys.getenv("INSEE_sdmx_link_idbank")

  list_length = length(list(...))

  if(list_length == 1){
    list_idbank = paste0(list(...)[[1]], collapse = "+")
  }else if(list_length <= 400){
    list_idbank = paste0(list(...), collapse = "+")
  }

  link = sprintf("%s/%s", insee_bdm_series_link, list_idbank)

  arg = c("startPeriod", "endPeriod", "firstNObservations", "lastNObservations")
  null_arg_vector = unlist(lapply(arg, function(x) is.null(get(x))))

  if(!all(null_arg_vector)){
    get_param = function(x) if(!is.null(get(x))){return(paste0(x, "=", get(x)))}
    param2add = paste0(unlist(lapply(arg, get_param)), collapse = "&")
    link = paste0(link, "?", param2add)
  }

  data = insee::get_insee(link)
  return(data)
}
