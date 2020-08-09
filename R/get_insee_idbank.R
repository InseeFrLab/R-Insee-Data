#' Get data from INSEE series idbank
#'
#' @details Get data from INSEE series idbank
#'
#' @param ... one or several series key (idbank)
#' @param startPeriod start date of data
#' @param endPeriod end date of data
#' @param firstNObservations get the first N observations for each key series (idbank)
#' @param lastNObservations get the last N observations for each key series (idbank)
#' @param limit by default, the function get_insee_idbank has a 1200-idbank limit. Set limit argument to FALSE to ignore the limit or modify the limit with the following command : Sys.setenv(INSEE_idbank_limit = 1200)
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
                             limit = TRUE,
                             startPeriod = NULL,
                             endPeriod = NULL,
                             firstNObservations = NULL,
                             lastNObservations = NULL){

  insee_bdm_series_link = Sys.getenv("INSEE_sdmx_link_idbank")
  insee_get_idbank_limit = as.numeric(Sys.getenv("INSEE_get_idbank_limit"))
  insee_sdmx_idbank_limit = as.numeric(Sys.getenv("INSEE_sdmx_idbank_limit"))

  if(length(list(...)) == 1){
    list_idbank = list(...)[[1]]
  }else{
    list_idbank = unlist(list(...))
  }

  n_idbank = length(list_idbank)

  if(n_idbank > insee_get_idbank_limit & limit){
    msg = sprintf("By default, this function has a %s-idbank limit.\n  Please set limit argument to FALSE to ignore the limit.\n  Otherwise, modify the limit with the following command : Sys.setenv(INSEE_idbank_limit = 1200).\n  Beware that it could be slow.\n  Nevertheless, the data is cached, so all queries are only run once per R session.\n  A query run twice is then almost immediate.", insee_get_idbank_limit)
    cat(testthat:::colourise(msg, "error"))
    return(NULL)
  }

  if(n_idbank > insee_sdmx_idbank_limit & limit){
    msg = sprintf("The number of idbanks is higher than %s (insee's sdmx query limit),\n  multiple queries are then triggered.\n  To make it faster, please reduce the number of idbanks.\n  The data is cached, so all queries are only run once per R session.\n  A query run twice is then almost immediate.\n", insee_sdmx_idbank_limit)
    cat(testthat:::colourise(msg, "error"))
  }

  arg = c("startPeriod", "endPeriod", "firstNObservations", "lastNObservations")
  null_arg_vector = unlist(lapply(arg, function(x) is.null(get(x))))

  if(!all(null_arg_vector)){
    get_param = function(x) if(!is.null(get(x))){return(paste0(x, "=", get(x)))}
    param2add = paste0(unlist(lapply(arg, get_param)), collapse = "&")
  }

  max_seq = ceiling(n_idbank / insee_sdmx_idbank_limit)

  if(n_idbank > insee_sdmx_idbank_limit){
    msg = sprintf("\nData download and Dataframe build steps will be repeted %s times, unless cached data exist.\n", max_seq)
    cat(msg)
  }

  list_seq = lapply(1:max_seq, function(x){
      return(((x-1) * insee_sdmx_idbank_limit + 1):(x * insee_sdmx_idbank_limit))
  })

  list_df = list()

  for (j in 1:max_seq){

    selected_idbank = min(list_seq[[j]]):(min(max(list_seq[[j]]), n_idbank))
    list_idbank_selected = paste0(list_idbank[selected_idbank], collapse = "+")

    link = sprintf("%s/%s", insee_bdm_series_link, list_idbank_selected)

    if(!all(null_arg_vector)){
      link = paste0(link, "?", param2add)
    }

    df = get_insee(link = link, step = sprintf("%s/%s", j, max_seq))

    if(!is.null(df)){
      list_df[[length(list_df)+1]] = df
    }

  }

  data = dplyr::bind_rows(list_df)

  return(data)
}
