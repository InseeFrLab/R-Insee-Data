#' Get dataset from INSEE BDM database
#'
#' @details Get dataset from INSEE BDM database
#' @param dataset dataset name to be downloaded
#' @param startPeriod start date of data
#' @param endPeriod end date of data
#' @param firstNObservations get the first N observations for each key series (idbank)
#' @param lastNObservations get the last N observations for each key series (idbank)
#' @param includeHistory boolean to access the previous releases (not available on all series)
#' @param updatedAfter starting point for querying the previous releases (format yyyy-mm-ddThh:mm:ss)
#' @param filter Use the filter to choose only some values in a dimension. It is recommended to use it for big datasets.
#' A dimension left empty means all values are selected. To select multiple values in one dimension put a "+" between those values (see example)
#' @return a tibble with the data
#' @examples
#' \donttest{
#' insee_dataset = get_dataset_list()
#' idbank_ipc = get_idbank_list("IPC-2015")
#'
#' #example 1
#' data = get_insee_dataset("IPC-2015", filter = "M+A.........CVS..", startPeriod = "2015-03")
#'
#' #example 2
#' data = get_insee_dataset("IPC-2015", filter = "A..SO...VARIATIONS_A....BRUT..SO",
#'  includeHistory = TRUE, updatedAfter = "2017-07-11T08:45:00")
#' }
#'
#' @export
get_insee_dataset <- function(dataset,
                              startPeriod = NULL,
                              endPeriod = NULL,
                              firstNObservations = NULL,
                              lastNObservations = NULL,
                              includeHistory = NULL,
                              updatedAfter = NULL,
                              filter = NULL){

  if(missing(dataset)){
    warning("dataset is missing")
    return(NULL)
  }

  insee_bdm_dataset_link = Sys.getenv("INSEE_sdmx_link_dataset")

  if(!"character" %in% class(dataset)){
    stop("dataset name must be a character")
  }
  if(length(dataset) != 1){
    stop("only one dataset is expected not more")
  }

  link = sprintf("%s/%s", insee_bdm_dataset_link, dataset)

  if(!is.null(filter)){
    link = paste0(link, "/", filter)
  }

  if(!is.null(includeHistory)){
    if(includeHistory == TRUE){includeHistory = "true"}
  }

  arg = c("startPeriod", "endPeriod", "firstNObservations", "lastNObservations",
          "includeHistory", "updatedAfter")
  null_arg_vector = unlist(lapply(arg, function(x) is.null(get(x))))

  if(!all(null_arg_vector)){
    get_param = function(x) if(!is.null(get(x))){return(paste0(x, "=", get(x)))}
    param2add = paste0(unlist(lapply(arg, get_param)), collapse = "&")
    link = paste0(link, "?", param2add)
  }

  data_final = get_insee(link)

  if(is.null(data_final)){

      response <- httr::GET(link)

      response_content = try(httr::content(response, encoding = "UTF-8"), silent = TRUE)

      if(!"try-error" %in% class(response_content)){

        content_list = xml2::as_list(response_content)
        content_query = content_list$Error$ErrorMessage

        if(!is.null(content_query)){
          lgt = length(content_query)

          list_query = unlist(lapply(1:lgt,
                                     function(i){content_query[[i]][1]}))

          list_query = list_query[stringr::str_detect(list_query, "bdm.insee.fr")]

          if(length(list_query) > 0){

            loc_slash = stringr::str_locate_all(list_query, "\\/")

            loc_slash_vec = unlist(lapply(1:length(loc_slash),
                                          function(i){
                                            max(loc_slash[[i]])
                                          }))

            query_filter = unlist(lapply(1:length(list_query),
                                         function(i){
                                           substr(list_query[i], loc_slash_vec[i]+1, nchar(list_query[i]))
                                         }))

            query_filter = paste0(query_filter, collapse = "  ")

            msg = sprintf("\nThe query is too big, please use the following filters : \n%s\n", query_filter)
            message(crayon::style(msg, "red"))

          }

        }
      }

  }

  return(data_final)
}
