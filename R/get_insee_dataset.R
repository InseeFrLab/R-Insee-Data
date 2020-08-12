#' Get dataset from INSEE BDM database
#'
#' @details Get dataset from INSEE BDM database
#' @param dataset dataset name to be downloaded
#' @param startPeriod start date of data
#' @param endPeriod end date of data
#' @param firstNObservations get the first N observations for each key series (idbank)
#' @param lastNObservations get the last N observations for each key series (idbank)
#' @param filter Use the filter to choose only some values in a dimension. It is recommended to use it for big datasets.
#' A dimension left empty means all values are selected. To select multiple values in one dimension put a "+" between those values (see example)
#' @examples
#' \donttest{
#' insee_dataset = get_dataset_list()
#'
#' data = get_insee_dataset("IPC-2015", filter = "M+A.........CVS.", startPeriod = "2015-03")
#' }
#'
#' @export
get_insee_dataset <- function(dataset,
                              startPeriod = NULL,
                              endPeriod = NULL,
                              firstNObservations = NULL,
                              lastNObservations = NULL,
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

  arg = c("startPeriod", "endPeriod", "firstNObservations", "lastNObservations")
  null_arg_vector = unlist(lapply(arg, function(x) is.null(get(x))))

  if(!all(null_arg_vector)){
    get_param = function(x) if(!is.null(get(x))){return(paste0(x, "=", get(x)))}
    param2add = paste0(unlist(lapply(arg, get_param)), collapse = "&")
    link = paste0(link, "?", param2add)
  }

  data = get_insee(link)
  return(data)
}
