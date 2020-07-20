#' Get title from INSEE series idbank
#'
#' @details Query INSEE website to get series title from series key (idbank).
#' Any query to INSEE database can handle around 400 idbanks at maximum, if necessary the idbank list will then be splitted in several lists of 400 idbanks each.
#' Consequently, it is not advised to use it on the whole idbank dataset, the user should filter the idbank dataset first.
#' @param ... list of series key (idbank)
#' @param lang language of the title, by default it is Engligh, if lang is different from "en" then French will be the title's language
#' @examples
#' library(tidyverse)
#'
#' idbank_dataset = get_idbank_list()
#'
#' idbank_dataset_with_title =
#'   idbank_dataset %>%
#'   filter(str_detect(nomflow, "CLIMAT-AFFAIRES")) %>%
#'   mutate(title = get_insee_title(idbank))
#' @export
get_insee_title = function(..., lang = "en"){

  if(length(list(...)) == 1){
    list_idbank = list(...)[[1]]
  }else{
    list_idbank = unlist(list(...))
  }

  # if there are more than 400 idbanks
  # the query is divided in several

  n_idbank = length(list_idbank)
  if(n_idbank > 200000){stop("Too many idbanks!")}
  n_idbank_query = 400

  list_seq = lapply(1:500, function(x){
    if(x == 1){
      return(1:n_idbank_query)
    }else{
      return(((x-1) * n_idbank_query + 1):(x * n_idbank_query))
    }
  })

  i = 1
  while(!(n_idbank %in% list_seq[[i]])){
    i = i + 1
  }

  titles_final = c()

  for(j in 1:i){

    selected_idbank = min(list_seq[[j]]):(min(max(list_seq[[j]]), n_idbank))
    list_idbank_selected = list_idbank[selected_idbank]

    df_title = get_insee_idbank(list_idbank_selected, lastNObservations = 1)
    df_title = as.data.frame(df_title)

    if(lang == "en"){
      titles = as.character(df_title[,"TITLE_EN"])
    }else{
      titles = as.character(df_title[,"TITLE_FR"])
    }
    titles_final = c(titles_final, titles)
  }

  return(titles_final)
}


#' Get data from INSEE series idbank
#'
#' @details Get data from INSEE series idbank
#'
#' @param ... one or several series key (idbank)
#' @param startPeriod start date of data
#' @param endPeriod end date of data
#' @param firstNObservations get the first N observations for each key series (idbank)
#' @param lastNObservations get the last N observations for each key series (idbank)
#' @examples data = get_insee_idbank("001558315", "010540726")
#'
#' @export
get_insee_idbank <- function(...,
                             startPeriod = NULL,
                             endPeriod = NULL,
                             firstNObservations = NULL,
                             lastNObservations = NULL){

  insee_bdm_series_link = Sys.getenv("INSEE_sdmx_link_idbank")

  if(length(list(...)) == 1){
    list_idbank = paste0(list(...)[[1]], collapse = "+")
  }else{
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

  data = get_insee(link)
  return(data)
}

#' Get dataset from INSEE BDM database
#'
#' @details Get dataset from INSEE BDM database
#' @param dataset dataset name to be downloaded
#' @param startPeriod start date of data
#' @param endPeriod end date of data
#' @param firstNObservations get the first N observations for each key series (idbank)
#' @param lastNObservations get the last N observations for each key series (idbank)
#' @param filter Use the filter to choose only some dimensions. It is recommended to use it for big datasets.
#' A dimension left empty means all values are selected. To select multiple values in one dimension put a "+" between those values (see example)
#' @examples
#' data = get_insee_dataset("CLIMAT-AFFAIRES")
#' data = get_insee_dataset("IPC-2015", filter = "M+A.........CVS.", startPeriod = "2015-03")
#'
#' @export
get_insee_dataset <- function(dataset,
                              startPeriod = NULL,
                              endPeriod = NULL,
                              firstNObservations = NULL,
                              lastNObservations = NULL,
                              filter = NULL){

  insee_bdm_dataset_link = Sys.getenv("INSEE_sdmx_link")

  if(!"character" %in% class(dataset)){
    stop("idbank must be a character")
  }
  if(length(dataset) != 1){
    stop("idbank length must be one")
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


#' Get data from INSEE BDM database with a SDMX query link
#'
#' @details Get data from INSEE BDM database with a SDMX query link
#' Mainly for package internal use from the functions get_insee_dataset and get_insee_idbank
#'
#' @param link SDMX query link
#' @examples data = get_insee("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010539365")
#'
#' @export
get_insee = function(link){

  response = httr::GET(link)
  response_content = try(httr::content(response, encoding = "UTF-8"), silent = TRUE)

  if(!"try-error" %in% class(response_content)){

    content_list = xml2::as_list(response_content)
    data = tibble::as_tibble(content_list)

    n_line = length(data[[1]])

    if(n_line > 1){

      n_series = length(data[[1]][[2]])
      list_df = list()

      for(i in 1:n_series){

        data_series = lapply(data[[1]][[2]][[i]], attributes)
        data_series = dplyr::bind_rows(data_series)

        metadata = attributes(data[[1]][[2]][[i]])
        names_in_metadata = which(names(metadata) == "names")

        if(length(names_in_metadata) > 0){
          metadata = metadata[-names_in_metadata]
        }

        if(nrow(data_series) > 0){
          for(metadata_item in names(metadata)){
            data_series[, metadata_item] = metadata[[metadata_item]]
          }

          list_df[[length(list_df)+1]] = data_series
        }
      }

      data_final = dplyr::bind_rows(list_df)
    }else{
      warning("The query might be either too big or wrongly done, try to modify it, use filter argument if necessary")
      warning(data[[1]][[1]][["Text"]][[1]])
      data_final = NULL
    }
  }else{
    warning("Wrong query")
    print(response)
    data_final = NULL
  }
  return(data_final)
}

