#' Get data from INSEE BDM database with a SDMX query link
#'
#' @details Get data from INSEE BDM database with a SDMX query link.
#' This function is mainly for package internal use.
#' It is used by the functions get_insee_dataset, get_insee_idbank and get_dataset_list.
#' The data is cached, hence all queries are only run once per R session.
#' The user can disable the download display in the console with the following command :
#' Sys.setenv(INSEE_download_verbose = "FALSE")
#' The use of cached data can be disabled with : Sys.setenv(INSEE_no_cache_use = "TRUE")
#' All queries are printed in the console with this command: Sys.setenv(INSEE_print_query = "TRUE").
#' The RapidXML C++ library is used optionally thanks to the readsdmx package if it is installed.
#' The previous parser can still be used with this command : Sys.setenv(INSEE_read_sdmx_slow = "TRUE")
#' @param link SDMX query link
#' @param step argument used only for internal package purposes to tweak download display
#' @return a tibble containing the data
#' @examples
#' \donttest{
#' insee_link = "http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM"
#' insee_query = file.path(insee_link, paste0("010539365","?", "firstNObservations=1"))
#' data = get_insee(insee_query)
#' }
#' @export
get_insee = function(link, step = "1/1"){

  if(missing(link)){
    warning("link is missing")
    return(NULL)
  }

  insee_download_verbose = if(Sys.getenv("INSEE_download_verbose") == "TRUE"){TRUE}else{FALSE}
  insee_value_as_numeric = if(Sys.getenv("INSEE_value_as_numeric") == "TRUE"){TRUE}else{FALSE}
  insee_print_query = if(Sys.getenv("INSEE_print_query") == "TRUE"){TRUE}else{FALSE}
  insee_no_cache_use = if(Sys.getenv("INSEE_no_cache_use") == "TRUE"){TRUE}else{FALSE}
  insee_read_sdmx_slow = if(Sys.getenv("INSEE_read_sdmx_slow") == "TRUE"){TRUE}else{FALSE}

  if(insee_download_verbose){
    if(insee_print_query == TRUE) {
      msg = sprintf("Query : %s", link)
      message(crayon::style(msg, "black"))
    }
  }

  file_cache = file.path(tempdir(), paste0(openssl::md5(link), ".rds"))

  if((!file.exists(file_cache)) | insee_no_cache_use){

    use_read_sdmx_fast = TRUE

    if(insee_read_sdmx_slow){
      use_read_sdmx_fast = FALSE
    }

    if(stringr::str_detect(link, "includeHistory")){
      use_read_sdmx_fast = FALSE
    }

    if(link == Sys.getenv("INSEE_sdmx_link_dataflow")){
      use_read_sdmx_fast = FALSE
    }

    if(!requireNamespace("readsdmx", quietly = TRUE)){
      use_read_sdmx_fast = FALSE
    }

    if(use_read_sdmx_fast){
      data_final = try(read_sdmx_fast(link, step), silent = TRUE)
    }

    # read_sdmx_slow is used as a backup solution in case read_sdmx_fast is not working
    if(use_read_sdmx_fast == TRUE){
      if(!"try-error" %in% class(data_final)){
        if(is.null(data_final)){
          use_read_sdmx_fast = FALSE
        }
      }else{
        use_read_sdmx_fast = FALSE
      }
    }

    if(use_read_sdmx_fast == FALSE){
      data_final = read_sdmx_slow(link, step)
    }

    if(is.null(data_final)){
      if(!stringr::str_detect(link, "SERIES_BDM")){

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

              loc_slash = str_locate_all(list_query, "\\/")

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


    }


    if(!is.null(data_final)){

      saveRDS(data_final, file = file_cache)

      if(insee_download_verbose){
        if(use_read_sdmx_fast){
          msg = sprintf("Data cached : %s\n", file_cache)
        }else{
          msg = sprintf("\nData cached : %s\n", file_cache)
        }

        message(crayon::style(msg, "green"))
      }
    }
  }else{

    if(insee_download_verbose){
      msg = "Cached data has been used"
      message(crayon::style(msg, "green"))
    }

    data_final = readRDS(file_cache)
  }

  return(data_final)
}
