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

  if(insee_download_verbose){
    if(insee_print_query == TRUE) {
      msg = sprintf("Query : %s", link)
      message(crayon::style(msg, "black"))
    }
  }

  file_cache = file.path(tempdir(), paste0(openssl::md5(link), ".rds"))

  if((!file.exists(file_cache)) | insee_no_cache_use){

    use_read_sdmx_fast = TRUE

    if(link == Sys.getenv("INSEE_sdmx_link_dataflow")){
      use_read_sdmx_fast = FALSE
    }
    if(stringr::str_detect(link, "includeHistory")){
      use_read_sdmx_fast = FALSE
    }
    if(Sys.getenv("INSEE_use_read_sdmx_slow") == "TRUE"){
      use_read_sdmx_fast = FALSE
    }

    if(use_read_sdmx_fast){
      data_final = read_sdmx_fast(link)
    }else{
      data_final = read_sdmx_slow(link)
    }

    if(!is.null(data_final)){

      saveRDS(data_final, file = file_cache)

      if(insee_download_verbose){
        msg = sprintf("\nData cached : %s\n", file_cache)
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
