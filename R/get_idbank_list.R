#' Download a full INSEE's series key list
#'
#' @details Download a mapping dataset betwen INSEE series keys (idbank) and SDMX series names.
#' Under the hood the get_idbank_list uses download.file function from utils, the user can change the mode argument with the following
#' command : Sys.getenv(INSEE_download_option_idbank_list = "wb")
#' If INSEE makes an update, the user can also change the zip file downloaded, the data file contained in the zip and data the separator  :
#' Sys.setenv(INSEE_idbank_dataset_path = "new_zip_file_link")
#' Sys.setenv(INSEE_idbank_sep = ",")
#' Sys.setenv(INSEE_idbank_dataset_file = "new_data_file_name")
#' @param ... one or several dataset names
#' @param dataset if a dataset name is provided, only a subset of the data is delivered, otherwise
#' all the data is returned, and column names refer directly to data dimensions
#' @param label if TRUE titles are provided for each dimension value, by default it is FALSE
#' @examples
#' \donttest{idbank_list = get_idbank_list()}
#' @return a tibble the idbank dataset
#' @export
get_idbank_list = function(
  ...,
  dataset = NULL,
  label = FALSE
){

  insee_no_cache_use = if(Sys.getenv("INSEE_no_cache_use") == "TRUE"){TRUE}else{FALSE}
  insee_download_verbose = if(Sys.getenv("INSEE_download_verbose") == "TRUE"){TRUE}else{FALSE}
  mapping_file_pattern = Sys.getenv("INSEE_idbank_dataset_file")

  if(length(list(...)) > 0){
    if(length(list(...)) == 1){
      list_dataset = list(...)[[1]]
    }else{
      list_dataset = unlist(list(...))
    }
    if(is.null(dataset)){
      dataset = list_dataset
    }else{
      dataset = c(dataset, list_dataset)
    }
  }

  # temporary files
  temp_dir = tempdir()

  label_hash = ""
  dataset_hash = ""

   if(!is.null(dataset)){
      dataset_hash = paste0(dataset, collapse = "_")
    if(is.null(label)){
      label_hash = "T"
    }else{
      if(label == TRUE){
        label_hash = "T"
      }
    }
   }

  mapping_file_cache = file.path(temp_dir,
                                 paste0(openssl::md5(paste0(mapping_file_pattern, dataset_hash, label_hash)), ".rds"))

  if(!file.exists(mapping_file_cache) | insee_no_cache_use){

    # download and unzip
    mapping_final = try(download_idbank_list(mapping_file_cache = mapping_file_cache,
                                             dataset = dataset,
                                             label = label), silent = TRUE)

    if(class(mapping_final) == "try-error"){

      mapping_final = idbank_list

      msg1 = "Idbank list download failed."
      msg2 = "Package's internal data has been used instead."
      msg3 = "\nPlease contact the package maintainer if this error persists."
      warning(sprintf("%s %s %s", msg1, msg2, msg3))
    }

  }else{

    mapping_final = readRDS(mapping_file_cache)

    if(insee_download_verbose){
      msg = "Cached data has been used"
      message(crayon::style(msg, "green"))
    }
  }
  mapping_final = tibble::as_tibble(mapping_final)
  return(mapping_final)
}

