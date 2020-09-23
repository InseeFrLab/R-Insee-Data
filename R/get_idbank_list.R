#' Download a full INSEE's series key list
#'
#' @details Download a mapping dataset betwen INSEE series keys (idbank) and SDMX series names.
#' Under the hood the get_idbank_list uses download.file function from utils, the user can change the mode argument with the following
#' command : Sys.getenv(INSEE_download_option_idbank_list = "wb")
#' If INSEE makes an update, the user can also change the zip file downloaded, the data file contained in the zip and data the separator  :
#' Sys.setenv(INSEE_idbank_dataset_path = "new_zip_file_link")
#' Sys.setenv(INSEE_idbank_sep = ",")
#' Sys.setenv(INSEE_idbank_dataset_file = "new_data_file_name")
#' @param dataset if a dataset name is provided, only a subset of the data is delivered, otherwise
#' all the data is returned
#' @examples
#' \donttest{idbank_list = get_idbank_list()}
#' @return a tibble the idbank dataset
#' @export
get_idbank_list = function(
  dataset = NULL
){

  insee_no_cache_use = if(Sys.getenv("INSEE_no_cache_use") == "TRUE"){TRUE}else{FALSE}
  insee_download_verbose = if(Sys.getenv("INSEE_download_verbose") == "TRUE"){TRUE}else{FALSE}
  mapping_file_pattern = Sys.getenv("INSEE_idbank_dataset_file")

  # temporary files
  temp_dir = tempdir()

  mapping_file_cache = file.path(temp_dir, paste0(openssl::md5(mapping_file_pattern), ".rds"))

  if(!file.exists(mapping_file_cache) | insee_no_cache_use){

    # download and unzip
    mapping_final = try(download_idbank_list(mapping_file_cache = mapping_file_cache,
                                             dataset = dataset), silent = TRUE)

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

#' @noRd
download_idbank_list = function(mapping_file_cache = NULL, dataset = NULL){

  insee_download_verbose = if(Sys.getenv("INSEE_download_verbose") == "TRUE"){TRUE}else{FALSE}
  insee_download_option_idbank_list = Sys.getenv("INSEE_download_option_idbank_list")
  file_to_dwn = Sys.getenv("INSEE_idbank_dataset_path")
  mapping_file_pattern = Sys.getenv("INSEE_idbank_dataset_file")

  mapping_file_sep = Sys.getenv("INSEE_idbank_sep")
  idbank_nchar = as.numeric(Sys.getenv("INSEE_idbank_nchar"))

  if(is.na(idbank_nchar)){idbank_nchar = 9}

  temp_file = tempfile()
  temp_dir = tempdir()

  if(missing(mapping_file_cache)){
    mapping_file_cache = file.path(temp_dir, paste0(openssl::md5(mapping_file_pattern), ".rds"))
  }

  dwn = utils::download.file(file_to_dwn, temp_file,
                             mode = insee_download_option_idbank_list, quiet = TRUE)

  uzp = utils::unzip(temp_file, exdir = temp_dir)

  mapping_file = file.path(temp_dir, list.files(temp_dir, pattern = mapping_file_pattern)[1])
  # load data
  mapping = utils::read.delim(mapping_file, sep = mapping_file_sep,
                              stringsAsFactors = F)

  # filter data
  if(!is.null(dataset)){
    dataset_list = unique(mapping[, "nomflow"])
    if(dataset %in% dataset_list){
      mapping = mapping[which(mapping[, "nomflow"] == dataset),]
    }
  }

  dot_vector = stringr::str_count(mapping$cleFlow, pattern = "\\.")
  n_col = max(dot_vector) + 1

  mapping_final = separate_col(df = mapping, col = "cleFlow",
                               sep = "\\.", into = paste0("dim", 1:n_col))

  add_zero = function(x, idbank_nchar_arg = idbank_nchar){
    paste0(c(rep("0", idbank_nchar_arg-nchar(x)), x), collapse = "")}

  mapping_final[,"idbank"] = vapply(mapping_final[,"idbank"], add_zero, "")

  if("n_series" %in% names(mapping_final)){
    mapping_final[,"n_series"] = as.numeric(as.character(mapping_final[,"n_series"]))
  }

  if(insee_download_verbose){
    msg = sprintf("\nData cached : %s\n", mapping_file_cache)
    message(crayon::style(msg, "green"))
  }

  saveRDS(mapping_final, file = mapping_file_cache)

  return(mapping_final)
}
