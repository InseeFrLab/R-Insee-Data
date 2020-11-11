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
#' @param update It is FALSE by default, if it is set to TRUE, it triggers the metadata update. This update is automatically triggered once every 6 months.
#' @examples
#' \donttest{idbank_list = get_idbank_list()}
#' @return a tibble the idbank dataset
#' @export
get_idbank_list = function(
  ...,
  dataset = NULL,
  update = FALSE
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

  insee_folder = file.path(rappdirs::user_data_dir(), "insee")
  list_folders = c(insee_folder, file.path(insee_folder, "insee"))

  for(ifile in 1:length(list_folders)){
    if(!file.exists(list_folders[ifile])){
      dir.create(list_folders[ifile])
    }
  }

  metadata_file_cache = file.path(rappdirs::user_data_dir("insee"),
                                  paste0(openssl::md5("insee_metadata_file"), ".rds"))

  metadata_file_cache_date = file.path(rappdirs::user_data_dir("insee"),
                                       paste0(openssl::md5("insee_metadata_file_date"), ".rds"))

  if(!is.null(dataset)){
    dataset_hash = paste0(dataset, collapse = "_")

    dataset_metadata_file_cache = file.path(rappdirs::user_data_dir("insee"),
                                            paste0(openssl::md5(sprintf("%s_metadata_file", dataset_hash)), ".rds"))

  }else{
    dataset_metadata_file_cache = metadata_file_cache
  }

  dataset_list = invisible(get_dataset_list()$id)

  if(!is.null(dataset)){
    dataset_selected = which(dataset %in% dataset_list)

    if(length(dataset_selected) == 0){
      warning("None dataset provided is among INSEE's datasets \nGet the list with get_dataset_list()")
      return(NULL)
    }else{
      dataset = dataset[dataset_selected]
    }
  }

  #
  # Check when was the last metadata update, update triggered if the metadata is older than 6 months
  #

  today_date = Sys.Date()
  auto = FALSE

  if(file.exists(metadata_file_cache_date)){

    date_last_update = readRDS(metadata_file_cache_date)

    if(difftime(today_date, date_last_update, units = "days") > 180){
      update = TRUE
      auto = TRUE
    }

  }else{
    update = TRUE
    auto = TRUE
  }

  #
  # if the update is not triggered
  # the metadata requested is built from existing local metadata
  # if some metadata is missing the update is triggered
  #

  if(update == FALSE){
    idbank_list = read_dataset_metadata(dataset = dataset,
                                        dataset_metadata_file_cache = dataset_metadata_file_cache)
    if(class(idbank_list) != "data.frame"){
      if(idbank_list == TRUE){
        auto = TRUE
        update = TRUE
      }
    }else{
      return(idbank_list)
    }
  }

    if(!file.exists(metadata_file_cache)){
      update = TRUE
      auto = TRUE
    }

    if(update){
      if(auto){
        msg1bis = "\nEither because it is older than 6 months, or because some files are missing"
        msg1 = sprintf("Metadata update has been triggered automatically%s", msg1bis)
      }else{
        msg1 = "Metadata update triggered manually"
      }

      msg2 = "\nIt may last several minutes"
      message(crayon::style(sprintf("%s %s", msg1, msg2), "red"))

      idbank_list = try(download_idbank_list(dataset = dataset_list,
                                             label = TRUE), silent = TRUE)

      idbank_list = try(download_idbank_list(), silent = TRUE)

      if(class(idbank_list) == "try-error"){

        idbank_list = clean_table(
          dplyr::filter(.data = idbank_list_internal, .data$nomflow %in% dataset)
        )

        msg1 = "Idbank list download failed"
        msg2 = "\nPackage's internal data has been used instead"
        msg3 = "\nPlease contact the package maintainer if this error persists"
        warning(sprintf("%s %s %s", msg1, msg2, msg3))

      }else{

        saveRDS(idbank_list, file = metadata_file_cache)
        saveRDS(today_date, file = metadata_file_cache_date)

        msg = sprintf("\nData cached : %s", metadata_file_cache)
        message(crayon::style(msg, "green"))
      }

    }

   idbank_list = read_dataset_metadata(dataset = dataset,
                                      dataset_metadata_file_cache = dataset_metadata_file_cache)

  idbank_list = tibble::as_tibble(idbank_list)

  return(idbank_list)

}

#' @noRd
read_dataset_metadata = function(dataset, dataset_metadata_file_cache){


    if(!file.exists(dataset_metadata_file_cache)){

      if(!is.null(dataset)){
        sub_dataset_metadata_file = unlist(lapply(dataset,
                                                  function(dt){
                                                    return(file.path(rappdirs::user_data_dir("insee"),
                                                                     paste0(openssl::md5(sprintf("%s_metadata_file", dt)), ".rds")))
                                                  }))

        if(all(file.exists(sub_dataset_metadata_file))){

          idbank_list = dplyr::bind_rows(lapply(sub_dataset_metadata_file,
                                                function(dataset_file){return(readRDS(dataset_file))}))

          idbank_list = set_metadata_col(idbank_list)

          saveRDS(idbank_list, file = dataset_metadata_file_cache)

          return(idbank_list)

        }else{
          return(TRUE)
        }
      }


    }else{

      idbank_list = readRDS(dataset_metadata_file_cache)
      return(idbank_list)

    }

}

#' @noRd
clean_insee_folder = function(){

  list_file_insee = file.path(rappdirs::user_data_dir("insee"),
                              list.files(rappdirs::user_data_dir("insee")))

  for(file_name in list_file_insee){
    file.remove(file_name)
  }

}

