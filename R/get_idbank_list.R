#' Download a full INSEE's series key list
#'
#' @details Download a mapping dataset between INSEE series keys (idbank) and SDMX series names.
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

  insee_today_date = as.Date(as.character(Sys.getenv("INSEE_today_date")))
  insee_no_cache_use = if(Sys.getenv("INSEE_no_cache_use") == "TRUE"){TRUE}else{FALSE}
  mapping_file_pattern = Sys.getenv("INSEE_idbank_dataset_file")

  if(length(list(...)) > 0){
    if(length(list(...)) == 1){
      list_dataset = unlist(list(...)[[1]])
    }else{
      list_dataset = unlist(list(...))
    }
    if(is.null(dataset)){
      dataset = list_dataset
    }else{
      dataset = c(dataset, list_dataset)
    }
  }

  dir_creation_fail = try(create_insee_folder(), silent = TRUE)

  if(!"try-error" %in% class(dir_creation_fail)){
    insee_local_dir = rappdirs::user_data_dir("insee")
  }else{
    insee_local_dir = tempdir()
  }

  metadata_file_cache = file.path(insee_local_dir, paste0(openssl::md5("insee_metadata_file"), ".rds"))

  metadata_file_cache_date = file.path(insee_local_dir, paste0(openssl::md5("insee_metadata_file_date"), ".rds"))

  # file_warning_deprecated = file.path(tempdir(), paste0(openssl::md5("dimdeprecated"), ".rds"))
  #
  # if(!file.exists(file_warning_deprecated)){
  #   msg1 = "\n!!! The use of dim columns is DEPRECATED"
  #   msg2 = "!!! First, please use datasets' name as input of this function"
  #   msg3 = "!!! Then, please use new columns' name instead as FREQ and INDICATEUR etc"
  #   msg4 = "This message is displayed once per R session"
  #   msg = sprintf("%s\n%s\n%s\n%s\n", msg1, msg2, msg3, msg4)
  #   message(crayon::style(msg, "red"))
  #   save(msg, file = file_warning_deprecated)
  # }

  if(!is.null(dataset)){
    dataset_hash = paste0(dataset, collapse = "_")

    dataset_metadata_file_cache = file.path(insee_local_dir,
                                            paste0(openssl::md5(sprintf("%s_metadata_file", dataset_hash)), ".rds"))

  }else{
    dataset_metadata_file_cache = metadata_file_cache
  }

  dataset_list = suppressMessages(get_dataset_list()$id)

  if(!is.null(dataset_list)){
    if("SERIES_BDM" %in% dataset_list){
      dataset_list = dataset_list[-which(dataset_list == "SERIES_BDM")]
    }

    if(!is.null(dataset)){
      dataset_selected = which(dataset %in% dataset_list)

      if(length(dataset_selected) == 0){
        warning("None dataset provided is among INSEE's datasets \nGet the list with get_dataset_list()")
        return(NULL)
      }else{
        dataset = dataset[dataset_selected]
      }
    }
  }

  #
  # Check when was the last metadata update, update triggered if the metadata is older than 6 months
  #

  auto = FALSE

  if(file.exists(metadata_file_cache_date)){

    date_last_update = readRDS(metadata_file_cache_date)

    if(difftime(insee_today_date, date_last_update, units = "days") > 90){
      update = TRUE
      auto = TRUE

      msg = "\nMetadata is older than 3 months"
      message(crayon::style(sprintf("%s", msg), "red"))
    }

  }else{
    list_file_insee_local_dir = list.files(insee_local_dir)

    if(length(list_file_insee_local_dir) > 1){
      msg = "\nMetadata date file is missing"
    }else{
      msg = "\nMetadata files are missing"
    }

    message(crayon::style(sprintf("%s", msg), "red"))

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
    if(all(class(idbank_list) != "data.frame")){
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
        msg1bis = "\nEither because it is older than 3 months, or because some files are missing"
        # msg1 = sprintf("Metadata update has been triggered automatically%s", msg1bis)
        msg1 = "Metadata update has been triggered automatically"
      }else{
        msg1 = "\nMetadata update has been triggered manually"
      }

      msg2 = "\nIt may last several minutes"
      message(crayon::style(sprintf("%s %s", msg1, msg2), "red"))

      # delete all metadata
      clean_insee_folder()

      idbank_list = try(download_idbank_list(dataset = dataset_list,
                                             label = TRUE), silent = TRUE)

      idbank_list = try(download_idbank_list(), silent = TRUE)

      if("try-error" %in% class(idbank_list)){

        idbank_list = clean_table(
          dplyr::filter(.data = idbank_list_internal, .data$nomflow %in% dataset)
        )

        msg1 = "\n\nIdbank list download failed"
        msg2 = "\nPackage's internal data has been used instead"
        msg3 = "\nPlease contact the package maintainer if this error persists"
        warning(sprintf("%s %s %s", msg1, msg2, msg3))

        idbank_list = tibble::as_tibble(idbank_list)

        return(idbank_list)

      }else{

        saveRDS(idbank_list, file = metadata_file_cache)
        saveRDS(insee_today_date, file = metadata_file_cache_date)

        msg = sprintf("\nData cached : %s", metadata_file_cache)
        message(crayon::style(msg, "green"))
      }

    }

  idbank_list = suppressMessages(
    read_dataset_metadata(dataset = dataset,
                          dataset_metadata_file_cache = dataset_metadata_file_cache)
  )

  # delete dim columns
  col_to_keep = names(idbank_list)[!names(idbank_list) %in% c(paste0("dim", 1:50))]

  idbank_list = idbank_list[,col_to_keep]

  idbank_list = tibble::as_tibble(idbank_list)

  return(idbank_list)

}

