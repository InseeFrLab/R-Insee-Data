#' @noRd
download_idbank_list = function(mapping_file_cache = NULL, dataset = NULL, label = FALSE){

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

  }

  dwn = utils::download.file(file_to_dwn, temp_file,
                             mode = insee_download_option_idbank_list, quiet = TRUE)

  uzp = utils::unzip(temp_file, exdir = temp_dir)

  mapping_file = file.path(temp_dir, list.files(temp_dir, pattern = mapping_file_pattern)[1])
  # load data
  mapping = utils::read.delim(mapping_file, sep = mapping_file_sep,
                              stringsAsFactors = F)

  # filter data
  idbank_list_defaul_value = FALSE

  if(!is.null(dataset)){
    dataset_list = unique(mapping[, "nomflow"])

    dataset_param_in_list = which(dataset %in% dataset_list)

    if(length(dataset_param_in_list) > 0){

      dataset_in_list = dataset[dataset_param_in_list]

      mapping = mapping[which(mapping[, "nomflow"] %in% dataset_in_list),]

      dot_vector = stringr::str_count(mapping$cleFlow, pattern = "\\.")
      n_col = max(dot_vector) + 1
      mapping = separate_col(df = mapping, col = "cleFlow",
                             sep = "\\.", into = paste0("dim", 1:n_col))

      file_warning_deprecated = file.path(temp_dir, paste0(openssl::md5("dimdeprecated"), ".rds"))

      if(!file.exists(file_warning_deprecated)){
        msg1 = "!!! in get_idbank_list function, if the dataset name is provided by the user then"
        msg2 = "!!! dim columns will no longer be included in the dataframe in a future version of this package"
        msg3 = "!!! CHANGES ARE NEEDED, Use new column names instead as FREQ INDICATEUR etc."
        msg4 = "!!! if the user relies on column's indices to handle dim columns, this new version is a CODE BREAKER"
        msg5 = "this message is displayed once per R session"
        msg = sprintf("%s\n%s\n%s\n%s\n%s", msg1, msg2, msg3, msg4, msg5)
        warning(msg)
        save(msg, file = file_warning_deprecated)
      }

      mapping_final =
        dplyr::bind_rows(
          lapply(dataset_in_list,
                 function(dataset_name){

                   new_col_names = get_dataset_dimension(dataset = dataset_name)
                   mapping_dataset = dplyr::filter(.data = mapping, .data$nomflow == dataset_name)

                   if(!is.null(new_col_names)){

                     mapping_dataset_sep =
                       separate_col(df = mapping_dataset, col = "cleFlow",
                                    sep = "\\.", into = new_col_names)

                     if(label == TRUE){
                       for(new_col_name in new_col_names){
                         dimension_labels = get_dimension_values(dimension = new_col_name)

                         if(!is.null(dimension_labels)){
                           mapping_dataset_sep = dplyr::left_join(mapping_dataset_sep, dimension_labels, by = new_col_name)
                         }

                       }
                     }

                     return(mapping_dataset_sep)
                   }else{
                     return(mapping_dataset)
                   }


                 }))

    }else{
      idbank_list_defaul_value = TRUE
      warning("Dataset names do not exist")
    }
  }else{
    idbank_list_defaul_value = TRUE
  }

  if(idbank_list_defaul_value == TRUE){
    dot_vector = stringr::str_count(mapping$cleFlow, pattern = "\\.")
    n_col = max(dot_vector) + 1

    mapping_final = separate_col(df = mapping, col = "cleFlow",
                                 sep = "\\.", into = paste0("dim", 1:n_col))

    if(label == TRUE){
      warning("Dataset names are missing, labels will not be provided")
    }
  }

  names(mapping_final) = gsub("-", "_", names(mapping_final))

  label_col = names(mapping_final)[grep("_label_", names(mapping_final))]
  dim_col = names(mapping_final)[grep("^dim", names(mapping_final))]

  if(length(label_col) > 0){
    other_col = names(mapping_final)[which(!names(mapping_final) %in% c(dim_col, label_col))]
    mapping_final = mapping_final[,c(other_col, label_col, dim_col)]
  }else{
    other_col = names(mapping_final)[which(!names(mapping_final) %in% c(dim_col))]
    mapping_final = mapping_final[,c(other_col, dim_col)]
  }

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
