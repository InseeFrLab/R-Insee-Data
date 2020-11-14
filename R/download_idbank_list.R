#' @noRd
download_idbank_list = function(dataset = NULL, label = FALSE){

  # insee_download_verbose = if(Sys.getenv("INSEE_download_verbose") == "TRUE"){TRUE}else{FALSE}
  insee_download_option_idbank_list = Sys.getenv("INSEE_download_option_idbank_list")
  file_to_dwn = Sys.getenv("INSEE_idbank_dataset_path")
  mapping_file_pattern = Sys.getenv("INSEE_idbank_dataset_file")

  mapping_file_sep = Sys.getenv("INSEE_idbank_sep")
  idbank_nchar = as.numeric(Sys.getenv("INSEE_idbank_nchar"))

  if(is.na(idbank_nchar)){idbank_nchar = 9}

  temp_file = tempfile()
  temp_dir = tempdir()

 idbank_list_file_cache = file.path(temp_dir,
                                 paste0(openssl::md5(file_to_dwn), ".rds"))

 if(!file.exists(idbank_list_file_cache)){

   dwn = utils::download.file(file_to_dwn, temp_file,
                              mode = insee_download_option_idbank_list, quiet = TRUE)

   uzp = utils::unzip(temp_file, exdir = temp_dir)

   mapping_file = file.path(temp_dir, list.files(temp_dir, pattern = mapping_file_pattern)[1])
   # load data
   mapping = utils::read.delim(mapping_file, sep = mapping_file_sep,
                               stringsAsFactors = F)

   saveRDS(mapping, file = idbank_list_file_cache)
 }else{
   mapping = readRDS(idbank_list_file_cache)
 }

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

      if(length(dataset_in_list) > 1){
        pb = utils::txtProgressBar(min = 1, max = length(dataset_in_list), initial = 1, style = 3)
      }

      mapping_final =
        dplyr::bind_rows(
          lapply(1:length(dataset_in_list),
                 function(j){

                   dataset_name = dataset_in_list[j]

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

                     mapping_dataset_sep = clean_table(set_metadata_col(mapping_dataset_sep))

                     dataset_metadata_file_cache = file.path(rappdirs::user_data_dir("insee"),
                                                             paste0(openssl::md5(sprintf("%s_metadata_file", dataset_name)), ".rds"))

                     saveRDS(mapping_dataset_sep, file = dataset_metadata_file_cache)

                     if(length(dataset_in_list) > 1){
                       utils::setTxtProgressBar(pb, j)
                     }

                     return(mapping_dataset_sep)
                   }else{
                     return(mapping_dataset)
                   }


                 }))

    }else{
      idbank_list_defaul_value = TRUE
      warning("Dataset names do not exist, the table by default is provided")
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

  mapping_final = set_metadata_col(mapping_final)

  return(mapping_final)
}

#' @noRd
set_metadata_col = function(mapping_final){

  idbank_nchar = as.numeric(Sys.getenv("INSEE_idbank_nchar"))

  if(is.na(idbank_nchar)){idbank_nchar = 9}

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

  mapping_final = dplyr::mutate(.data = mapping_final,
                                idbank = purrr::map_chr(.data$idbank, add_zero))

  # mapping_final[,"idbank"] = vapply(mapping_final[,"idbank"], add_zero, "")

  # if("n_series" %in% names(mapping_final)){
  #   # mapping_final[,"n_series"] = as.numeric(as.character(mapping_final[,"n_series"]))
  #   mapping_final = dplyr::mutate(.data = mapping_final,
  #                                 n_series = as.numeric(as.character(.data$n_series)))
  # }

  mapping_final = tibble::as_tibble(mapping_final)

  return(mapping_final)
}
