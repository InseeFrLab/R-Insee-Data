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

    dataset_param_in_list = which(dataset %in% dataset_list)

    if(length(dataset_param_in_list) > 0){

      dataset_in_list = dataset[dataset_param_in_list]

      mapping = mapping[which(mapping[, "nomflow"] %in% dataset_in_list),]

      mapping_final =
        dplyr::bind_rows(
          lapply(dataset_in_list,
                 function(dataset_name){

                   new_col_names = get_dataset_dimension(dataset = dataset_name)

                   mapping_dataset = dplyr::filter(.data = mapping, nomflow == dataset_name)

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
                 }))

    }else{
      warning("Dataset names do not exist")
    }
  }else{

    dot_vector = stringr::str_count(mapping$cleFlow, pattern = "\\.")
    n_col = max(dot_vector) + 1

    mapping_final = separate_col(df = mapping, col = "cleFlow",
                                 sep = "\\.", into = paste0("dim", 1:n_col))

    if(label == TRUE){
      warning("Dataset names are missing, labels will not be provided")
    }
  }

  names(mapping_final) = gsub("-", "_", names(mapping_final))

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
