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
