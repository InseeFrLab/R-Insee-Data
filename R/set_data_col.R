#' @noRd
set_data_col = function(data_final){

  insee_value_as_numeric = if(Sys.getenv("INSEE_value_as_numeric") == "TRUE"){TRUE}else{FALSE}

  if(insee_value_as_numeric & "OBS_VALUE" %in% names(data_final)){
    data_final = dplyr::mutate(.data = data_final,
                               OBS_VALUE = as.numeric(as.character(.data$OBS_VALUE)))
  }

  colnames_order = c("DATE", "TIME_PERIOD", "OBS_VALUE", "OBS_STATUS", "OBS_QUAL", "OBS_TYPE",
                     "IDBANK", "FREQ", "TITLE_FR", "TITLE_EN", "LAST_UPDATE", "UNIT_MEASURE",
                     "UNIT_MULT", "REF_AREA", "DECIMALS")

  col_common = which(colnames_order %in% names(data_final))

  if(length(col_common) > 0){
    colnames_order = colnames_order[col_common]
  }else{
    colnames_order = names(data_final)
  }

  other_columns_id = which(!names(data_final) %in% colnames_order)
  if(length(other_columns_id) > 0){
    colnames_order = c(colnames_order, sort(names(data_final)[other_columns_id]))
  }

  data_final = dplyr::select(.data = data_final, tidyselect::all_of(colnames_order))

  colnames(data_final) = gsub("\\.|-", "_", colnames(data_final))

  return(data_final)
}
