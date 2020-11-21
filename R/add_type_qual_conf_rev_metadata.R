
#' @noRd
add_type_qual_conf_rev_metadata = function(df){

  df_status = get_dimension_values("OBS_STATUS")
  df_qual = get_dimension_values("OBS_QUAL")
  df_type = get_dimension_values("OBS_TYPE")
  df_rev = get_dimension_values("OBS_REV")
  df_conf = get_dimension_values("OBS_CONF")

  if("OBS_STATUS" %in% names(df)){
    df = dplyr::left_join(df, df_status, by = "OBS_STATUS")
  }
  if("OBS_QUAL" %in% names(df)){
    df = dplyr::left_join(df, df_qual, by = "OBS_QUAL")
  }
  if("OBS_TYPE" %in% names(df)){
    df = dplyr::left_join(df, df_type, by = "OBS_TYPE")
  }
  if("OBS_REV" %in% names(df)){
    df = dplyr::left_join(df, df_rev, by = "OBS_REV")
  }
  if("OBS_CONF" %in% names(df)){
    df = dplyr::left_join(df, df_conf, by = "OBS_CONF")
  }

  return(df)
}

