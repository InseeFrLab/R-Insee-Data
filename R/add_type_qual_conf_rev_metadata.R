
#' @noRd
add_type_qual_conf_rev_metadata = function(df){

  df_status = get_dimension_values("OBS_STATUS")
  df_qual = get_dimension_values("OBS_QUAL")
  df_type = get_dimension_values("OBS_TYPE")
  df_rev = get_dimension_values("OBS_REV")
  df_conf = get_dimension_values("OBS_CONF")

  # df_qual = data.frame(
  #   OBS_QUAL = c("E", "P", "SD", "DEF", "F"),
  #   OBS_QUAL_label_en = c("estimate", "provisional value", "semi-final value", "final value", "forecast"),
  #   OBS_QUAL_label_fr = c("estimation", "valeur provisoire", "valeur semi-d\u00E9finitive", "valeur d\u00E9finitive", "pr\u00E9vision"),
  #   stringsAsFactors = FALSE
  # )
  #
  # df_type = data.frame(
  #   OBS_TYPE = c("A", "U", "O", "ND", "N"),
  #   OBS_TYPE_label_en = c("normal", "atypical", "missing", "not available", "not significant"),
  #   OBS_TYPE_label_fr = c("normal", "atypique", "manquant", "non disponible", "non significatif"),
  #   stringsAsFactors = FALSE
  # )
  # df_rev = data.frame(
  #   OBS_REV = c(1),
  #   OBS_REV_label_en = c("revised"),
  #   OBS_REV_label_fr = c("r\u00E9vis\u00E9"),
  #   stringsAsFactors = FALSE
  # )
  # df_conf = data.frame(
  #   OBS_CONF = c("F", "C", "Q"),
  #   OBS_CONF_label_en = c("public value", "confidential value", "covered by statistical confidentiality"),
  #   OBS_CONF_label_fr = c("donn\u00E9e publique", "donn\u00E9e confidentielle", "couvert par le secret statistique"),
  #   stringsAsFactors = FALSE
  # )

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

