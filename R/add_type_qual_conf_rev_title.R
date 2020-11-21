
#' @noRd
add_type_qual_conf_rev_title = function(){

  df = data.frame(
    dimension = c("FREQ",
                  "LAST_UPDATE",
                  "UNIT_MEASURE",
                  "UNIT_MULT",
                  "DECIMALS",
                  "REF_AREA"),
    label_fr = c("P\u00E9riodicit\u00E9",
                 "Date de la derni\U00E8re mise \U00E0 jour",
                 "Unit\u00E9",
                 "Puissance",
                 "Nombre de d\u00E9cimales",
                 "Zone g\u00E9ographique"),
    label_en = c("Frequency",
                 "Last update date",
                 "Unit",
                 "Unit multiplier",
                 "Decimals",
                 "Reference Area"),
    stringsAsFactors = FALSE
  )

  df_status = get_dimension_values("OBS_STATUS", name = TRUE)
  df_qual = get_dimension_values("OBS_QUAL", name = TRUE)
  df_type = get_dimension_values("OBS_TYPE", name = TRUE)
  df_rev = get_dimension_values("OBS_REV", name = TRUE)
  df_conf = get_dimension_values("OBS_CONF", name = TRUE)

  df = dplyr::bind_rows(df, df_status, df_conf, df_qual, df_type, df_rev)

  return(df)
}
