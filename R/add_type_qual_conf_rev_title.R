
#' @noRd
add_type_qual_conf_rev_title = function(){

  df = data.frame(
    dimension = c("OBS_QUAL", "OBS_CONF", "OBS_TYPE", "OBS_REV"),
    label_fr = c("Qualit\u00E9", "Confidentialit\u00E9", "Type de donn\u00E9es", "R\u00E9vision"),
    label_en = c("Quality", "Confidentiality", "Type of value", "Revision"),
    stringsAsFactors = FALSE
  )
  return(df)
}
