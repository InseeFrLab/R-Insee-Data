#' Split the title column in several columns
#'
#' @details The number of separators in the official INSEE title can vary and is not normalized. Beware all title columns created may not be a cleaned dimension label.
#' @param df a dataframe containing a title column
#' @param n_split number of new columns, by default the maximum is chosen
#' @param title_col_name the column name to be splitted, if missing it will be either TITLE_EN
#' @param lang by default it returns an English title (its default value is "en"), any other value returns a French title
#' @examples
#' \donttest{
#' # quarterly payroll enrollment in the construction sector
#' data_raw = get_insee_idbank("001577236")
#'
#' data = data_raw %>%
#'   split_title(lang = "fr")
#' }
#' @export
split_title = function(df, title_col_name, n_split, lang = "en"){

  insee_title_sep = Sys.getenv("INSEE_title_sep")

  if(missing(title_col_name)){
    if(lang == "en"){
      title_col_name = "TITLE_EN"
    }else{
      title_col_name = "TITLE_FR"
    }
  }

  col_title = which(names(df) == title_col_name)

  if(length(col_title) > 0){

    if(missing(n_split)){
      n_split = max(stringr::str_count(df[[title_col_name]], pattern = insee_title_sep)) + 1
    }

    df = tidyr::separate(data = df, col = title_col_name,
                         into = paste0(title_col_name, 1:n_split),
                         sep = insee_title_sep, fill = "right",
                         extra = "merge", remove = FALSE)
  }
  return(df)
}
