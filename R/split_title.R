#' Split the title column in several columns
#'
#' @details The number of separators in the official INSEE title can vary and is not normalized. Beware all title columns created may not be a cleaned dimension label.
#' @param df a dataframe containing a title column
#' @param n_split number of new columns, by default the maximum is chosen
#' @param title_col_name the column name to be splitted, if missing it will be either TITLE_EN
#' @param lang by default it returns both the French and the English title provided by INSEE
#' @param pattern the value by default is stored in the package and it is advised to use it, but in some cases it is useful to use one's pattern
#' @return the same dataframe with the title column splitted
#' @examples
#' \donttest{
#' library(tidyverse)
#'
#' # quarterly payroll enrollment in the construction sector
#' data_raw = get_insee_idbank("001577236")
#'
#' data = data_raw %>%
#'   split_title()
#' }
#' @export
split_title = function(df, title_col_name, pattern, n_split = "max", lang = NULL){

  if(any(class(df) %in% c("data.frame"))){
    if(!is.null(df)){

      if(missing(pattern)){
        insee_title_sep = Sys.getenv("INSEE_title_sep")
      }else{
        insee_title_sep = pattern
      }

      if(missing(title_col_name)){
        if(is.null(lang)){
          title_col_name = c("TITLE_EN", "TITLE_FR")
        }else{
          if(lang == "en"){
            title_col_name = "TITLE_EN"
          }else{
            title_col_name = "TITLE_FR"
          }
        }
      }

      col_title = which(title_col_name %in% names(df))

      if(length(col_title) > 0 & nrow(df) > 0){

        n_split_max = max(stringr::str_count(df[[title_col_name[1]]], pattern = insee_title_sep), na.rm = TRUE) + 1

        if(n_split == "max"){
          n_split = n_split_max
        }

        n_split = max(1, n_split, na.rm = TRUE)

        n_split = min(n_split_max, n_split, na.rm = TRUE)

        for(i in 1:length(title_col_name)){
          df = separate_col(df = df, col = title_col_name[i],
                            into = paste0(title_col_name[i], 1:n_split),
                            sep = insee_title_sep)
        }

      }
    }
  }


  return(df)
}
