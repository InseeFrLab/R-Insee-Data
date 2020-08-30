#' Get title from INSEE series idbank
#'
#' @details Query INSEE website to get series title from series key (idbank).
#' Any query to INSEE database can handle around 400 idbanks at maximum, if necessary the idbank list will then be splitted in several lists of 400 idbanks each.
#' Consequently, it is not advised to use it on the whole idbank dataset, the user should filter the idbank dataset first.
#' @param ... list of series key (idbank)
#' @param lang language of the title, by default it is Engligh, if lang is different from "en" then French will be the title's language
#' @return a character vector with the titles
#' @examples
#' #example 1 : industrial production index on manufacturing and industrial activities
#' title = get_insee_title("010537900")
#'
#' \donttest{
#' #example 2 : automotive industry and overall industrial production
#' library(tidyverse)
#'
#'idbank_list = get_idbank_list()
#'
#'idbank_list_selected =
#'  idbank_list %>%
#'  filter(nomflow == "IPI-2015") %>% #industrial production index dataset
#'  filter(dim1 == "M") %>% #monthly
#'  filter(dim5 == "INDICE") %>% #index
#'  filter(dim8  == "CVS-CJO") %>% #Working day and seasonally adjusted SA-WDA
#'  filter(str_detect(dim4,"^29$|A10-BE")) %>% #automotive industry and overall industrial production
#'  mutate(title = get_insee_title(idbank))
#' }
#'
#' @importFrom rlang .data
#' @export
get_insee_title = function(..., lang = "en"){

  if(length(list(...)) == 0){
    msg = "The idbank list is missing"
    cat(crayon::style(msg, "red"))
    return(NULL)
  }else if(length(list(...)) == 1){
    list_idbank = list(...)[[1]]
  }else{
    list_idbank = unlist(list(...))
  }
  list_idbank = unique(list_idbank)

  df_title = get_insee_idbank(list_idbank, lastNObservations = 1, limit = FALSE)

  df_title = dplyr::select(.data = df_title, c("IDBANK", "TITLE_EN", "TITLE_FR"))

  list_idbank_obtained = dplyr::pull(.data = df_title, "IDBANK")
  list_idbank_missing_id = which(!list_idbank %in% list_idbank_obtained)

  if(length(list_idbank_missing_id) > 0){

    df_title_missing = data.frame(
      IDBANK = list_idbank[list_idbank_missing_id],
      TITLE_EN = NA,
      TITLE_FR = NA,
      stringsAsFactors = FALSE)

    df_title = dplyr::bind_rows(df_title, df_title_missing)
  }

  df_title = dplyr::mutate(.data = df_title,
                           IDBANK = factor(.data$IDBANK, levels = list_idbank))

  df_title = dplyr::arrange(.data = df_title, .data$IDBANK)

  if(lang == "en"){
    titles = dplyr::pull(.data = df_title, .data$TITLE_EN)
  }else{
    titles = dplyr::pull(.data = df_title, .data$TITLE_FR)
  }

  return(titles)
}
