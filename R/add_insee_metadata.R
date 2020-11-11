#' Add metadata to the raw data
#'
#' @details Add metadata to the raw data obtained from get_insee_idbank or get_insee_dataset
#' @param df a dataframe containing data obtained from get_insee_idbank or get_insee_dataset
#' @return a tibble with the data given as parameter plus the corresponding metadata
#' @examples
#' \donttest{
#' library(tidyverse)
#'
#' data =
#'  get_insee_idbank("001694061") %>%
#'  add_insee_metadata()
#' }
#' @export
add_insee_metadata = function(df){

  if(any(class(df) %in% c("data.frame"))){
    if("IDBANK" %in% names(df)){

      list_idbank_selected_df = dplyr::distinct(.data = df, .data$IDBANK)
      list_idbank_selected = dplyr::pull(.data = list_idbank_selected_df, .data$IDBANK)

      idbank_list = suppressMessages(get_idbank_list())

      idbank_list_short = dplyr::filter(.data = idbank_list,
                                        .data$idbank %in% list_idbank_selected)

      list_dataset_selected_df = dplyr::distinct(.data = idbank_list_short, .data$nomflow)
      list_dataset_selected = dplyr::pull(.data = list_dataset_selected_df, .data$nomflow)

      metadata = get_idbank_list(list_dataset_selected)

      metadata = dplyr::filter(.data = metadata,
                                        .data$idbank %in% list_idbank_selected)

      col_to_keep = names(metadata)[!names(metadata) %in% c(names(df), paste0("dim", 1:50))]

      metadata = metadata[,col_to_keep]

      df = dplyr::left_join(df, metadata, by = c("IDBANK" = "idbank"))
    }
  }
  return(df)
}
