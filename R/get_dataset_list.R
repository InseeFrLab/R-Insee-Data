#' Download a full INSEE's dataset list
#'
#' @details the datasets returned are the ones available through a SDMX query
#' @return a tibble with 5 columns :  id, Name.fr, Name.en, url, n_series
#' @examples
#' \donttest{insee_dataset = get_dataset_list()}
#'
#' @export
get_dataset_list = function(){

  link_dataflow = Sys.getenv("INSEE_sdmx_link_dataflow")

  df = get_insee(link_dataflow)
  df = dplyr::filter(.data = df, !is.na(.data$Name.fr))

  return(df)
}
