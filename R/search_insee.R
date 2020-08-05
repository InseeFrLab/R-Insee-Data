
#' Search a pattern among insee dataset names
#'
#' @details the function ignores accents and cases
#' @param pattern string used to filtern the dataset list
#' @examples
#' \donttest{
#' # example 1 : search one pattern
#' #the correct word in French would be 'enqu\U00EAte' which means survey
#' dataset_enquete = search_insee("enquete")
#'
#' # example 2 : search multiple patterns
#' dataset_survey_gdp = search_insee("Survey|gdp")
#' }
#' @export
search_insee = function(pattern){

  dataset_list = get_dataset_list()

  # create new french name column withtout accent
  dataset_list = dplyr::mutate(.data = dataset_list,
                               Name.fr_accent = iconv(.data$Name.fr,
                                                      from = "UTF-8", to = 'ASCII//TRANSLIT')
                               )

  # filter the dataset list no matter the cases
  dataset_selected = dplyr::filter_at(
    .tbl = dataset_list,
    .vars = dplyr::vars(.data$Name.en, .data$Name.fr, .data$Name.fr_accent),
    .vars_predicate = dplyr::any_vars(stringr::str_detect(.data$.,
                                                          stringr::regex(pattern, ignore_case = TRUE))))

  dataset_selected = dplyr::select(.data = dataset_selected, -.data$Name.fr_accent)

  return(dataset_selected)
}


