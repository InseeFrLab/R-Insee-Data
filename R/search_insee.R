
#' Search a pattern among insee datasets and idbanks
#'
#' @details The data related to idbanks is stored internally in the package and might the most up to date.
#' The function ignores accents and cases.
#' @param pattern string used to filter the dataset and idbank list
#' @return the dataset and idbank table filtered with the pattern
#' @examples
#' \donttest{
#' # example 1 : search one pattern, the accents do not matter
#' writeLines("the word 'enqu\U00EAte' (meaning survey in French) will match with 'enquete'")
#' dataset_enquete = search_insee("enquete")
#'
#' # example 2 : search multiple patterns
#' dataset_survey_gdp = search_insee("Survey|gdp")
#'
#' # example 3 : data about paris
#' data_paris = search_insee('paris')
#'
#' # example 4 : all data
#' data_all = search_insee()
#' }
#' @export
search_insee = function(pattern = '.*'){

  if(is.null(pattern)){pattern = '.*'}
  if(pattern == ''){pattern = '.*'}

  dataset_list = get_dataset_list()

  # create new french name column withtout accent
  dataset_list = dplyr::mutate(.data = dataset_list,
                               nomflow = .data$id,
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


  idbank_list_search = dplyr::select(.data = idbank_list_internal,
                                     .data$nomflow, .data$idbank, .data$title_fr, .data$title_en)

  idbank_list_search = dplyr::mutate(.data = idbank_list_search,
                                     stop_var = dplyr::case_when(stringr::str_detect(title_en,
                                                                                     stringr::regex('stopped series', ignore_case = TRUE)) ~ 1,
                                                                 TRUE ~ 0))

  idbank_list_search = dplyr::arrange(.data = idbank_list_search, .data$stop_var)

  idbank_list_search = dplyr::mutate(.data = idbank_list_search,
                                     title_fr_accent = iconv(.data$title_fr,
                                                             from = "UTF-8", to = 'ASCII//TRANSLIT'))

  idbank_list_search = dplyr::filter_at(
    .tbl = idbank_list_search,
    .vars = dplyr::vars(.data$title_en, .data$title_fr, .data$title_fr_accent),
    .vars_predicate = dplyr::any_vars(stringr::str_detect(.data$.,
                                                          stringr::regex(pattern, ignore_case = TRUE))))

  idbank_list_search = dplyr::select(.data = idbank_list_search,
                                     -.data$title_fr_accent, -.data$stop_var)

  idbank_list_search = dplyr::rename(.data = idbank_list_search,
                                   id = .data$idbank)

  dataset_selected = dplyr::rename(.data = dataset_selected,
                                   title_fr = .data$Name.fr,
                                   title_en = .data$Name.en)

  search_results = dplyr::bind_rows(dataset_selected, idbank_list_search)

  search_results = dplyr::select(.data = search_results,
                                 .data$nomflow, .data$id, .data$title_fr, .data$title_en)
  return(search_results)
}


