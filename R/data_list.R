
#' Download a mapping dataset betwen INSEE series keys (idbank) and SDMX series names
#'
#' @details Download a mapping dataset betwen INSEE series keys (idbank) and SDMX series names
#' @param dataset if a dataset name is provided, only a subset of the data is delivered, otherwise
#' all the data is returned
#' @examples
#' \donttest{idbank_list = get_idbank_list()}
#' @export
get_idbank_list = function(
  dataset = NULL
){

  file_to_dwn = Sys.getenv("INSEE_idbank_dataset_path")
  mapping_file_pattern = Sys.getenv("INSEE_idbank_dataset_file")

  mapping_file_sep = Sys.getenv("INSEE_idbank_sep")
  idbank_nchar = as.numeric(Sys.getenv("INSEE_idbank_nchar"))
  if(is.na(idbank_nchar)){idbank_nchar = 9}

  # temporary files
  temp_file = tempfile()
  temp_dir = tempdir()

  mapping_file_cache = file.path(temp_dir, paste0(mapping_file_pattern, ".rds"))

  # download and unzip
  if(!file.exists(mapping_file_cache)){

  utils::download.file(file_to_dwn, temp_file, mode = "wb", quiet = TRUE)
  utils::unzip(temp_file, exdir = temp_dir)

  mapping_file = file.path(temp_dir, list.files(temp_dir, pattern = mapping_file_pattern)[1])
  # load data
  mapping = utils::read.delim(mapping_file, sep = mapping_file_sep, stringsAsFactors = F)

  # filter data
  if(!is.null(dataset)){
    dataset_list = unique(mapping[, "nomflow"])
    if(dataset %in% dataset_list){
      mapping = mapping[which(mapping[, "nomflow"] == dataset),]
    }
  }

  dot_vector = stringr::str_count(mapping$cleFlow, pattern = "\\.")
  n_col = max(dot_vector) + 1

  # split cleFlow column by dot
  mapping_final = tidyr::separate(data = mapping, col = "cleFlow", remove = FALSE,
                                  fill = "right", sep = "\\.", into = paste0("dim", 1:n_col))

  add_zero = function(x, idbank_nchar_arg = idbank_nchar){
    paste0(c(rep("0", idbank_nchar_arg-nchar(x)), x), collapse = "")}

  mapping_final[,"idbank"] = sapply(mapping_final[,"idbank"], add_zero)

  saveRDS(mapping_final, file = mapping_file_cache)

  }else{
    mapping_final = readRDS(mapping_file_cache)
  }

  return(mapping_final)
}

#' Download an INSEE dataset list
#'
#' @details the datasets returned are the ones available through a SDMX query
#' @examples
#' \donttest{insee_dataset = get_dataset_list()}
#'
#' @export
get_dataset_list = function(){

  link_dataflow = Sys.getenv("INSEE_sdmx_link_dataflow")

  df = get_insee(link_dataflow)

  return(df)
}

#' Search among insee dataset names a pattern
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

  # create new french name columns withtout accent
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


