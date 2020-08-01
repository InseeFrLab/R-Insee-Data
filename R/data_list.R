
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

  # download and unzip
  utils::download.file(file_to_dwn, temp_file, mode = "wb", quiet = TRUE)
  utils::unzip(temp_file, exdir = temp_dir)

  # load data
  mapping_file = file.path(temp_dir, list.files(temp_dir, pattern = mapping_file_pattern)[1])
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
  tfile = tempfile()
  on.exit(unlink(tfile))
  dwn = try(utils::download.file(link_dataflow, tfile, mode = "wb", quiet = TRUE), silent = TRUE)

  if(class(dwn) != "try-error"){
    sdmx = rsdmx::readSDMX(tfile, isURL = FALSE)
    df = as.data.frame(sdmx)
  }else{
    df = NULL
  }
  return(df)
}


