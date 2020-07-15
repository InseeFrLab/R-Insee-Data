#' Download a mapping dataset betwen INSEE series keys (idbank) and SDMX series names
#'
#' @details Download a mapping dataset betwen INSEE series keys (idbank) and SDMX series names
#' @examples idbank_all = get_idbank_list()
#' @examples idbank_ipc = get_idbank_list("IPC-2015")
#'
#' @param dataset if a dataset name is provided, only a subset of the data is delivered, otherwise
#' all the data is returned
#' @param insee_path main INSEE link
#' @param mapping_file_path INSEE zip file containting idbank dataset ()
#' @param mapping_file_pattern dataset file name pattern
#' @param mapping_file_sep dataset file separator
#' @param idbank_nchar number of character in an series key (idbank)
#' @export
get_idbank_list = function(
  dataset = NULL,
  insee_path = "https://www.insee.fr/en/statistiques",
  mapping_file_path = "/fichier/2868055/correspondance_idbank_dimension.zip",
  mapping_file_pattern = "correspondance_idbank_dimension",
  mapping_file_sep = ";",
  idbank_nchar = 9
){
  # temporary files
  temp_file = tempfile()
  temp_dir = tempdir()

  options(warn = -1)
  # download and unzip
  utils::download.file(file.path(insee_path, mapping_file_path), temp_file, mode = "wb", quiet = TRUE)
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

  # split cleFlow column by dot
  mapping_split = data.frame(do.call('rbind', strsplit(as.character(mapping$cleFlow), '.', fixed = TRUE)))
  names(mapping_split) = paste0("dim", 1:ncol(mapping_split))

  mapping_final = cbind(mapping, mapping_split)

  add_zero = function(x, idbank_nchar_arg = idbank_nchar){
    paste0(c(rep("0", idbank_nchar_arg-nchar(x)), x), collapse = "")}

  mapping_final[,"idbank"] = sapply(mapping_final[,"idbank"], add_zero)

  return(mapping_final)
}

#' Download an INSEE dataset list
#'
#' @details the datasets returned are the ones available through a SDMX query
#' @param link_dataflow SDMX query link
#' @examples dataset = get_dataset_list()
#'
#' @export
get_dataset_list = function(link_dataflow = "https://bdm.insee.fr/series/sdmx/dataflow"){
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



