
#' @noRd
.onLoad <- function(libname, pkgname){
  Sys.setenv(INSEE_sdmx_link_dataflow = "https://bdm.insee.fr/series/sdmx/dataflow")
  Sys.setenv(INSEE_sdmx_link_idbank = "https://bdm.insee.fr/series/sdmx/data/SERIES_BDM")
  Sys.setenv(INSEE_sdmx_link_dataset = "https://bdm.insee.fr/series/sdmx/data")
  Sys.setenv(INSEE_idbank_dataset_path = "https://www.insee.fr/en/statistiques/fichier/2868055/correspondance_idbank_dimension.zip")
  Sys.setenv(INSEE_idbank_dataset_file = "correspondance_idbank_dimension")
  Sys.setenv(INSEE_last_release_link = "https://bdm.insee.fr/series/sdmx/rss/donnees")
  Sys.setenv(INSEE_idbank_nchar = 9)
  Sys.setenv(INSEE_get_idbank_limit = 1200)
  Sys.setenv(INSEE_sdmx_idbank_limit = 400)
  Sys.setenv(INSEE_idbank_sep = ";")
  Sys.setenv(INSEE_title_sep = " - ")
  Sys.setenv(INSEE_value_as_numeric = "TRUE")
  Sys.setenv(INSEE_download_verbose = "TRUE")
  Sys.setenv(INSEE_print_query = "FALSE")
}
