
#' @noRd
.onLoad <- function(libname, pkgname){

  Sys.setenv(INSEE_sdmx_link_codelist = "https://www.bdm.insee.fr/series/sdmx/codelist/FR1")
  Sys.setenv(INSEE_sdmx_link_datastructure = "https://www.bdm.insee.fr/series/sdmx/datastructure/FR1")
  Sys.setenv(INSEE_sdmx_link_dataflow = "https://bdm.insee.fr/series/sdmx/dataflow")
  Sys.setenv(INSEE_sdmx_link_idbank = "https://bdm.insee.fr/series/sdmx/data/SERIES_BDM")
  Sys.setenv(INSEE_sdmx_link_dataset = "https://bdm.insee.fr/series/sdmx/data")

  # Sys.setenv(INSEE_idbank_dataset_path = "https://www.insee.fr/en/statistiques/fichier/2868055/2020_correspondance_idbank_dimension.zip")
  # Sys.setenv(INSEE_idbank_dataset_file = "2020_correspondances_idbank_dimension")
  # Sys.setenv(INSEE_idbank_sep = ",")

  Sys.setenv(INSEE_idbank_sep = ";")
  Sys.setenv(INSEE_idbank_dataset_path = "https://www.insee.fr/en/statistiques/fichier/2868055/2021_correspondance_idbank_dimension.zip")
  Sys.setenv(INSEE_idbank_dataset_file = "2021_correspondance_idbank_dimension")

  Sys.setenv(INSEE_last_release_link = "https://bdm.insee.fr/series/sdmx/rss/donnees")
  Sys.setenv(INSEE_idbank_nchar = 9)
  Sys.setenv(INSEE_get_idbank_limit = 1200)
  Sys.setenv(INSEE_sdmx_idbank_limit = 400)

  Sys.setenv(INSEE_title_sep = sprintf(" %s | %s | %s ", "-",
                                       intToUtf8("0x2014"), intToUtf8("0x2013")))
  Sys.setenv(INSEE_value_as_numeric = "TRUE")
  Sys.setenv(INSEE_download_verbose = "TRUE")
  Sys.setenv(INSEE_download_option_idbank_list = "wb")
  Sys.setenv(INSEE_no_cache_use = "FALSE")
  Sys.setenv(INSEE_print_query = "FALSE")
  Sys.setenv(INSEE_today_date = as.character(Sys.Date()))
  Sys.setenv(INSEE_read_sdmx_fast = "FALSE")

  # if different from the default value, it makes an error in create_insee_folder function
  # and all metadata files are stored in tempdir
  Sys.setenv(INSEE_metadata_folder = "")
}
