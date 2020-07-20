

.onLoad <- function(libname, pkgname){
  Sys.setenv(INSEE_sdmx_link = "https://bdm.insee.fr/series/sdmx/data")
  Sys.setenv(INSEE_website = "https://www.insee.fr/en/statistiques")
  Sys.setenv(INSEE_idbank_file = "correspondance_idbank_dimension")
  Sys.setenv(INSEE_idbank_path = "fichier/2868055")
  Sys.setenv(INSEE_idbank_nchar = 9)
  Sys.setenv(INSEE_idbank_sep = ";")
}
