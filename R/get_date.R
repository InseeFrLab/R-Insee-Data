
#' @noRd
get_date = function(date, freq){

  freq = unique(freq)[1]

  # semester
  if(freq == "S"){
    date = stringr::str_replace_all(date, c("-S1" = "-Q1",
                                            "-S2" = "-Q3"))
  }
  if(freq == "B"){
    # bimonthly
    date = stringr::str_replace_all(date, c("-B1" = "-01",
                                            "-B2" = "-03",
                                            "-B3" = "-05",
                                            "-B4" = "-07",
                                            "-B5" = "-09",
                                            "-B6" = "-11"
                                            ))
  }

  if(freq %in% c("M", "B")){
    # monthly
    date = stringr::str_replace_all(date, "^[0-9]{4}-[0-1][0-9]$", as.character(paste0(date, "-01")))
  }
  if(freq %in% c("S", "T")){
    # quarterly
    date = stringr::str_replace_all(date, "^[0-9]{4}-Q[1-4]$", as.character(lubridate::yq(date)))
  }
  if(freq == "A"){
    # annualy
    date = stringr::str_replace_all(date, "^[0-9]{4}$", as.character(paste0(date, "-01-01")))
  }
  if(freq %in% c("M", "B", "T", "A", "S")){
    date = lubridate::ymd(date)
  }
  return(date)
}
