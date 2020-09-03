
#' @noRd
read_sdmx_fast = function(link, step = "1/1"){

  insee_download_verbose = if(Sys.getenv("INSEE_download_verbose") == "TRUE"){TRUE}else{FALSE}

  if(insee_download_verbose == TRUE){
    msg = sprintf("%s Data download & Dataframe build", step)
    message(crayon::style(msg, "black"))
  }

  data = try(readsdmx::read_sdmx(link, quiet = TRUE), silent = TRUE)

  if(class(data) != "try-error"){
    if(nrow(data) > 0){
      data$OBS_VALUE = as.numeric(data$OBS_VALUE)

      data = dplyr::group_by(.data = data, IDBANK)

      data = dplyr::mutate(.data = data,
                           DATE = insee:::get_date(TIME_PERIOD, unique(FREQ)))

      colnames_order = c("DATE", "TIME_PERIOD", "OBS_VALUE", "OBS_STATUS", "OBS_QUAL", "OBS_TYPE",
                         "IDBANK", "FREQ", "TITLE_FR", "TITLE_EN", "LAST_UPDATE", "UNIT_MEASURE",
                         "UNIT_MULT", "REF_AREA", "DECIMALS")

      data_final = tibble::as_tibble(data[, colnames_order])
    }else{
      data_final = NULL
    }
  }else{
    data_final = NULL
  }

  return(data_final)
}
