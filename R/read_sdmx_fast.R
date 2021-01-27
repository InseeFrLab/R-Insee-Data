#' @noRd
read_sdmx_fast = function(link, step = "1/1"){

  insee_download_verbose = if(Sys.getenv("INSEE_download_verbose") == "TRUE"){TRUE}else{FALSE}

  if(insee_download_verbose == TRUE){
    msg = sprintf("%s - Data download & Dataframe build", step)
    message(crayon::style(msg, "black"))
  }

  data = try(readsdmx::read_sdmx(link, quiet = TRUE), silent = TRUE)

  if(class(data) != "try-error"){
    if(nrow(data) > 0){
      # data$OBS_VALUE = as.numeric(data$OBS_VALUE)

      data = dplyr::group_by(.data = data, .data$IDBANK)

      data = dplyr::mutate(.data = data,
                           DATE = get_date(.data$TIME_PERIOD, .data$FREQ))

      data = dplyr::ungroup(x = data)

      data_final = set_data_col(data)

      data_final = tibble::as_tibble(data_final)
    }else{
      # warning("Wrong query")
      data_final = NULL
    }
  }else{
    # warning("Wrong query")
    data_final = NULL
  }

  return(data_final)
}
