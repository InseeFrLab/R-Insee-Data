
#' @noRd
read_sdmx_slow = function(link, step = "1/1"){

  insee_download_verbose = if(Sys.getenv("INSEE_download_verbose") == "TRUE"){TRUE}else{FALSE}

  response = try(httr::GET(link), silent = TRUE)

  if("try-error" %in% class(response)){
    # warning("Wrong query")
    return(NULL)
  }

  response_content = try(httr::content(response, encoding = "UTF-8"), silent = TRUE)

  if(!"try-error" %in% class(response_content)){

    content_list = xml2::as_list(response_content)
    data = tibble::as_tibble(content_list)

    n_data_obs = length(data[[1]])

    if(n_data_obs > 1){

      n_series_tot = sum(unlist(lapply(2:n_data_obs,
                                       function(x){
                                         n_series = length(data[[1]][[x]])

                                         sum(unlist(lapply(1:n_series,
                                                           function(i){
                                                             length(data[[1]][[x]][[i]])
                                                           }
                                         )))

                                       }
      )))

      list_df = list()

      n_series = length(data[[1]][[2]])

      dataflow_dwn = FALSE

      if(n_series == 1){
        if(names(data[[1]][[2]]) == "Dataflows"){
          dataflow_dwn = TRUE

          n_dataflows = length(data[[1]][[2]]$Dataflows)

          for (j in 1:n_dataflows){

            dataset_id = if_null_na(attr(data[[1]][[2]]$Dataflows[[j]], "id"))
            dataset_url = if_null_na(data[[1]][[2]]$Dataflows[[j]]$Annotation[[1]]$AnnotationURL[[1]])
            dataset_n_series = if_null_na(data[[1]][[2]]$Dataflows[[j]]$Annotation[[2]]$AnnotationText[[1]])

            if(!is.null(dataset_n_series)){
              dataset_n_series = stringr::str_replace_all(dataset_n_series, "[:alpha:]|\\s|:", "")
            }

            dataset_name_fr = NA
            dataset_name_en = NA

            lang1 = attr(data[[1]][[2]]$Dataflows[[j]][[2]], "lang")
            name1 = data[[1]][[2]]$Dataflows[[j]][[2]][[1]]

            lang2 = attr(data[[1]][[2]]$Dataflows[[j]][[3]], "lang")
            name2 = data[[1]][[2]]$Dataflows[[j]][[3]][[1]]

            if(!is.null(lang1) & !is.null(name1)){
              if(lang1 == "fr"){
                dataset_name_fr = name1
              }
              if(lang1 == "en"){
                dataset_name_en = name1
              }
            }
            if(!is.null(lang2) & !is.null(name2)){
              if(lang2 == "fr"){
                dataset_name_fr = name2
              }
              if(lang2 == "en"){
                dataset_name_en = name2
              }
            }
            dataflow_df = data.frame(
              id = dataset_id,
              Name.fr = dataset_name_fr,
              Name.en = dataset_name_en,
              url = dataset_url,
              n_series = as.numeric(dataset_n_series),
              stringsAsFactors = FALSE
            )
            list_df[[length(list_df) + 1]] = dataflow_df
          }

          msg = sprintf("%s - Data download & Dataframe build : 100%%", step)
          message(crayon::style(msg, "black"))

          data_final = tibble::as_tibble(dplyr::bind_rows(list_df))
        }
      }

      if(!dataflow_dwn){

        # progress bar
        if(insee_download_verbose){
          if(n_series_tot > 1){

            msg = sprintf("%s - Data download & Dataframe build :", step)
            message(crayon::style(msg, "black"))

            pb = utils::txtProgressBar(min = 1, max = n_series_tot, initial = 1, style = 3)
          }else{
            msg = sprintf("%s - Data download & Dataframe build : 100%%", step)
            message(crayon::style(msg, "black"))
          }
        }
        count_series = 0

        for(j in 2:n_data_obs){

          n_series = length(data[[1]][[j]])

          for (i in 1:n_series) {

            n_obs = length(data[[1]][[j]][[i]])
            count_series = count_series + n_obs

            data_series = lapply(data[[1]][[j]][[i]], attributes)
            data_series = dplyr::bind_rows(data_series)

            metadata = attributes(data[[1]][[j]][[i]])
            names_in_metadata = which(names(metadata) == "names")

            if (length(names_in_metadata) > 0) {
              metadata = metadata[-names_in_metadata]
            }
            if (nrow(data_series) > 0) {
              for (metadata_item in names(metadata)) {
                data_series[, metadata_item] = metadata[[metadata_item]]
              }
              if (all(c("TIME_PERIOD", "FREQ") %in% names(data_series))) {
                col_date = dplyr::pull(.data = data_series, "TIME_PERIOD")
                freq_data = as.character(unique(data_series$FREQ)[1])
                data_series[, "DATE"] = suppressWarnings(get_date(col_date, freq = freq_data))
              }

              list_df[[length(list_df) + 1]] = data_series
            }

            if(insee_download_verbose){
              if(n_series_tot > 1){
                utils::setTxtProgressBar(pb, count_series)
              }
            }
          }
        }

        if(insee_download_verbose){
          if(n_series_tot > 1){
            message(crayon::style("", "black"))
          }
        }

      }

      data_final = dplyr::bind_rows(list_df)

      if(!dataflow_dwn){
        data_final = set_data_col(data_final)
      }

    }else{
      # warning("The query might be either too big or wrongly done, try to modify it, use filter argument if necessary")
      # warning(data[[1]][[1]][["Text"]][[1]])
      # warning("Wrong query")

      data_final = NULL
    }
  }else{
    data_final = NULL
    # print(response)
    # stop("Wrong query")
  }
  return(data_final)
}
