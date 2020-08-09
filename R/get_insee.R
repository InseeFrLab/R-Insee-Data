#' Get data from INSEE BDM database with a SDMX query link
#'
#' @details Get data from INSEE BDM database with a SDMX query link.
#' This function is mainly for package internal use.
#' It is used by the functions get_insee_dataset, get_insee_idbank and get_dataset_list.
#' The data is cached, hence all queries are only run once per R session.
#'
#' @param link SDMX query link
#' @examples
#' insee_link = "http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM"
#' insee_query = file.path(insee_link, paste0("010539365","?", "firstNObservations=1"))
#' data = get_insee(insee_query)
#'
#' @export
get_insee = function(link){

  if(Sys.getenv("INSEE_value_as_numeric") == "TRUE"){
    insee_value_as_numeric = TRUE
  }else{
    insee_value_as_numeric = FALSE
  }

  if_null_na = function(x) {
    if (is.null(x)) {
      return(NA)
    } else{
      return(x)
    }
  }

  file_cache = file.path(tempdir(), paste0(openssl::md5(link), ".rds"))

  if(!file.exists(file_cache)){

    cat("Data download : \n")
    response = httr::GET(link, httr::progress())
    response_content = try(httr::content(response, encoding = "UTF-8"), silent = TRUE)

    if(!"try-error" %in% class(response_content)){

      content_list = xml2::as_list(response_content)
      data = tibble::as_tibble(content_list)

      n_line = length(data[[1]])

      if(n_line > 1){

        n_series = length(data[[1]][[2]])
        list_df = list()

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
                n_series = dataset_n_series,
                stringsAsFactors = FALSE
              )
              list_df[[length(list_df) + 1]] = dataflow_df
            }

            data_final = tibble::as_tibble(dplyr::bind_rows(list_df))
          }
        }

        if(!dataflow_dwn){

          cat("Dataframe build : \n")

          if(n_series > 1){
            pb = txtProgressBar(min = 1, max = n_series, initial = 1, style = 3)
          }

          for (i in 1:n_series) {

            data_series = lapply(data[[1]][[2]][[i]], attributes)
            data_series = dplyr::bind_rows(data_series)

            metadata = attributes(data[[1]][[2]][[i]])
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

            if(n_series > 1){
              setTxtProgressBar(pb,i)
            }

          }

          data_final = dplyr::bind_rows(list_df)
        }

        if(insee_value_as_numeric & "OBS_VALUE" %in% names(data_final)){
          data_final = dplyr::mutate(.data = data_final,
                                     OBS_VALUE = as.numeric(as.character(.data$OBS_VALUE)))
        }

        if("DATE" %in% names(data_final)){
          col_names_ordered = c("DATE", names(data_final)[which(names(data_final) != "DATE")])
          data_final = dplyr::select(.data = data_final, tidyselect::all_of(col_names_ordered))
        }

      }else{
        warning("The query might be either too big or wrongly done, try to modify it, use filter argument if necessary")
        warning(data[[1]][[1]][["Text"]][[1]])
        data_final = NULL
      }
    }else{
      warning("Wrong query")
      print(response)
      data_final = NULL
    }
    if(!is.null(data_final)){
      saveRDS(data_final, file = file_cache)
      cat(testthat:::colourise(sprintf("\nData cached at %s\n", file_cache), "success"))
    }
  }else{
    cat(testthat:::colourise("Cached data has been used", "success"))
    data_final = readRDS(file_cache)
  }

  return(data_final)
}
