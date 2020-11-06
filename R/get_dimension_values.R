#' @noRd
get_dimension_values = function(dimension){

  insee_sdmx_link_codelist =  Sys.getenv("INSEE_sdmx_link_codelist")
  link = sprintf("%s%s", insee_sdmx_link_codelist, dimension)

  temp_dir = tempdir()
  dimension_file_cache = file.path(temp_dir, paste0(openssl::md5(link), ".rds"))

  if(!file.exists(dimension_file_cache)){
    response = try(httr::GET(link), silent = TRUE)

    if(class(response) != "try-error"){

        response_content = try(httr::content(response, encoding = "UTF-8"), silent = TRUE)

        content_list = xml2::as_list(response_content)
        data = tibble::as_tibble(content_list)
        data_code = try(data[[1]][[2]][["Codelists"]][["Codelist"]], silent = TRUE)

        if(class(data_code) != "try-error"){

          code_element = which(names(data_code) == "Code")

          if(length(code_element) > 0){

            data_code = data_code[code_element]

            data_code[which(names(data_code) == "Code")]

            labels = dplyr::bind_rows(lapply(1:length(data_code), function(i){

              if(attr(data_code[[i]][[1]], "lang") == "fr"){
                label_fr = data_code[[i]][[1]][[1]]
                label_en = data_code[[i]][[2]][[1]]
              }else{
                label_fr = data_code[[i]][[2]][[1]]
                label_en = data_code[[i]][[1]][[1]]
              }

              df = data.frame(code = attr(data_code[[i]], "id"),
                              label_fr = label_fr,
                              label_en = label_en)

              saveRDS(df, file = dimension_file_cache)

              return(df)
            }))

            names(labels) = c(dimension, paste0(dimension, "_label_fr"), paste0(dimension, "_label_en"))

            return(labels)

          }else{return(NULL)}

        }else{return(NULL)}

    }else{return(NULL)}

  }else{
    df = readRDS(dimension_file_cache)
    return(df)
  }
}
