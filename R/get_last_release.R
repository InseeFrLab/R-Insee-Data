#' Get the datasets released in the last 30 days
#'
#' @return a tibble with the data
#' @examples
#' \donttest{
#'last_release = get_last_release()
#' }
#'
#' @export
get_last_release = function(){

  insee_last_release_link = Sys.getenv("INSEE_last_release_link")

  # dir_creation_fail = try(create_insee_folder(), silent = TRUE)
  #
  # if(!"try-error" %in% class(dir_creation_fail)){
  #   insee_data_dir = file.path(rappdirs::user_data_dir("insee"), "data")
  # }else{
  #   insee_data_dir = tempdir()
  # }

  insee_data_dir = tempdir()

  file_cache = file.path(insee_data_dir, paste0(openssl::md5(insee_last_release_link), ".rds"))

  if(!file.exists(file_cache)){
    response = httr::GET(insee_last_release_link)

    data_xml = xml2::read_xml(response)
    data_list = xml2::as_list(data_xml)
    data_item = data_list$rss$channel

    if(!is.null(data_item)){

      list_item = which(names(data_item) == "item")

      if(length(list_item) > 0){

        list_df = lapply(list_item, function(item){
          data.frame(
            title = if_null_na(data_item[[item]]$title[[1]]),
            link = if_null_na(data_item[[item]]$link[[1]]),
            description = if_null_na(data_item[[item]]$description[[1]]),
            guid = if_null_na(data_item[[item]]$guid[[1]]),
            pubDate = if_null_na(data_item[[item]]$pubDate[[1]]),
            stringsAsFactors = FALSE)
        })

        extract_dataset = function(x){
          str_start = stringr::str_locate(x, "\\[")[[1]] + 1
          str_end = stringr::str_locate(x, "\\]")[[1]] - 1
          if(!is.na(str_start) & !is.na(str_end)){
            x = substr(x, str_start, str_end)
          }
          return(x)
        }

        data_final = tibble::as_tibble(dplyr::bind_rows(list_df))

        if("title" %in% names(data_final)){
          data_final = dplyr::mutate(.data = data_final,
                                     dataset = purrr::map_chr(.data$title, extract_dataset))
        }

        saveRDS(data_final, file = file_cache)

      }else{
        data_final = NULL
      }
    }else{
      data_final = NULL
    }
  }else{
    data_final = readRDS(file_cache)
  }

  return(data_final)
}






