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

  response = httr::GET(insee_last_release_link)

  data_xml = xml2::read_xml(response)
  data_list = xml2::as_list(data_xml)
  data_item = data_list$rss$channel

  if(!is.null(data_item)){

    list_item = which(names(data_item) == "item")

    if(length(list_item) > 0){

      list_df = lapply(list_item, function(item){
        data.frame(
          title = data_item[[item]]$title[[1]],
          link = data_item[[item]]$link[[1]],
          description = data_item[[item]]$description[[1]],
          guid = data_item[[item]]$guid[[1]],
          pubDate = data_item[[item]]$pubDate[[1]],
          stringsAsFactors = FALSE)
      })

      data_final = tibble::as_tibble(dplyr::bind_rows(list_df))

    }else{
      data_final = NULL
    }
  }else{
    data_final = NULL
  }

  return(data_final)
}






