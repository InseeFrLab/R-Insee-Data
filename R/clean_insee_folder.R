#' @noRd
clean_insee_folder = function(){

  list_file_insee = file.path(rappdirs::user_data_dir("insee"),
                              list.files(rappdirs::user_data_dir("insee")))

  if(length(list_file_insee) > 0){
    for(file_name in list_file_insee){
      file.remove(file_name)
    }
  }
}

