#' @noRd
clean_insee_folder = function(){

  dir_creation_fail = try(create_insee_folder(), silent = TRUE)

  if(!"try-error" %in% class(dir_creation_fail)){
    insee_local_dir = rappdirs::user_data_dir("insee")
  }else{
    insee_local_dir = tempdir()
  }

  if(insee_local_dir == rappdirs::user_data_dir("insee")){

    list_file_insee = file.path(insee_local_dir,
                                list.files(insee_local_dir))

    if(length(list_file_insee) > 0){
      for(file_name in list_file_insee){
        # try(file.remove(file_name), silent = TRUE)
        unlink(file_name, recursive = TRUE)
      }
    }

  }

}

