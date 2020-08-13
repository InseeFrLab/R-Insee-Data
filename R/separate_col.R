#' @noRd
separate_col = function(df, col, sep, into){

  if(!is.null(df)){
    if(nrow(df) > 0){
      if(col %in% names(df)){

        col_location = which(names(df) == col)

        n_split = length(into)

        output = data.frame(stringr::str_split(string = df[[col]],
                                                  simplify = TRUE,
                                                  pattern = sep, n = n_split),
                            stringsAsFactors = FALSE)

        names(output) = into

        if_void_NA = function(x){
          void_rows = which(x == "")
          if(length(void_rows) > 0){
            x[void_rows] = NA
          }
          return(x)
        }

        output = dplyr::mutate_at(.tbl = output, .vars = into, .funs = if_void_NA)

        if(col_location == ncol(df)){
          df = dplyr::bind_cols(df, output)
        }else{
          df = dplyr::bind_cols(df[,1:col_location], output, df[,(col_location + 1):ncol(df)])
        }
      }
    }
  }

  return(df)
}












