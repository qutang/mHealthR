#' @name mhealth.validate
#' @title validate filename or dataframe against mhealth specification
#' @import stringr
#' @export
mhealth.validate = function(file_or_df, file_type){
  if(is.character(file_or_df)){
    valid = .validate.filename(file_or_df, file_type)
  }else if(is.data.frame(file_or_df)){
    # validate dataframe
  }else{
    valid = FALSE
    message(sprintf(
      fmt = "\n
       Input is not feasible for validation,
       input class is: %s,
       but should be filename string or dataframe",
                 class(file_or_df)))
  }
  return(valid)
}

.validate.filename = function(filename, filetype){
  # validate overall pattern
  valid = stringr::str_detect(filename, mhealth$pattern$filename$FILENAME)
  if(!valid) message(sprintf(
    "\n
    File name does not match mhealth convention: %s,
    As an example: %s",
    filename, mhealth$example$filename[filetype]
  ))

  # validate timestamp
  ts = stringr::str_extract(filename, pattern = mhealth$pattern$filename$TIMESTAMP)
  valid = valid & .validate.timestamp(ts, format = mhealth$format$filename$TIMESTAMP)

  # validate filetype
  valid = valid & .validate.filetype(filename, filetype)

  return(valid)
}

.validate.dataframe = function(df, filetype){
  # validate first column to be date
  valid = is.POSIXct(df[1,1]) || is.POSIXlt(df[1,1]) || is.Date(df[1,1])
  if(!valid) message()
}

.validate.filetype = function(filename, filetype){
  tokens = stringr::str_split(filename, "\\.", simplify = TRUE)
  if(tokens[length(tokens)] != "gz"){
    type = tokens[length(tokens) - 1]
  }else{
    type = tokens[length(tokens) - 2]
  }
  valid = type == filetype
  if(!valid){
    message(sprintf(
      "\n
      File type string in the filename is not correct: %s,
      It should be: %s, according to the input parameter",
      type, filetype
    ))
  }
  return(valid)
}

.validate.timestamp = function(ts, format){
  d = try( as.Date( ts, format= format ) )
  if( class( d ) == "try-error" || is.na( d ) ){
    message(sprintf(
      "\n
      Filename's timestamp format is not correct or the timestamp is not a valid date:
      %s,
      The correct format should be: %s",
      ts, format
    ))
    return(FALSE)
  }
  else return(TRUE)
}
