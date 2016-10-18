#' @name mhealth.convert
#' @title mhealth.convert
#' @description Convert dataframe to mhealth specification. The column order of converted dataframe would be required_cols, group_cols. Columns that are not specified in the input arguments will be dropped.
#' @param file_type mhealth specification file types, see `mhealth$filetype` for all supported types
#' @param required_cols the required columns of file types that are in order. E.g. for annotation, timestamp, start and end time and annotation name columns are required in the order; for sensor, timestamp and necessary numeric columns are required; for feature, timestamp, start and end time and numeric or categoric features are required; for event, timestamp, start and end time and numeric or categoric info columns are required.
#' @param group_cols the columns used for indexing or grouping. Will be converted to numeric or if not, character. Default is null, which means no group columns.
#' @param datetime_format in string. Used to convert tiemstamp and start and stop time columns to date object
#' @param timezone in location string. Used to convert timestamp and start and stop time columns to date object
#' @export
#' @import stringr lubridate

mhealth.convert = function(df,
                           file_type,
                           required_cols,
                           group_cols = NULL,
                           datetime_format = NULL,
                           timezone = "UTC"
                           ) {

  if(missing(file_type) || missing(required_cols)){
    message(sprintf(
      "\n
      File type or required columns are missing"
    ))
    return(NULL)
  }

  # validate required_cols format
  required_cols = .convert.column_input(df, required_cols)

  # validate required_cols are compatible with file_type
  if(is.null(required_cols)) return(NULL)
  if(length(required_cols) < 2 && file_type %in% c(mhealth$filetype$sensor, mhealth$filetype$event)){
    message(sprintf(
      "\n
      There should be at least two required columns for %s file",
      file_type
    ))
    return(NULL)
  }else if(length(required_cols) < 4 && file_type %in% c(mhealth$filetype$annotation, mhealth$filetype$feature)){
    message(sprintf(
      "\n
      There should be at least four required columns for %s file",
      file_type
    ))
    return(NULL)
  }

  # timestamp column
  df = .convert.datecolumn(
    df,
    required_cols[1],
    datetime_format = datetime_format,
    timezone = timezone,
    new_name = mhealth$column$TIMESTAMP
  )
  if(is.null(df)) return(df)

  # start and end time column
  if (file_type == mhealth$filetype$annotation ||
             file_type == mhealth$filetype$feature) {
    df = .convert.datecolumn(
      df,
      required_cols[2],
      datetime_format = datetime_format,
      timezone = timezone,
      new_name = mhealth$column$START_TIME
    )
    if(is.null(df)) return(df)

    df = .convert.datecolumn(
      df,
      required_cols[3],
      datetime_format = datetime_format,
      timezone = timezone,
      new_name = mhealth$column$STOP_TIME
    )
    if(is.null(df)) return(df)
  }

  # convert annotation name column
  if (file_type == mhealth$filetype$annotation) {
    # convert annotation column to string and rename
    df = .convert.column_to(df, required_cols[4], class_type = "character", new_name = mhealth$column$ANNOTATION_NAME)
    if(is.null(df)) return(df)
  }

  # convert other columns
  if(file_type == mhealth$filetype$sensor){
    rest_cols = required_cols[-1]
  }
  else if(file_type == mhealth$filetype$annotation){
    rest_cols = required_cols[-(1:4)]
  }else if(file_type == mhealth$filetype$feature){
    rest_cols = required_cols[-(1:3)]
  }else if(file_type == mhealth$filetype$event){
    rest_cols = required_cols[-1]
  }

  for(n in rest_cols){
    if(!is.factor(df[1, n])){
      temp_df = .convert.column_to(df, col = n, class_type = "numeric")
    }else{
      temp_df = .convert.column_to(df, col = n, class_type = "character")
    }
    if(is.character(df[1,n])) next;
    if(is.null(temp_df)) return(NULL)
    if(is.na(temp_df[1, n])){
      if(filetype == mhealth$filetype$sensor){
        message(sprintf(
          "\n
          Required column %s should be convertible to numeric",
          names(temp_df)[n]
        ))
        return(NULL)
      }
      # try to convert to character
      temp_df = .convert.column_to(temp_df, col = n, class_type = "character")
      if(is.null(temp_df)) return(NULL)
      if(is.na(temp_df[1, n])){
        message(sprintf(
          "\n
          Don't know how to convert required column %s to numeric or character",
          names(temp_df)[n]
        ))
        return(NULL)
      }else{
        df = temp_df
      }
    }else{
      df = temp_df
    }
  }

  # convert group columns
  # ignore those in required cols
  group_cols = setdiff(group_cols, required_cols)
  group_cols = .convert.column_input(df, group_cols)
  if(!is.null(group_cols)){
    for(n in group_cols){
      if(!is.factor(df[1, n])){
        temp_df = .convert.column_to(df, col = n, class_type = "numeric")
      }else{
        temp_df = .convert.column_to(df, col = n, class_type = "character")
      }
      if(is.character(df[1,n])) next;
      if(is.null(temp_df)) return(NULL)
      if(is.na(temp_df[1, n])){
        if(filetype == mhealth$filetype$sensor){
          message(sprintf(
            "\n
            Group column %s should be convertible to numeric, ignore",
            names(temp_df)[n]
          ))
        }
        # try to convert to character
        temp_df = .convert.column_to(temp_df, col = n, class_type = "character")
        if(is.null(temp_df)) return(NULL)
        if(is.na(temp_df[1, n])){
          message(sprintf(
            "\n
            Don't know how to convert group column %s to numeric or character. ignore",
            names(temp_df)[n]
          ))
        }else{
          df = temp_df
        }
      }else{
        df = temp_df
      }
    }
  }

  # reorder columns
  df = df[c(required_cols, group_cols)]

  # convert all column headers to uppercase, and exclude illegal characters
  df = .convert.legit_column(df)
  if(is.null(df)) return(NULL)

  return(df)
}

.convert.column_to = function(df, col, class_type, new_name = NULL) {
  if (class(col) == "numeric" ||
      class(col) == "character" ||
      class(col) == "integer") {
    # col should be a number and character
    if(is.character(col)) valid = length(which(names(df) == col)) != 0
    else if(is.numeric(col)) valid = col >= 1 & col <= ncol(df)
    if (!valid)
    {
      message(sprintf(
        "\n
        Can't find column, make sure the column index/name is correct: %s",
        col
      ))
      return(NULL)
    } else{
      # convert to column with new class type
      if (class_type == "character" || class_type == "numeric") {
        df[col] = lapply(df[[col]], function(x) {
          return(as(x, class_type))
        })
      }
      if(!is.null(new_name)){
        if(is.character(col)) old_name = col
        else if(is.numeric(col)) old_name = names(df)[col]
        df = .convert.rename_column(df, old_name, new_name)
      }
    }
  } else{
    message(sprintf(
      "\n
      Column name is not a numerical index or name string, it is %s",
      class(col)
    ))
    return(NULL)
  }
  return(df)
}

.convert.datecolumn = function(df,
                               timestamp,
                               datetime_format,
                               timezone,
                               new_name) {
  if (class(timestamp) == "numeric" ||
      class(timestamp) == "character" ||
      class(timestamp) == "integer") {
    # timestamp should be a number and character
    if(is.character(timestamp)) valid = length(which(names(df) == timestamp)) != 0
    else if(is.numeric(timestamp) || is.integer(timestamp)) valid = timestamp >= 1 & timestamp <= ncol(df)
    if (!valid)
    {
      message(
        sprintf(
          "\n
          Can't find column, make sure the column index/name is correct: %s",
          timestamp
        )
      )
      return(NULL)
    } else{
      # timestamp values must be valid date
      if (is.character(df[1, timestamp]) &
          is.character(datetime_format) &
          is.character(timezone)) {
        valid = .validate.columndate(df, timestamp, datetime_format = datetime_format)
        if (!valid) {
          message("Not a valid date column")
          return(NULL)
        }
      } else{
        message(
          sprintf(
            "\n
            You must provide datetime format and time zone as character for conversion"
          )
          )
        return(NULL)
      }
      if (valid) {
        # convert to mhealth timestamp column
        if (is.character(df[1, timestamp])) {
          df[timestamp] = as.POSIXct(df[[timestamp]], tz = timezone, format = datetime_format)
        } else{
          df[timestamp] = df[[timestamp]]
        }
        old_name = timestamp
        if(is.numeric(timestamp)){
          old_name = names(df)[timestamp]
        }
        df = .convert.rename_column(df, old_name, new_name)
      }
      }
} else{
  message(sprintf(
    "\n
    Column name is not a numerical index or name string, it is %s",
    timestamp
  ))
  return(NULL)
}
  return(df)
}

.convert.rename_column  = function(df, old_name, new_name) {
  if (!is.character(new_name)) {
    message(sprintf("\n
                 The new name has to be a string, but it is %s",
                 class(new_name)))
    return(NULL)
  }
  if (is.character(old_name)) {
    if (class(try(df[old_name])) != "try-error") {
      names(df)[names(df) == old_name] <- new_name
      return(df)
    } else{
      message(sprintf(
        "\n
        The column refers to does not exist, column name/index: %s",
        old_name
      ))
      return(NULL)
    }
  } else{
    message(sprintf(
      "\n
      The refered column name is not a string, it is %s",
      class(old_name)
    ))
    return(NULL)
  }
  }

.convert.legit_column = function(df) {
  names(df) = toupper(names(df))
  new_names = sapply(stringr::str_extract_all(names(df), "[A-Z0-9_]+", simplify = FALSE), function(x) {
    return(stringr::str_c(x, collapse = ""))
  }, simplify = TRUE)
  names(df) = new_names
  return(df)
}

.convert.column_input = function(df, cols){
  if(is.null(cols)) return(cols)
  if(is.character(cols)){
    cols = sapply(cols, function(x){
      pos = which(names(df) == x)
      if(length(pos) == 0){
        message(sprintf(
          "\n
          These columns are not existing: %s",
          x
        ))
        return(-1)
      }else{
        return(pos)
      }
      })
  }else if(is.numeric(cols) || is.integer(cols)){
  }else{
    message(sprintf(
      "\n
      Wrong format for columns argument: %s",
      class(cols)
    ))
    cols = NULL
  }

  # existence
  if(is.null(cols)) return(cols)
  existence = sapply(cols, function(x){
    return(x > 0 & x <= ncol(df))
    })
  if(!all(existence)){
    message(sprintf(
      "\n
      These columns are not existing: %s",
      stringr::str_c(cols[!existence], collapse = ",")
    ))
    cols = NULL
  }
  return(cols)
}
