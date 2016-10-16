#' @name mhealth.convert
#' @title Convert dataframe to mhealth specification
#' @export
#' @import stringr lubridate

mhealth.convert = function(df,
                           timestamp_col,
                           datetime_format = NULL,
                           timezone = "UTC",
                           file_type,
                           start_time_col = NULL,
                           stop_time_col = NULL,
                           annotation_name_col = NULL,
                           other_cols_order = NULL) {
  orders = c(mhealth$column$TIMESTAMP)

  # timestamp column
  df = .convert.datecolumn(
    df,
    timestamp_col,
    datetime_format = datetime_format,
    timezone = timezone,
    new_name = mhealth$column$TIMESTAMP
  )

  if (file_type == mhealth$filetype$sensor ||
      file_type == mhealth$filetype$event) {
    # ignore start_time and stop_time and annotation_name
    start_time_col = NULL
    stop_time_col = NULL
    annotation_name_col = NULL
    if(file_type == mhealth$filetype$sensor){
      # convert all to numeric
      for(n in setdiff(names(df), mhealth$column$TIMESTAMP)){
        df = .convert.column_to(df, col = n, class_type = "numeric")
      }
    }
  } else if (file_type == mhealth$filetype$annotation ||
             file_type == mhealth$filetype$feature) {
    df = .convert.datecolumn(
      df,
      start_time_col,
      datetime_format = datetime_format,
      timezone = timezone,
      new_name = mhealth$column$START_TIME
    )
    df = .convert.datecolumn(
      df,
      stop_time_col,
      datetime_format = datetime_format,
      timezone = timezone,
      new_name = mhealth$column$STOP_TIME
    )
    orders = c(orders,
               mhealth$column$START_TIME,
               mhealth$column$STOP_TIME)
    if (file_type == mhealth$filetype$annotation) {
      # convert annotation column to string and rename
      df = .convert.column_to(df, annotation_name_col, class_type = "character", new_name = mhealth$column$ANNOTATION_NAME)
      orders = c(orders, mhealth$column$ANNOTATION_NAME)
    }
  }

  # reorder according to mhealth specification
  if (!is.null(other_cols_order)) {
    if (is.character(other_cols_order)) {
      orders = c(orders, other_cols_order)
    } else if (is.numeric(other_cols_order)) {
      orders = c(orders, names(df)[other_cols_order])
    } else{
      stop(sprintf(
        "\n
        Unknown type in other cols order: %s",
        class(other_cols_order)
      ))
    }
  }else{
    orders = c(orders, setdiff(names(df), orders))
  }
  df = df[orders]

  # convert factor columns to character
  for(n in names(df)[-1]){
    if(is.factor(df[1, n])){
      df = .convert.column_to(df, n, class_type = "character")
    }
  }

  # convert all column headers to uppercase, and exclude illegal characters
  df = .convert.legit_column(df)

  return(df)
}

.convert.column_to = function(df, col, class_type, new_name = NULL) {
  if (class(col) == "numeric" ||
      class(col) == "character") {
    # col should be a number and character
    valid = class(try(df[col])) != "try-error"
    if (!valid)
    {
      stop(sprintf(
        "\n
        Can't find column, make sure the column index/name is correct: %s",
        col
      ))
    } else{
      # convert to column with new class type
      if (class_type == "character" || class_type == "numeric") {
        df[col] = lapply(df[[col]], function(x) {
          return(as(x, class_type))
        })
      }
      if(!is.null(new_name)){
        df = .convert.rename_column(df, col, new_name)
      }
    }
  } else{
    stop(sprintf(
      "\n
      Column name is not a numerical index or name string, it is %s",
      col
    ))
  }
  return(df)
}

.convert.datecolumn = function(df,
                               timestamp,
                               datetime_format,
                               timezone,
                               new_name) {
  if (class(timestamp) == "numeric" ||
      class(timestamp) == "character") {
    # timestamp should be a number and character
    valid = class(try(df[timestamp])) != "try-error"
    if (!valid)
    {
      stop(
        sprintf(
          "\n
          Can't find column, make sure the column index/name is correct: %s",
          timestamp
        )
      )
    } else{
      # timestamp values must be valid date
      if (is.character(df[1, timestamp]) &
          is.character(datetime_format) &
          is.character(timezone)) {
        valid = .validate.columndate(df, timestamp, datetime_format = datetime_format)
        if (!valid) {
          stop()
        }
      } else{
        stop(
          sprintf(
            "\n
            You must provide your timestamp's datetime format in string for conversion"
          )
          )
      }
      if (valid) {
        # convert to mhealth timestamp column
        if (is.character(df[1, timestamp])) {
          df[timestamp] = as.POSIXct(df[[timestamp]], tz = timezone, format = datetime_format)
        } else{
          df[timestamp] = df[[timestamp]]
        }
        df = .convert.rename_column(df, timestamp, new_name)
      }
      }
} else{
  stop(sprintf(
    "\n
    Column name is not a numerical index or name string, it is %s",
    timestamp
  ))
}
  return(df)
}

.convert.rename_column  = function(df, old_name, new_name) {
  if (!is.character(new_name)) {
    stop(sprintf("\n
                 The new name has to be a string, but it is %s",
                 class(new_name)))
  }
  if (is.character(old_name)) {
    if (class(try(df[old_name])) != "try-error") {
      names(df)[names(df) == old_name] <- new_name
      return(df)
    } else{
      stop(sprintf(
        "\n
        The column refers to does not exist, column name/index: %s",
        old_name
      ))
    }
  } else{
    stop(sprintf(
      "\n
      The refered column name is not a string, it is %s",
      class(old_name)
    ))
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
