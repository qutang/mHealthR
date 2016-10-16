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
  if(missing(timestamp_col)){
    message("\n
            timestamp_col is missing")
    return(NULL)
  }

  orders = c(mhealth$column$TIMESTAMP)

  # timestamp column
  df = .convert.datecolumn(
    df,
    timestamp_col,
    datetime_format = datetime_format,
    timezone = timezone,
    new_name = mhealth$column$TIMESTAMP
  )
  if(is.null(df)) return(NULL)

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
        if(is.null(df)) return(NULL)
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
    if(is.null(df)) return(NULL)

    df = .convert.datecolumn(
      df,
      stop_time_col,
      datetime_format = datetime_format,
      timezone = timezone,
      new_name = mhealth$column$STOP_TIME
    )
    if(is.null(df)) return(NULL)

    orders = c(orders,
               mhealth$column$START_TIME,
               mhealth$column$STOP_TIME)
    if (file_type == mhealth$filetype$annotation) {
      # convert annotation column to string and rename
      df = .convert.column_to(df, annotation_name_col, class_type = "character", new_name = mhealth$column$ANNOTATION_NAME)
      if(is.null(df)) return(NULL)
      orders = c(orders, mhealth$column$ANNOTATION_NAME)
    }
  }

  # reorder according to mhealth specification
  if (!is.null(other_cols_order)) {
    if (is.character(other_cols_order)) {
      orders = c(orders, setdiff(other_cols_order, orders))
    } else if (is.numeric(other_cols_order)) {
      orders = c(orders, setdiff(names(df)[other_cols_order], orders))
    } else if (length(other_cols_order) == 1 && is.na(other_cols_order)){

    }
    else{
      message(sprintf(
        "\n
        Unknown type in other cols order: %s",
        class(other_cols_order)
      ))
      return(NULL)
    }
  }else{
    orders = c(orders, setdiff(names(df), orders))
  }
  orders = na.omit(orders)
  orders = intersect(orders, names(df))
  other_cols_order = unique(other_cols_order)
  df = df[orders]

  # convert factor columns to character
  for(n in names(df)[-1]){
    if(is.factor(df[1, n])){
      df = .convert.column_to(df, n, class_type = "character")
      if(is.null(df)) return(NULL)
    }
  }

  # convert all column headers to uppercase, and exclude illegal characters
  df = .convert.legit_column(df)
  if(is.null(df)) return(NULL)

  return(df)
}

.convert.column_to = function(df, col, class_type, new_name = NULL) {
  if (class(col) == "numeric" ||
      class(col) == "character") {
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
      class(timestamp) == "character") {
    # timestamp should be a number and character
    if(is.character(timestamp)) valid = length(which(names(df) == timestamp)) != 0
    else if(is.numeric(timestamp)) valid = timestamp >= 1 & timestamp <= ncol(df)
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
            You must provide your timestamp column, datetime format and time zone as character for conversion"
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
