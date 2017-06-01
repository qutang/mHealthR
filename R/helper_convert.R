.convert.column_to = function(df, col, class_type, new_name = NULL) {
  if (class(col) == "numeric" ||
      class(col) == "character" ||
      class(col) == "integer") {
    # col should be a number and character
    if (is.character(col))
      valid = length(which(names(df) == col)) != 0
    else if (is.numeric(col))
      valid = col >= 1 & col <= ncol(df)
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
        df[col] = as(df[[col]], class_type)
      }
      if (!is.null(new_name)) {
        if (is.character(col))
          old_name = col
        else if (is.numeric(col))
          old_name = names(df)[col]
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
    # timestamp column should be a number and character
    if (is.character(timestamp))
      valid = length(which(names(df) == timestamp)) != 0
    else if (is.numeric(timestamp) ||
             is.integer(timestamp))
      valid = timestamp >= 1 & timestamp <= ncol(df)
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

      if (is.POSIXt(df[1, timestamp])) {
        # do nothing, already in correct format
      }
      else if (is.character(df[1, timestamp]) &
               is.character(datetime_format) &
               is.character(timezone)) {
        valid = .validate.columndate(df, timestamp, datetime_format = datetime_format)
        if (!valid) {
          message("Not a valid date column")
          return(NULL)
        }
      } else if((is.numeric(df[1, timestamp]) |
                is.double(df[1, timestamp]) |
                is.integer(df[1, timestamp])) &
                is.character(timezone)) {
        valid = TRUE
      } else {
        message(
          sprintf(
            "\n
            You must provide timestamps as character or number, datetime format or time zone as character for conversion"
          )
          )
        return(NULL)
      }
      if (valid) {
        # convert to mhealth timestamp column
        if (is.character(df[1, timestamp])) {
          df[timestamp] = as.POSIXct(df[[timestamp]], tz = timezone, format = datetime_format)
        } else if(is.numeric(df[1, timestamp]) |
                  is.double(df[1, timestamp]) |
                  is.integer(df[1, timestamp])){
          if(nchar(as.character(df[1,timestamp])) >= 13){
            # is in milliseconds
            df[timestamp] = df[[timestamp]] / 1000
          }
          df[timestamp] = as.POSIXct(df[[timestamp]], origin = "1970-01-01", tz = timezone)
        }
        else{
          df[timestamp] = df[[timestamp]]
        }
        old_name = timestamp
        if (is.numeric(timestamp)) {
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
    if (class(try(df[old_name])
    ) != "try-error") {
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

.convert.column_input = function(df, cols) {
  if (is.null(cols))
    return(cols)
  if (is.character(cols)) {
    cols = sapply(cols, function(x) {
      pos = which(names(df) == x)
      if (length(pos) == 0) {
        message(sprintf("\n
                        These columns are not existing: %s",
                        x))
        return(-1)
      } else{
        return(pos)
      }
    })
  } else if (is.numeric(cols) || is.integer(cols)) {

  } else{
    message(sprintf("\n
                    Wrong format for columns argument: %s",
                    class(cols)))
    cols = NULL
  }

  # existence
  if (is.null(cols))
    return(cols)
  existence = sapply(cols, function(x) {
    return(x > 0 & x <= ncol(df))
  })
  if (!all(existence)) {
    message(sprintf(
      "\n
      These columns are not existing: %s",
      stringr::str_c(cols[!existence], collapse = ",")
    ))
    cols = NULL
  }
  return(cols)
}
