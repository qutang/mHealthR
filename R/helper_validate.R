.validate.columndate = function(df, i, datetime_format) {
  if(missing(datetime_format)) datetime_format = mhealth$format$csv$TIMESTAMP
  valid = is.POSIXct(df[1, i]) ||
    is.POSIXlt(df[1, i])
  if(is.numeric(i)){
    pos_str = switch(as.character(i),
                     "1" = "st",
                     "2" = "nd",
                     "3" = "rd",
                     "th")
  }else if(is.character(i)){
    pos_str = ""
  }

  if (!valid && !is.character(df[1, i])) {
    message(
      sprintf(
        "\n
        The %s%s column is not a valid date object: %s
        Supported date object: POSIXlt, POSIXct, Date, %s",
        i,
        pos_str,
        class(df[1, i]),
        datetime_format
      )
    )
  }

  if (!valid && is.character(df[1, i])) {
    valid = .validate.timestamp(df[1, i], datetime_format)
  }
  return(valid)
}

.validate.columntype = function(df, i, coltype) {
  col_class = class(df[1, i])
  v = col_class == coltype
  if (!v) {
    message(sprintf(
      "\n
      column %d has wrong data type: %s
      should be %s",
      i,
      col_class,
      coltype
    ))
  }
  return(v)
}

.validate.columnheader = function(col_names, i, header, filetype) {
  v_col = col_names[i] == header
  pos_str = switch(as.character(i),
                   "1" = "st",
                   "2" = "nd",
                   "3" = "rd",
                   "th")
  if (!v_col) {
    message(
      sprintf(
        "\n
        The %d%s column header is: %s
        Should be %s for %s type file",
        i,
        pos_str,
        col_names[i],
        header,
        filetype
      )
    )
  }
  return(v_col)
}

.validate.columnstyle = function(col_names, i) {
  valid = stringr::str_detect(col_names[i], pattern = mhealth$pattern$csv$COLUMN_STYLE)
  if (!valid) {
    message(
      sprintf(
        "\n
        Column style is invalid: %s
        Column name should only contain uppercase alphabets, digits and '_'",
        col_names[i]
      )
    )
  }
  return(valid)
}

.validate.timestamp = function(ts, format) {
  d = try(as.Date(ts, format = format))
  if (class(d) == "try-error" || is.na(d)) {
    message(
      sprintf(
        "\n
        Timestamp format is not correct or the timestamp string is not a valid date:
        %s,
        The correct format should be: %s",
        ts,
        format
      )
    )
    return(FALSE)
  }
  else
    return(TRUE)
}
