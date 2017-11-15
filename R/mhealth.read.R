#' @name mhealth.read
#' @title Write files in mhealth specification into dataframe
#' @import readr stringr lubridate
#' @export

mhealth.read = function(file, filetype) {
  tz_str = "+0000"

  # validate filename
  valid = mhealth.validate(basename(file), filetype)
  if (!valid) {
    warning(
      sprintf(
        "\n
        The filename is not in mhealth specification,
        try to read in, default time zone offset is UTC"
      )
      )
  } else{
    # parse time zone
    tz_str = stringr::str_extract_all(file, mhealth$pattern$filename$TIMEZONE, simplify = FALSE)
    tz_str = tz_str[length(tz_str)]
    tz_str = stringr::str_replace(tz_str, pattern = "M", replacement = "-")
    tz_str = stringr::str_replace(tz_str, pattern = "P", replacement = "+")
    message(sprintf("\n
                    Time zone offset for the file is: UTC%s",
                    tz_str))
  }

  ncols = readr::count_fields(file, tokenizer_csv(), skip = 0, n_max = 1L)
  date_format = col_datetime(format = mhealth$format$csv$TIMESTAMP)
  coltypes = list(date_format)
  colheaders = c(mhealth$column$TIMESTAMP)
  if (filetype == mhealth$filetype$sensor) {
    for (i in 2:ncols) {
      coltypes = append(coltypes, list(col_double()))
    }
  } else if (filetype == mhealth$filetype$annotation) {
    coltypes = append(coltypes, list(date_format))
    coltypes = append(coltypes, list(date_format))
    coltypes = append(coltypes, list(col_character()))
    colheaders = append(colheaders, mhealth$column$START_TIME)
    colheaders = append(colheaders, mhealth$column$STOP_TIME)
    colheaders = append(colheaders, mhealth$column$ANNOTATION_NAME)
    if (ncols > 4) {
      for (i in 5:ncols) {
        coltypes = append(coltypes, list(col_guess()))
      }
    }
  } else if (filetype == mhealth$filetype$event) {
    for (i in 2:ncols) {
      coltypes = append(coltypes, list(col_guess()))
    }
  } else if (filetype == mhealth$filetype$feature) {
    coltypes = append(coltypes, list(date_format))
    coltypes = append(coltypes, list(date_format))
    colheaders = append(colheaders, mhealth$column$START_TIME)
    colheaders = append(colheaders, mhealth$column$STOP_TIME)
    if (ncols > 3) {
      for (i in 4:ncols) {
        coltypes = append(coltypes, col_guess())
      }
    }
  }

  df = readr::read_csv(file = file,
                       quoted_na = TRUE,
                       col_types = coltypes)
  # convert factors back to characters
  col_classes = sapply(1:ncols, function(i) {
    return(class(df[1, i]))
  })
  factor_cols = which(col_classes == "factor")
  df[, factor_cols] = as.character(df[, factor_cols])

  # enhance column headers
  colnames(df)[1:length(colheaders)] = colheaders
  colnames(df) = toupper(colnames(df))
  df = data.frame(df, stringsAsFactors = FALSE)

  # set correct time zone
  # df[[mhealth$column$TIMESTAMP]] = lubridate::force_tz(df[[mhealth$column$TIMESTAMP]], tzone = timezone.to_location(tz_str)[1])
  # if (filetype == mhealth$filetype$annotation ||
  #     filetype == mhealth$filetype$feature) {
  #   df[[mhealth$column$START_TIME]] = lubridate::force_tz(df[[mhealth$column$START_TIME]], tzone = timezone.to_location(tz_str)[1])
  #   df[[mhealth$column$STOP_TIME]] = lubridate::force_tz(df[[mhealth$column$STOP_TIME]], tzone = timezone.to_location(tz_str)[1])
  # }

  # validate dataframe
  if(!mhealth.validate(df, file_type = filetype)){
    message(sprintf(
      "\n
      The read in data frame does not match mhealth specification, use it with caution"
    ))
  }
  return(df)
}
