#' @name mhealth.validate
#' @title validate filename or dataframe against mhealth specification
#' @param group_cols numeric or character vector specify columns to be validated as group variable. Only feasible for dataframe validation. Default is NULL, meaning there is no group columns in the dataframe.
#' @import stringr
#' @export
mhealth.validate = function(file_or_df, file_type, group_cols = NULL) {
  if (is.character(file_or_df) & length(file_or_df) == 1) {
    valid = .validate.filename(file_or_df, file_type)
  } else if (is.data.frame(file_or_df)) {
    # validate dataframe
    valid = .validate.dataframe(file_or_df, file_type, group_cols)
  } else{
    valid = FALSE
    message(
      sprintf(
        fmt = "\n
        Input is not feasible for validation,
        input class is: %s,
        but should be filename string or dataframe",
        class(file_or_df)
      )
    )
  }
  return(valid)
}

.validate.filename = function(filename, filetype) {
  # validate number of sections
  tokens = stringr::str_split(filename, "\\.", simplify = TRUE)
  if (length(tokens) != 5 && length(tokens) != 6) {
    valid = FALSE
    message(
      sprintf(
        "\n
        The number of sections separated by '.' is not correct: %s
        There should be 'name', 'ID', 'timestamp', 'filetype', 'extension', 'opt-extension' sections.",
        filename
      )
    )
    return(valid)
  }

  # validate name section
  valid = stringr::str_detect(tokens[1], pattern = mhealth$pattern$filename$NAME)
  if (!valid) {
    message(
      sprintf(
        "\n
        Invalid name section: %s,
        name section should only contain alphabets, numbers and '-'",
        tokens[1]
      )
    )
  }

  # validate ID section
  v_id = stringr::str_detect(tokens[2], pattern = mhealth$pattern$filename$ID)
  valid = valid & v_id
  if (!v_id) {
    message(
      sprintf(
        "\n
        Invalid id section: %s,
        id section should only contain uppercase alphabets, numbers and '-'",
        tokens[2]
      )
    )
  }

  # validate timestamp
  ts = stringr::str_extract(filename, pattern = mhealth$pattern$filename$TIMESTAMP)
  valid = valid &
    .validate.timestamp(ts, format = mhealth$format$filename$TIMESTAMP)

  # validate timezone
  v_tz = stringr::str_detect(tokens[3], pattern = mhealth$pattern$filename$TIMEZONE)
  valid = valid & v_tz
  tz = stringr::str_extract(tokens[3], pattern = mhealth$pattern$filename$TIMEZONE)
  if (!v_tz) {
    message(sprintf(
      "\n
      Invalid time zone format: %s,
      time zone should be like: [M/P]HHmm",
      tz
    ))
  }

  # validate filetype
  v_filetype = tokens[4] == filetype
  valid = valid & v_filetype
  if (!v_filetype) {
    message(
      sprintf(
        "\n
        Invalid file type section: %s,
        file types should be %s",
        tokens[4],
        filetype
      )
    )
  }

  # validate extension
  v_ext = tokens[5] == mhealth$pattern$filename$EXTENSION
  valid = valid & v_ext
  if (!v_ext) {
    message(
      sprintf(
        "\n
        Invalid extension: %s,
        extension should be '%s'",
        tokens[5],
        mhealth$pattern$filename$EXTENSION
      )
    )
  }

  # validate optional extension
  if (length(tokens) == 6) {
    v_optext = tokens[6] == mhealth$pattern$filename$OPT_EXTENSION
    valid = valid & v_optext
    if (!v_optext) {
      message(
        sprintf(
          "\n
          Invalid optional extension: %s,
          optional extension can only be '%s'",
          tokens[6],
          mhealth$pattern$filename$OPT_EXTENSION
        )
      )
    }
  }


  # # validate overall pattern
  # Use this simple method to valid as a whole pattern, but this way we can't get the insightful information where the pattern breaks
  # valid = valid & stringr::str_detect(filename, mhealth$pattern$filename$FILENAME)
  # if (!valid) {
  #   message(
  #     sprintf(
  #       "\n
  #       File name does not match mhealth convention: %s,
  #       As an example: %s",
  #       filename,
  #       mhealth$example$filename[filetype]
  #     )
  #   )
  # }

  return(valid)
}

.validate.dataframe = function(df, filetype, group_cols) {
  required_cols = c(1)
  cols = colnames(df)
  ncols = ncol(df)
  if(filetype == mhealth$filetype$annotation){
    required_cols = 1:4
  }else if(filetype == mhealth$filetype$feature){
    required_cols = 1:3
  }

  # validate number of columns
  if (filetype == mhealth$filetype$sensor ||
      filetype == mhealth$filetype$event) {
    valid = ncols >= 2
    if (!valid) {
      message(
        sprintf(
          "\n
          The dataframe should have at least two columns for %s,
          it only has: %d",
          filetype,
          ncols
        )
      )
      return(valid)
    }
  }

  if (filetype == mhealth$filetype$annotation ||
      filetype == mhealth$filetype$feature) {
    valid = ncols >= 4
    if (!valid) {
      message(
        sprintf(
          "\n
          The dataframe should have at least fhour columns for %s,
          it only has: %d",
          filetype,
          ncols
        )
      )
      return(valid)
    }
  }

  # validate column style
  for (i in 1:ncols) {
    valid = valid & .validate.columnstyle(cols, i)
  }

  # validate first column to be date
  valid = .validate.columndate(df, 1)

  # validate first column header
  valid = valid &
    .validate.columnheader(cols, 1, mhealth$column$TIMESTAMP, filetype)

  # validate start and stop time column header for annotation and feature files
  if (filetype == mhealth$filetype$feature ||
      filetype == mhealth$filetype$annotation) {
    valid = valid &
      .validate.columnheader(cols, 2, mhealth$column$START_TIME, filetype)
    valid = valid &
      .validate.columnheader(cols, 3, mhealth$column$STOP_TIME, filetype)
    # validate second and third column to be date for annotation and feature file
    valid = valid & .validate.columndate(df, 2)
    valid = valid & .validate.columndate(df, 3)
  }

  # validate the annotation name for annotation file
  if(filetype == mhealth$filetype$annotation){
    valid = valid & .validate.columnheader(cols, 4, mhealth$column$ANNOTATION_NAME, filetype)
    valid = valid & .validate.columntype(df, 4, "character")
  }

  # convert group cols to numeric vector
  if(is.character(group_cols)){
    group_cols = sapply(group_cols, function(x){which(x == names(df))}, simplify = TRUE)
  }else if(is.numeric(group_cols)){
  }else{
    message(sprintf(
      "\n
      group columns are not valid vector: %s
      So it will be ignored",
      class(group_cols)
    ))
    group_cols = NULL
  }

  # validate group columns to be numeric or character
  if(!is.null(group_cols)){
    group_cols = setdiff(group_cols, required_cols)
    result = sapply(group_cols, function(x){
      if(x > ncol(df) || x < 1){
        message(sprintf(
          "\n
          group column index %d does not exist",
          x
        ))
        return(FALSE)
      }
      if(!is.character(df[1,x]) && !is.numeric(df[1,x]) && !is.integer(df[1,x])){
        message(sprintf(
          "\n
          group column %s is not in the correct format: %s",
          names(df)[x], class(df[1, x])
        ))
        return(FALSE)
      }
      return(TRUE)
    })
    valid = valid & all(result)
  }

  # validate numerical values for sensor type file
  if (filetype == mhealth$filetype$sensor) {
    validate_cols = 2:ncols
    if(!is.null(group_cols)){
      validate_cols = setdiff(2:ncols, group_cols)
    }

    for (i in validate_cols) {
      valid = valid & .validate.columntype(df, i, "numeric")
    }
  }

  return(valid)
}
