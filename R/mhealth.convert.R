#' @name mhealth.convert
#' @title Convert dataframe to be compatible with mhealth specification
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
    if(is.character(df[1,n]) || is.numeric(df[1,n])) next;
    if(!is.factor(df[1, n])){
      temp_df = .convert.column_to(df, col = n, class_type = "numeric")
    }else{
      temp_df = .convert.column_to(df, col = n, class_type = "character")
    }
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
