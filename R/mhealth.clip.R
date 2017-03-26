#' @name mhealth.clip
#' @title Clip mhealth dataframe to the given start and stop time
#' @import plyr
#' @export

mhealth.clip = function(df, start_time, stop_time, file_type) {
  tzone = lubridate::tz(df[[mhealth$column$TIMESTAMP]][1])
  if(is.character(start_time)){
    start_time = as.POSIXct(start_time, tz = tzone)
  }
  if(is.character(stop_time)){
    stop_time = as.POSIXct(stop_time, tz = tzone)
  }

  if (file_type == mhealth$filetype$annotation) {
    mask = df[[mhealth$column$STOP_TIME]] >= start_time &
      df[[mhealth$column$START_TIME]] <= stop_time
    sub_df = df[mask, ]
    if (nrow(sub_df) == 0) {
      sub_df[1, 1:3] = list(start_time, start_time, stop_time)
      return(sub_df)
    }
    sub_df = plyr::adply(sub_df, .margins = 1, function(row) {
      if (row[1, mhealth$column$START_TIME] < start_time) {
        row[1, mhealth$column$START_TIME] = as.POSIXct(start_time)
      }
      if (row[1, mhealth$column$STOP_TIME] > stop_time) {
        row[1, mhealth$column$STOP_TIME] = as.POSIXct(stop_time)
      }
      return(row)
    })
  } else if (file_type == mhealth$filetype$sensor ||
             file_type == mhealth$filetype$event) {
    mask = df[[mhealth$column$TIMESTAMP]] >= start_time &
      df[[mhealth$column$TIMESTAMP]] <= stop_time
    sub_df = df[mask, ]
  } else if (file_type == mhealth$filetype$feature) {
    mask = df[[mhealth$column$START_TIME]] >= start_time &
      df[[mhealth$column$STOP_TIME]] <= stop_time
    sub_df = df[mask, ]
  } else {
    message(sprintf("File type: %s is not supported, return NULL", file_type))
    sub_df = NULL
  }

  return(sub_df)
}
