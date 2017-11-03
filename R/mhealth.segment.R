#' @name mhealth.segment
#' @title segment dataframe in mhealth specification by time window
#' @importFrom plyr adply
#' @export

mhealth.segment = function(df, breaks, file_type){
  if(file_type == mhealth$filetype$sensor || file_type == mhealth$filetype$event){
    segments = as.numeric(.segment(df[[mhealth$column$TIMESTAMP]], breaks))
    df[mhealth$column$SEGMENT] = segments
  }else if(file_type == mhealth$filetype$feature){
    segments = as.numeric(.segment(df[[mhealth$column$START_TIME]], breaks))
    df[mhealth$column$SEGMENT] = segments
  }else if(file_type == mhealth$filetype$annotation){
    st_segments = seq(from = .segment.floor_date(df[1, mhealth$column$START_TIME], breaks), to = .segment.ceil_date(df[nrow(df), mhealth$column$STOP_TIME], breaks), by = breaks)
    et_segments = st_segments + diff(st_segments)[1]
    segments = data.frame(st = st_segments, et = et_segments)
    segments = plyr::adply(segments, .margins = 1, function(row){
      sub_df = mhealth.clip(df, start_time = row[1,1], stop_time = row[1, 2], file_type = "annotation")
      sub_df[mhealth$column$SEGMENT] = as.character(row[1,1])
      return(sub_df)
    })
    segments = segments[c(-1,-2)]
    segments[mhealth$column$SEGMENT] = as.numeric(factor(segments[[mhealth$column$SEGMENT]]))
    df = segments
    df = na.omit(df)
  }

  return(df)
}

.segment = function(ts, breaks){
  if(missing(breaks) || is.null(breaks)){
    br = ts[1]
    return(br)
  }else{
    ts[1] = .segment.ceil_date(ts[1], breaks)
  }
  segments = cut(ts, breaks= breaks)
  return(segments)
}

.segment.floor_date = function(ts, breaks){
  if(stringr::str_detect(breaks, "sec")){
    ts = lubridate::floor_date(ts, unit = c("second"))
  }else if(str_detect(breaks, "min")){
    ts = lubridate::floor_date(ts, unit = c("minute"))
  }else if(str_detect(breaks, "hour")){
    ts = lubridate::floor_date(ts, unit = c("hour"))
  }else if(str_detect(breaks, "day")){
    ts = lubridate::floor_date(ts, unit = c("day"))
  }
  return(ts)
}

.segment.ceil_date = function(ts, breaks){
  if(stringr::str_detect(breaks, "sec")){
    ts = lubridate::ceiling_date(ts, unit = c("second"))
  }else if(str_detect(breaks, "min")){
    ts = lubridate::ceiling_date(ts, unit = c("minute"))
  }else if(str_detect(breaks, "hour")){
    ts = lubridate::ceiling_date(ts, unit = c("hour"))
  }else if(str_detect(breaks, "day")){
    ts = lubridate::ceiling_date(ts, unit = c("day"))
  }
  return(ts)
}
