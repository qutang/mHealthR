#' @name mhealth.segment
#' @title segment dataframe in mhealth specification by time window
#' @import plyr
#' @export

mhealth.segment = function(df, breaks, file_type){
  if(file_type == mhealth$filetype$sensor || file_type == mhealth$filetype$event){
    segments = as.numeric(cut(df[[mhealth$column$TIMESTAMP]], breaks))
    df[mhealth$column$SEGMENT] = segments
  }else if(file_type == mhealth$filetype$feature){
    segments = as.numeric(cut(df[[mhealth$column$START_TIME]], breaks))
    df[mhealth$column$SEGMENT] = segments
  }else if(file_type == mhealth$filetype$annotation){
    st_segments = seq(from = df[1, mhealth$column$START_TIME], to = df[nrow(df), mhealth$column$STOP_TIME], by = breaks)
    et_segments = c(st_segments[-1], st_segments[length(st_segments)] + diff(st_segments)[1])
    segments = data.frame(st = st_segments, et = et_segments)
    segments = plyr::adply(segments, .margins = 1, function(row){
      sub_df = mhealth.clip(df, start_time = row[1,1], stop_time = row[1, 2], file_type = "annotation")
      sub_df[mhealth$column$SEGMENT] = as.character(row[1,1])
      return(sub_df)
    })
    segments = segments[c(-1,-2)]
    segments[mhealth$column$SEGMENT] = as.numeric(factor(segments[[mhealth$column$SEGMENT]]))
    df = segments
  }

  return(df)
}
