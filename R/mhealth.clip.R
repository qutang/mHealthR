#' @name mhealth.clip
#' @title mhealth.clip
#' @import plyr
#' @export

mhealth.clip = function(df, start_time, stop_time, file_type){
  if(file_type == mhealth$filetype$annotation){
    mask = df[[mhealth$column$STOP_TIME]] >= start_time & df[[mhealth$column$START_TIME]] <= stop_time
    sub_df = df[mask,]
    if(nrow(sub_df) == 0){
      row= data.frame(start_time, start_time, stop_time, NA)
      names(row) = names(sub_df)
      return(row)
    }
    sub_df = plyr::adply(sub_df, .margins = 1, function(row){
      if(row[1,mhealth$column$START_TIME] < start_time){
        row[1,mhealth$column$START_TIME] = as.POSIXct(start_time)
      }
      if(row[1, mhealth$column$STOP_TIME] > stop_time){
        row[1, mhealth$column$STOP_TIME] = as.POSIXct(stop_time)
      }
      return(row)
    })
  }else if(file_type == mhealth$filetype$sensor || file_type == mhealth$filetype$event){
    mask = df[[mhealth$column$TIMESTAMP]] >= start_time & df[[mhealth$column$TIMESTAMP]] <= stop_time
    sub_df = df[mask,]
  }

  return(sub_df)
}
