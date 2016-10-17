#' @name mhealth.segment
#' @title segment dataframe in mhealth specification by time window
#' @export

mhealth.segment = function(df, breaks){
  segments = as.numeric(cut(df[[mhealth$column$TIMESTAMP]], breaks))
  df[mhealth$column$SEGMENT] = segments
  return(df)
}
