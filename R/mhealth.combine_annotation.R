#' @name mhealth.combine_annotation
#' @title Combine annotation rows into mutually exclusive format
#' @export
#' @importFrom plyr ddply
mhealth.combine_annotation = function(df, group_cols = NULL){
  # verify annotation file
  valid = mhealth.validate(df, file_type = mhealth$filetype$annotation)
  if(valid){
    if(is.null(group_cols)){
      result = .combine_annotation.no_group(df, group_cols)
    }else{
      result = plyr::ddply(df, group_cols, function(seg){
        result = .combine_annotation.no_group(seg, group_cols)
        return(result)
      })
    }
    return(result)
  }else{
    stop(sprintf(
      "The input dataframe is not a valid annotation data file, please try to convert it manually or using mhealth.convert"
    ))
  }
}

#' @importFrom plyr ldply
.combine_annotation.no_group = function(df, group_cols = NULL){
  sts = df[, mhealth$column$START_TIME]
  ets = df[, mhealth$column$STOP_TIME]
  ts = sort(unique(floor(as.numeric(c(sts, ets)))), decreasing = FALSE)
  result = plyr::ldply(1:(length(ts)-1), function(i){
    middleTime = mean(c(ts[i], ts[i + 1]))
    criteria = floor(as.numeric(sts)) < as.numeric(middleTime) &
      floor(as.numeric(ets)) >= as.numeric(middleTime)
    labels = unique(df[criteria, mhealth$column$ANNOTATION_NAME])
    labels = sort(labels)
    if(length(labels) > 0){
      result = data.frame(ts = ts[i], st = ts[i], et = ts[i+1], label = paste(labels, collapse = " and "))
    }else{
      result = data.frame(ts = ts[i], st = ts[i], et = ts[i+1], label = "Unlabelled")
    }
    if(!is.null(group_cols)){
      groups = unique(df[group_cols])
      result = cbind(result, groups)
    }
    return(result)
  })
  result = mhealth.convert(result, file_type = mhealth$filetype$annotation, required_cols = 1:4, group_cols = group_cols, datetime_format = mhealth$format$csv$TIMESTAMP, timezone = tz(sts[1]))
  return(result)
}
