#' @name mhealth.extract_characteristics
#' @title extract data characteristics (time or spectrum) from each column of a sensor, feature or annotation data file
#' @note Check NULL for returned value.
#' @export
#' @import plyr reshape2 moments stats
#' @param preset predefined feature sets, see description for detailed definition for each set
#' @param group_cols default is empty, if so, it will extract on all rows

mhealth.extract_characteristics = function(df, file_type, select_cols, group_cols = c(), preset = "stat"){
  valid = mhealth.validate(df, file_type = file_type, group_cols = group_cols)
  if(valid){
    gcols = names(df)[.convert.column_input(df, group_cols)]
    scols = names(df)[.convert.column_input(df, select_cols)]
    if(file_type == "sensor") id.vars = c(mhealth$column$TIMESTAMP, gcols)
    else if(file_type == "feature") id.vars = c(mhealth$column$TIMESTAMP, mhealth$column$START_TIME, mhealth$column$STOP_TIME, gcols)
    else if(file_type == "annotation") id.vars = c(mhealth$column$TIMESTAMP, mhealth$column$START_TIME, mhealth$column$STOP_TIME, gcols)
    else{
      message(sprintf(
        "File type: %s is not supported in characteristics extraction", file_type
      ))
      return(NULL)
    }
    melted_df = melt(df, id.vars = c(mhealth$column$TIMESTAMP, gcols), measure.vars = scols, variable.name = "COLUMNS", value.name = "VALUE", factorsAsStrings = FALSE)
    if(preset == "stat"){
      result_df = .extract.stat(melted_df, file_type, gcols)
    }else if(preset == "primary"){
      result_df = .extract.primary(melted_df, file_type, gcols)
    }
    else{
      message(sprintf(
        "preset: %s is currently not supported in characteristics extraction", preset
      ))
      return(NULL)
    }

    return(result_df)
  }else{
    return(NULL)
  }
}

.extract.stat = function(melted_df, file_type, group_cols){
  stat_df = ddply(melted_df, .variables = c(group_cols, "COLUMNS"), summarise,
                  N = length(VALUE),
                  MEAN = mean(VALUE),
                  MEDIAN = median(VALUE),
                  MIN = min(VALUE),
                  QUANTILE1 = quantile(VALUE, probs = 0.01),
                  QUANTILE25 = quantile(VALUE, probs = 0.25),
                  QUANTILE75 = quantile(VALUE, probs = 0.75),
                  QUANTILE99 = quantile(VALUE, probs = 0.99),
                  MAX = max(VALUE),
                  SD = sd(VALUE),
                  SE = SD /sqrt(N),
                  KUR = kurtosis(VALUE),
                  SKEW = skewness(VALUE),
                  MOMENT1ST = moment(VALUE, order = 1, central = TRUE)
  )

  if(file_type == "sensor"){
    stat_df2 = ddply(melted_df, .variables = c(group_cols, "COLUMNS"), function(segment){
      result = data.frame(ts = segment[1, mhealth$column$TIMESTAMP],
                        start.time = segment[1, mhealth$column$TIMESTAMP],
                        stop.time = segment[nrow(segment), mhealth$column$TIMESTAMP],
                        duration = as.numeric(segment[nrow(segment), mhealth$column$TIMESTAMP] - segment[1, mhealth$column$TIMESTAMP], units = "secs"), stringsAsFactors = FALSE)
      colnames(result) = c(mhealth$column$TIMESTAMP, mhealth$column$START_TIME, mhealth$column$STOP_TIME, "DURATION")
      return(result)
    })
    stat_df = join(stat_df2, stat_df)
  }
  return(stat_df)
}

.extract.primary = function(melted_df, file_type, group_cols){
  stat_df = ddply(melted_df, .variables = c(group_cols, "COLUMNS"), summarise,
                  MEAN = mean(VALUE),
                  MEDIAN = median(VALUE),
                  MIN = min(VALUE),
                  QUANTILE1 = quantile(VALUE, probs = 0.01),
                  QUANTILE25 = quantile(VALUE, probs = 0.25),
                  QUANTILE75 = quantile(VALUE, probs = 0.75),
                  QUANTILE99 = quantile(VALUE, probs = 0.99),
                  MAX = max(VALUE),
                  AMP = max(abs(MIN), MAX),
                  SD = sd(VALUE)
  )

  if(file_type == "sensor"){
    stat_df2 = ddply(melted_df, .variables = c(group_cols, "COLUMNS"), function(segment){
      duration = as.numeric(segment[nrow(segment), mhealth$column$TIMESTAMP] - segment[1, mhealth$column$TIMESTAMP], units = "secs")
      N = nrow(segment)
      SR = ceiling(N / duration)
      dominant_freqs = .compute_dominant_freq(segment$VALUE, SR, nth = 3)
      result = data.frame(ts = segment[1, mhealth$column$TIMESTAMP],
                          start.time = segment[1, mhealth$column$TIMESTAMP],
                          stop.time = segment[nrow(segment), mhealth$column$TIMESTAMP],
                          DURATION = duration,
                          SR = SR,
                          DOMINANT_FREQ = dominant_freqs[1,1],
                          DOMINANT_FREQ_SECOND = dominant_freqs[2,1],
                          DOMINANT_FREQ_THIRD = dominant_freqs[3,1],
                          stringsAsFactors = FALSE)
      colnames(result)[1:3] = c(mhealth$column$TIMESTAMP, mhealth$column$START_TIME, mhealth$column$STOP_TIME)
      return(result)
    }, .progress = progress_text(), .inform = TRUE)
  }else{
    return(NULL)
  }
  stat_df = join(stat_df2, stat_df)
  return(stat_df)
}

