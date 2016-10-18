#' @name mhealth.plot
#' @title plot or add to plot of dataframe of different file types in mhealth specification
#' @param size "thumbnail", "poster", "", "publication"
#' @param aspect_ratio width/height
#' @param interactive whether to support zoom in, scrolling, tooltip
#' @export
#' @import ggplot2 reshape2

mhealth.plot = function(p = NULL,
                        df,
                        file_type,
                        interactive = FALSE,
                        divide_by = NULL) {
  valid = mhealth.validate(df, file_type = file_type, group_cols = divide_by)
  if (!valid)
    return(NULL)
  if (file_type == mhealth$filetype$sensor) {
    id = c(mhealth$column$TIMESTAMP)
    if (is.character(divide_by)) {
      id = c(id, divide_by)
    }
    md = reshape2::melt(df, id = id, variable = "series")
    p = .plot.point_numeric(p, md, interactive)
  } else if (file_type == mhealth$filetype$feature) {
    numeric_cols = sapply(df[1, 2:ncol(df)], function(x) {
      is.numeric(x)
    }, simplify = TRUE)
    cols = c(1, numeric_cols)
    id = c(mhealth$column$TIMESTAMP)
    if (is.character(divide_by)) {
      cols = c(cols, which(names(df) == divide_by))
      id = c(id, divide_by)
    }
    df_num = df[cols]
    md = reshape2::melt(df_num, id = id, variable = "series")
    p = .plot.range_numeric(p, md, interactive)
    # deal with categorical columns
  } else if (file_type == mhealth$filetype$annotation) {
    cols = c(2,3,4, which(names(df) == divide_by))
    df_range = df[cols]
    p = .plot.range_categoric(p, df_range, interactive)
  }

  p = p + theme_bw(base_size = 9)
  p = p + theme(legend.position = "top")
  p = p + xlab(label = "") + ylab(label = "")
  if (is.character(divide_by)) {
    p = p + theme(axis.text.x=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank())
    p = p + facet_wrap(divide_by, ncol = 4, scales = "free_x")
  }

  return(p)
}

.plot.point_numeric = function(p, df, interactive) {
  if (!interactive) {
    if (is.null(p)) {
      p = ggplot(df, aes_string(x = mhealth$column$TIMESTAMP, y = "value"))
      p = p + geom_line(aes_string(color = "series"))
    } else{
      p = p + geom_line(data = df,
                        aes_string(
                          x = mhealth$column$TIMESTAMP,
                          y = "value",
                          color = "series"
                        ))
    }
    return(p)
  }
}

.plot.range_categoric = function(p, df, interactive, jitter = TRUE) {
  categories = unique(df[, 3])
  if(jitter){
    ypos = jitter(rep(0, length(categories)), amount = length(categories) /
                    3)
  }else{
    ypos = rep(0, length(categories))
  }

  df["ypos"] = sapply(df[, 3], function(x) {
    ypos[x == categories]
  }, simplify = TRUE)
  if (!interactive) {
    if (is.null(p)) {
      p = ggplot(df)
      p = p + geom_segment(
        aes_string(
          x = mhealth$column$START_TIME,
          xend = mhealth$column$STOP_TIME,
          y = "ypos",
          yend = "ypos",
          color = names(df)[3]
        ),
        alpha = 0.5,
        size = 1.5
      )
    } else{
      p = p + geom_segment(
        data = df,
        aes_string(
          x = mhealth$column$START_TIME,
          xend = mhealth$column$STOP_TIME,
          y = "ypos",
          yend = "ypos",
          color = names(df)[3]
        ),
        alpha = 0.5,
        size = 1.5
      )
    }
    return(p)
  }
}
