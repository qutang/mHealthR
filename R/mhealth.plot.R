#' @name mhealth.plot_timeseries
#' @title Plot time series with annotations
#' @param file_types list of file_types for each input dataframes
#' @param group_cols group column names in character vector to divide the plot into subplot. All dataframes should share the same group column names.
#' @param select_cols list of selected cols for each input data frames to be plotted.
#' @param ncols number of columns in the subplot layout per page. Default is 4 columns.
#' @param nrows number of rows in the subplot layout per page. Default is NULL, which will be calculated automatically by `ncols`, so that ncols * nrows >= total number of subplots. But it should not exceed 6 per page.
#' @export
#' @import ggplot2 reshape2 plyr gridExtra

mhealth.plot_timeseries <- function(dfs,
                                    file_types,
                                    select_cols,
                                    group_cols = NULL,
                                    ncols = 4,
                                    nrows = NULL) {
  # validate input arguments
  if (length(dfs) > 1) {
    stopifnot(is.list(dfs))
  }
  if (length(file_types) > 1) {
    stopifnot(is.character(file_types))
  }
  if (length(select_cols) > 1) {
    stopifnot(is.list(select_cols))
  }
  stopifnot(length(dfs) == length(file_types) &&
              length(dfs) == length(select_cols))

  n_total = length(dfs)

  # Find out the common Y scale
  v_max = c()
  v_min = c()
  for (i in 1:n_total) {
    temp_df = dfs[[i]][.convert.column_input(dfs[[i]], select_cols[[i]])]
    if (file_types[i] == mhealth$filetype$sensor) {
      v_max = max(v_max, max(temp_df))
      v_min = min(v_min, min(temp_df))
    }
  }
  if (is.null(v_max) || is.null(v_min)) {
    range = NULL
  } else{
    range = c(floor(v_min), ceiling(v_max))
  }


  # Get the original time zone
  tz = lubridate::tz(dfs[[1]][1, 1])

  if (is.null(group_cols)) {
    # single plot
    p = ggplot()
    for (i in 1:n_total) {
      p = .plot.timeseries(p,
                           dfs[[i]],
                           select_cols[[i]],
                           file_type = file_types[[i]],
                           range = range)
    }
    p <- p + theme_bw(base_size = 9)
    p <- p + theme(legend.position = "top")
    p <- p + xlab(label = "") + ylab(label = "")
    return(p)
  } else if (is.character(group_cols)) {
    # subplots
    seg_list = lapply(group_cols, function(col) {
      seg = sapply(dfs, function(df) {
        return(unique(df[[col]]))
      }, simplify = FALSE)
      seg = Reduce(function(x, y) {
        unique(c(x, y))
      }, seg[[1]])
      return(seg)
    })
    segs = expand.grid(seg_list, stringsAsFactors = FALSE)
    names(segs) = group_cols

    durations = alply(segs, .margins = 1, function(seg) {
      x_min = NA
      x_max = NA
      for (i in 1:n_total) {
        g_cols = .convert.column_input(dfs[[i]], group_cols)
        mask = Reduce(function(x, y) {
          x & y
        }, lapply(group_cols, function(col) {
          dfs[[i]][[col]] == seg[[col]]
        }))
        df = dfs[[i]][mask, ]
      }
      if (file_types[[i]] == mhealth$filetype$sensor) {
        x_min = min(x_min, df[[mhealth$column$TIMESTAMP]][1], na.rm = TRUE)
        x_max = max(x_max, df[[mhealth$column$TIMESTAMP]][nrow(df)], na.rm = TRUE)
      } else if (file_types[[i]] == mhealth$filetype$annotation) {
        x_min = min(x_min, df[[mhealth$column$START_TIME]][1], na.rm = TRUE)
        x_max = max(x_max, df[[mhealth$column$STOP_TIME]][nrow(df)], na.rm = TRUE)
      }
      x_min = as.POSIXct(x_min, origin = "1970-01-01", tz = tz)
      x_max = as.POSIXct(x_max, origin = "1970-01-01", tz = tz)
      duration = as.numeric(x_max - x_min, units = "secs")
      return(duration)
    })

    common_duration = max(unlist(durations))

    p_list = alply(segs, .margins = 1, function(seg) {
      p = ggplot()
      x_min = NA
      x_max = NA
      for (i in 1:n_total) {
        g_cols = .convert.column_input(dfs[[i]], group_cols)
        mask = Reduce(function(x, y) {
          x & y
        }, lapply(group_cols, function(col) {
          dfs[[i]][[col]] == seg[[col]]
        }))
        df = dfs[[i]][mask, ]
        if (all(!mask))
          return(NA)
        if (i == 1) {
          title_label = stringr::str_c(df[1, g_cols], collapse = "-")
        }
        p = .plot.timeseries(p,
                             df,
                             select_cols[[i]],
                             file_type = file_types[[i]],
                             range = range)
        if (file_types[[i]] == mhealth$filetype$sensor) {
          x_min = min(x_min, df[[mhealth$column$TIMESTAMP]][1], na.rm = TRUE)
          x_max = max(x_max, df[[mhealth$column$TIMESTAMP]][nrow(df)], na.rm = TRUE)
        } else if (file_types[[i]] == mhealth$filetype$annotation) {
          x_min = min(x_min, df[[mhealth$column$START_TIME]][1], na.rm = TRUE)
          x_max = max(x_max, df[[mhealth$column$STOP_TIME]][nrow(df)], na.rm = TRUE)
        }
      }
      x_min = as.POSIXct(x_min, origin = "1970-01-01", tz = tz)
      x_max = as.POSIXct(x_max, origin = "1970-01-01", tz = tz)
      xlabel = sprintf("%s - %s",
                       format(x_min, "%H:%M:%OS"),
                       format(x_max, "%H:%M:%OS"))
      breaks = round(common_duration / 20)
      p = p + xlim(x_min, x_max)
      p = p + scale_color_discrete(guide = FALSE)
      p = p + scale_x_datetime(date_breaks = paste(breaks, "secs"), date_minor_breaks = paste(breaks / 2, "secs"))
      p <- p + theme_bw(base_size = 9)
      p <- p + theme(legend.position = "top")

      p <-
        p + xlab(label = xlabel) + ylab(label = "") + ggtitle(title_label)
      p <- p + theme(
        axis.title.x = element_text(size = 5.5),
        title = element_text(size = 5.5),
        axis.text.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.08),
        panel.grid.minor.y = element_line(color = "gray", size = 0.04),
        strip.background = element_blank()
      )
      return(p)
    })
    p_result = list()
    i = 1
    for (p in p_list) {
      if (is.ggplot(p)) {
        p_result[[i]] = p
        i = i + 1
      }
    }
    if(is.null(nrows)){
      nrows = min(ceiling(length(p_result) / ncols ), 6)
    }else{
      nrows = min(nrows, 6)
    }
    return(gridExtra::marrangeGrob(
      p_result,
      ncol = ncols,
      nrow = nrows
    ))
  } else{
    stop("group_cols is illegal input, must be character vector with column names")
  }
}

#' @name mhealth.plot_instance
#' @title Plot instance (each row in a feature data file) with bar chart, so taht easy to analyze and compare features for different instances.
#' @param df feature set file
#' @param select_cols selected cols for input data frame to be plotted.
#' @param group_cols group cols that contains extra information about the row to be displayed
#' @param ncols number of columns in the subplot layout per page. Default is 4 columns.
#' @param nrows number of rows in the subplot layout per page. Default is NULL, which will be calculated automatically by `ncols`, so that ncols * nrows >= total number of subplots. But it should not exceed 6 per page.
#' @export
#' @import ggplot2 reshape2 plyr gridExtra

mhealth.plot_instance <- function(df,
                                   select_cols,
                                   group_cols = NULL,
                                   ncols = 4,
                                   nrows = NULL) {
  # validate input arguments

  s_cols = .convert.column_input(df, select_cols)
  g_cols = .convert.column_input(df, group_cols)
  if(mhealth.validate(df, file_type = "feature")){
    df_standard = df
    df_standard[s_cols] = lapply(df[s_cols], function(col_data){
      result = (col_data - mean(col_data, na.rm = TRUE)) / sd(col_data, na.rm = TRUE)
      return(result)
    })
  }
  y_min = floor(min(df_standard[s_cols]))
  y_max = ceiling(max(df_standard[s_cols]))
  p_list = lapply(1:nrow(df), function(i){
    title_label = ""
    if(!is.null(g_cols)){
      title_label = stringr::str_c(df[i, g_cols], collapse = "-")
    }
    xlabel = sprintf("%s - %s",
                     format(df[i, 2], "%H:%M:%OS"),
                     format(df[i, 3], "%H:%M:%OS"))
    p = ggplot()
    p = .plot.numeric_row(p = p, df = df[i,c(1,2,3,s_cols)], df_standard = df_standard[i, c(1,2,3,s_cols)])
    p = p + ggtitle(title_label) + ylab("") + xlab(xlabel)
    p = p + ylim(y_min, y_max)
    p <- p + theme_bw(base_size = 9)
    p <- p + theme(
      legend.position = "top",
      axis.title.x = element_text(size = 5.5),
      title = element_text(size = 5.5),
      axis.text.x = element_text(size = 1.5, angle = 30),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank()
    )
    return(p)
  })
  if(is.null(nrows)){
    nrows = min(ceiling(length(p_list) / ncols), 6)
  }else{
    nrows = min(nrows, 6)
  }

  return(gridExtra::marrangeGrob(
    p_list,
    ncol = ncols,
    nrow = nrows
  ))
}
