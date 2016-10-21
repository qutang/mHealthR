#' @name mhealth.plot_timeseries
#' @title Plot time series with annotations
#' @param file_types list of file_types for each input dataframes
#' @param group_cols group column names in character vector to divide the plot into subplot. All dataframes should share the same group column names.
#' @param select_cols list of selected cols for each input data frames to be plotted
#' @param scales "free_y", "fixed_y"
#' @export
#' @import ggplot2 reshape2 plyr gridExtra

mhealth.plot_timeseries <- function(dfs,
                                    file_types,
                                    select_cols,
                                    group_cols = NULL,
                                    ncols = 4) {
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
  stopifnot(length(dfs) >= length(file_types) &&
              length(dfs) >= length(select_cols))

  n = length(dfs)

  v_max = c()
  v_min = c()
  for(i in 1:n){
    temp_df = dfs[[i]][.convert.column_input(dfs[[i]], select_cols[[i]])]
    if(file_types[i] == mhealth$filetype$sensor){
      v_max = max(v_max, max(temp_df))
      v_min = min(v_min, min(temp_df))
    }
  }
  if(is.null(v_max) || is.null(v_min)){
    range = NULL
  }else{
    range = c(floor(v_min), ceiling(v_max))
  }


  tz = lubridate::tz(dfs[[1]][1,1])

  if (is.null(group_cols)) {
    # single plot
    p = ggplot()
    for (i in 1:n) {
      p = .plot.timeseries(p, dfs[[i]], select_cols[[i]], file_type = file_types[[i]], range = range)
    }
    p <- p + theme_bw(base_size = 9)
    p <- p + theme(legend.position = "top")
    p <- p + xlab(label = "") + ylab(label = "")
    return(p)
  } else if (is.character(group_cols)) {
    seg_list = lapply(group_cols, function(col) {
      seg = sapply(dfs, function(df) {
        return(unique(df[[col]]))
      }, simplify = FALSE)
      seg = Reduce(function(x, y){unique(c(x,y))}, seg[[1]])
      return(seg)
    })
    segs = expand.grid(seg_list, stringsAsFactors = FALSE)
    names(segs) = group_cols
    p_list = alply(segs, .margins = 1, function(seg) {
      p = ggplot()
      x_min = NA
      x_max = NA
      for (i in 1:n) {
        g_cols = .convert.column_input(dfs[[i]], group_cols)
        mask = Reduce(function(x, y) {
          x & y
        }, lapply(group_cols, function(col) {
          dfs[[i]][[col]] == seg[[col]]
        }))
        df = dfs[[i]][mask,]
        p = .plot.timeseries(p, df, select_cols[[i]], file_type = file_types[[i]], range = range)
        if(file_types[[i]] == mhealth$filetype$sensor){
          x_min = min(x_min, df[[mhealth$column$TIMESTAMP]][1], na.rm = TRUE)
          x_max = max(x_max, df[[mhealth$column$TIMESTAMP]][nrow(df)], na.rm = TRUE)
        }else if(file_types[[i]] == mhealth$filetype$annotation){
          x_min = min(x_min, df[[mhealth$column$START_TIME]][1], na.rm = TRUE)
          x_max = max(x_max, df[[mhealth$column$STOP_TIME]][nrow(df)], na.rm = TRUE)
        }
      }
      x_min = as.POSIXct(x_min, origin = "1970-01-01", tz = tz)
      x_max = as.POSIXct(x_max, origin = "1970-01-01", tz = tz)
      xlabel = sprintf("%s - %s",
                       format(x_min, "%H:%M:%OS"),
                       format(x_max, "%H:%M:%OS"))

      p = p + xlim(x_min, x_max)
      p = p + scale_color_discrete(guide = FALSE)
      p <- p + theme_bw(base_size = 9)
      p <- p + theme(legend.position = "top")

      p <- p + xlab(label = xlabel) + ylab(label = "")
      p <- p + theme(
        axis.title.x = element_text(size=5.5),
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank()
      )
      return(p)
    })
    return(gridExtra::marrangeGrob(p_list, ncol = ncols, nrow = min(ceiling(nrow(segs) / ncols), 6)))
    } else{
      stop("group_cols is illegal input, must be character vector with column names")
    }
}

.plot.timeseries = function(p, df, select_cols, file_type, range = NULL){
  s_cols = .convert.column_input(df, select_cols)

  if (file_type == mhealth$filetype$sensor) {
    cols = c(1, s_cols)
    df = df[cols]
    p = .plot.numeric_column(p, df)
    p = p + ylim(range)
  } else if (file_type == mhealth$filetype$annotation) {
    cols = c(1, 2, 3, s_cols)
    df = df[cols]
    p = .plot.categoric_row(p, df, range, jitter = TRUE)
  } else {
    warning(
      sprintf(
        "\nFile type %s is not supported for plotting for now, will skip this data frame",
        file_type
      )
    )
  }
  return(p)
}

  .plot.numeric_column <- function(p, df) {
    df <- reshape2::melt(df,
                         id = mhealth$column$TIMESTAMP,
                         variable.name = "series",
                         value.name = "value")
    p <- p + geom_line(data = df,
                   aes_string(
                     x = mhealth$column$TIMESTAMP,
                     y = "value",
                     color = "series"
                   ))
    return(p)
  }

  .plot.categoric_row = function(p, df, range = NULL, jitter = TRUE) {
    cat_values = unname(unlist(plyr::alply(df[,-c(1,2,3)], 1, function(x){stringr::str_c(x, collapse = ",")}, .dims = FALSE)))
    categories = unique(cat_values)
    df$cat_combined = cat_values

    if (jitter) {
      if(is.null(range)){
        amount = length(categories) / 3

      }else{
        amount = min(abs(range))
      }
      ypos = jitter(rep(0, length(categories)), amount = amount)

    } else{
      ypos = rep(0, length(categories))
    }

    df["ypos"] = sapply(cat_values, function(x) {
      ypos[x == categories]
    }, simplify = TRUE)

    p = p + geom_segment(
      data = df,
      aes_string(
        x = mhealth$column$START_TIME,
        xend = mhealth$column$STOP_TIME,
        y = "ypos",
        yend = "ypos",
        color = "cat_combined"
      ),
      alpha = 0.5,
      size = 1.5
    )
    p = p + geom_text(data = df,
                      aes_string(x = mhealth$column$START_TIME,
                                 y = "ypos",
                                 label = "cat_combined"),
                      size = 2, hjust = 0, vjust = 0)
    return(p)
  }
