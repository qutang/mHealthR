#' @name mhealth.plot_timeseries
#' @title Plot time series with annotations
#' @param file_types list of file_types for each input dataframes
#' @param group_cols group column names in character vector to divide the plot into subplot. All dataframes should share the same group column names.
#' @param select_cols list of selected cols for each input data frames to be plotted
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

  if (is.null(group_cols)) {
    p = ggplot()
    n = length(dfs)
    for (i in 1:n) {
      s_cols = .convert.column_input(dfs[[i]], select_cols[[i]])

      if (file_types[[i]] == mhealth$filetype$sensor) {
        cols = c(1, s_cols)
        dfs[[i]] = dfs[[i]][cols]
        p = .plot.numeric_column(p, dfs[[i]])
      } else if (file_types[[i]] == mhealth$filetype$annotation) {
        cols = c(1, 2, 3, s_cols)
        dfs[[i]] = dfs[[i]][cols]
        p = .plot.categoric_row(p, dfs[[i]], jitter = TRUE)
      } else {
        warning(
          sprintf(
            "\nFile type %s is not supported for plotting for now, will skip this data frame",
            file_types[[i]]
          )
        )
      }
    }
    p <- p + theme_bw(base_size = 9)
    p <- p + theme(legend.position = "top")
    p <- p + xlab(label = "") + ylab(label = "")
    return(p)
  } else if (is.character(group_cols)) {
    n = length(dfs)
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
      for (i in 1:n) {
        s_cols = .convert.column_input(dfs[[i]], select_cols[[i]])
        g_cols = .convert.column_input(dfs[[i]], group_cols)

        mask = Reduce(function(x, y) {
          x & y
        }, lapply(group_cols, function(col) {
          dfs[[i]][[col]] == seg[[col]]
        }))
        if (file_types[[i]] == mhealth$filetype$sensor) {
          cols = c(1, s_cols)
          df = dfs[[i]][mask, cols]
          p = .plot.numeric_column(p, df)

        } else if (file_types[[i]] == mhealth$filetype$annotation) {
          cols = c(1,2,3, s_cols)
          df = dfs[[i]][mask, cols]
          p = .plot.categoric_row(p, df, jitter = TRUE)
        } else {
          warning(
            sprintf(
              "\nFile type %s is not supported for plotting for now, will skip this data frame",
              file_types[[i]]
            )
          )
        }
      }
      p = p + scale_color_discrete(guide = FALSE)
      p <- p + theme_bw(base_size = 9)
      p <- p + theme(legend.position = "top")
      p <- p + xlab(label = "") + ylab(label = "")
      p <- p + theme(
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

  .plot.categoric_row = function(p, df, jitter = TRUE) {
    cat_values = unname(unlist(plyr::alply(df[,-c(1,2,3)], 1, function(x){stringr::str_c(x, collapse = ",")}, .dims = FALSE)))
    categories = unique(cat_values)
    df$cat_combined = cat_values

    if (jitter) {
      ypos = jitter(rep(0, length(categories)), amount = length(categories) /
                      3)
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
                      size = 1.5, hjust = 0, vjust = 0)
    return(p)
  }
