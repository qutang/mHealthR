#' @importFrom ggplot2 ylim
.plot.timeseries = function(p, df, select_cols, file_type, range = NULL, text_annotation=FALSE) {
  s_cols = .convert.column_input(df, select_cols)

  if (file_type == mhealth$filetype$sensor) {
    cols = c(1, s_cols)
    df = df[cols]
    p = .plot.numeric_column(p, df)
    if(!is.null(range)){
      p = p + ggplot2::ylim(range)
    }
  } else if (file_type == mhealth$filetype$annotation) {
    cols = c(1, 2, 3, s_cols)
    df = df[cols]
    p = .plot.categoric_row(p, df, range, jitter = TRUE, text_annotation=text_annotation)
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

#' @importFrom reshape2 melt
#' @importFrom ggplot2 geom_line aes_string
.plot.numeric_column <- function(p, df) {
  df <- reshape2::melt(
    df,
    id = mhealth$column$TIMESTAMP,
    variable.name = "series",
    value.name = "value"
  )
  p <- p + ggplot2::geom_line(data = df,
                     ggplot2::aes_string(
                       x = mhealth$column$TIMESTAMP,
                       y = "value",
                       color = "series"
                     ))
  return(p)
}

#' @importFrom ggrepel geom_text_repel
#' @importFrom plyr alply
#' @importFrom stringr str_c
#' @importFrom ggplot2 geom_segment aes_string
.plot.categoric_row = function(p,
                               df,
                               range = NULL,
                               jitter = TRUE,
                               text_annotation = FALSE) {
  cat_values = unname(unlist(plyr::alply(df[, -c(1, 2, 3)], 1, function(x) {
    stringr::str_c(x, collapse = ",")
  }, .dims = FALSE)))
  categories = unique(cat_values)
  df$cat_combined = cat_values

  if (jitter) {
    if (is.null(range)) {
      amount = length(categories) / 3
    } else{
      amount = min(abs(range))
    }
    ypos = jitter(rep(0, length(categories)), amount = amount)

  } else{
    ypos = rep(0, length(categories))
  }

  df["ypos"] = sapply(cat_values, function(x) {
    ypos[x == categories]
  }, simplify = TRUE)

  p = p + ggplot2::geom_segment(
    data = df,
    ggplot2::aes_string(
      x = mhealth$column$START_TIME,
      xend = mhealth$column$STOP_TIME,
      y = "ypos",
      yend = "ypos",
      color = "cat_combined"
    ),
    alpha = 0.5,
    size = 1.5
  )
  if(text_annotation){
    p = p + ggrepel::geom_text_repel(
      data = df,
      ggplot2::aes_string(
        x = mhealth$column$START_TIME,
        y = "ypos",
        label = "cat_combined",
        color = "cat_combined"
      ),
      size = 2,
      hjust = 0,
      vjust = 0
    )
  }
  return(p)
}

#' @importFrom reshape2 melt
#' @importFrom ggplot2 geom_bar aes_string
.plot.numeric_row = function(p, df, df_standard) {
  df <- reshape2::melt(
    df,
    id = c(mhealth$column$TIMESTAMP, mhealth$column$START_TIME, mhealth$column$STOP_TIME),
    variable.name = "category",
    value.name = "value"
  )

  df$value = round(df$value * 100) / 100

  df_standard <- reshape2::melt(
    df_standard,
    id = c(mhealth$column$TIMESTAMP, mhealth$column$START_TIME, mhealth$column$STOP_TIME),
    variable.name = "category",
    value.name = "value"
  )

  df_standard$value = round(df_standard$value * 100) / 100

  df$standard_value = df_standard$value
  remove(df_standard)

  df$vjust = df$standard_value < 0

  p <- p + ggplot2::geom_bar(data = df,
                    ggplot2::aes_string(
                      x = "category",
                      y = "standard_value"
                    ), stat = "identity")
  p = p + ggrepel::geom_text_repel(data = df,
                    ggplot2::aes_string(
                      x = "category",
                      y = "standard_value",
                      label = "value",
                      vjust = "vjust"
                    ),
                    stat = "identity",
                    size = 1,
                    hjust = 0.5)
  return(p)
}
