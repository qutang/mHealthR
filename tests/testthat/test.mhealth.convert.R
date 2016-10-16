library(mHealthR)

context("mhealth.convert")

test_that("argument validation", {

  expect_null(
      mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                      datetime_format = "%m/%d/%Y %H:%M:%OS",
                      timezone = Sys.timezone(),
                      file_type = "sensor"),
      info = "Missing timestamp col"
  )

  expect_null(
    mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                    timestamp_col = 1,
                    timezone = Sys.timezone(),
                    file_type = "sensor"),
    info = "Missing datetime format"
  )

  expect_null(
    mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                    timestamp_col = -1,
                    datetime_format = "%m/%d/%Y %H:%M:%OS",
                    timezone = Sys.timezone(),
                    file_type = "sensor"),
    info = "Non-existing timestamp column"
  )

  expect_null(
    mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                    timestamp_col = "s",
                    datetime_format = "%m/%d/%Y %H:%M:%OS",
                    timezone = Sys.timezone(),
                    file_type = "sensor"),
    info = "Non-existing timestamp column 2"
  )

  expect_null(
    mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",et = "05/06/2010 12:00:33.212", label = "walking", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                    timestamp_col = 1,
                    datetime_format = "%m/%d/%Y %H:%M:%OS",
                    stop_time_col = 2,
                    annotation_name_col = 3,
                    timezone = Sys.timezone(),
                    file_type = "annotation"),
    info = "Missing start time column"
  )

  expect_null(
    mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",st = "05/06/2010 12:00:33.212", label = "walking", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                    timestamp_col = 1,
                    datetime_format = "%m/%d/%Y %H:%M:%OS",
                    start_time_col = 2,
                    annotation_name_col = 3,
                    timezone = Sys.timezone(),
                    file_type = "annotation"),
    info = "Missing stop time column"
  )

  expect_null(
    mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",st = "05/06/2010 12:00:33.212", et = "05/06/2010 12:00:33.212", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                    timestamp_col = 1,
                    datetime_format = "%m/%d/%Y %H:%M:%OS",
                    start_time_col = 2,
                    stop_time_col = 3,
                    timezone = Sys.timezone(),
                    file_type = "annotation"),
    info = "Missing annotation name column"
  )

  expect_null(
    mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",st = "05/06/2010 12:00:33.212", et = "05/06/2010 12:00:33.212", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                    timestamp_col = 1,
                    datetime_format = "%m/%d/%Y %H:%M:%OS",
                    start_time_col = 2,
                    stop_time_col = 3,
                    annotation_name_col = 4,
                    timezone = Sys.timezone(),
                    other_cols_order = data.frame(1,3,2,5,6, "s"),
                    file_type = "annotation"),
    info = "Ilegal other column order"
  )
})

test_that("correct cases", {
  expect_true({
    df = mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                         timestamp_col = 1, datetime_format = "%m/%d/%Y %H:%M:%OS",
                         timezone = Sys.timezone(),
                         file_type = "sensor")
    mhealth.validate(df, file_type = "sensor")
  }, info = "Actigraph CSV normal")

  expect_true({
    df = mhealth.convert(data.frame(x = 1.2, y = 1.3, ts = "05/06/2010 12:00:33.212", z = 4.5, stringsAsFactors = FALSE),
                         timestamp_col = 3, datetime_format = "%m/%d/%Y %H:%M:%OS",
                         timezone = Sys.timezone(),
                         file_type = "sensor")
    mhealth.validate(df, file_type = "sensor")
  }, info = "Actigraph CSV wrong order")

  expect_true({
    df = mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",st = "05/06/2010 12:00:33.212", et = "05/06/2010 12:00:33.212", label = "walking", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                         timestamp_col = 1,
                         datetime_format = "%m/%d/%Y %H:%M:%OS",
                         start_time_col = 2,
                         stop_time_col = 3,
                         annotation_name_col = 4,
                         timezone = Sys.timezone(),
                         file_type = "annotation")
    mhealth.validate(df, file_type = "annotation")
  },
    info = "Normal annotation"
  )

  expect_true({
    df = mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",st = "05/06/2010 12:00:33.212", et = "05/06/2010 12:00:33.212", label = factor("walking"), x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                         timestamp_col = 1,
                         datetime_format = "%m/%d/%Y %H:%M:%OS",
                         start_time_col = 2,
                         stop_time_col = 3,
                         annotation_name_col = 4,
                         timezone = Sys.timezone(),
                         file_type = "annotation")
    mhealth.validate(df, file_type = "annotation")
  },
  info = "annotation with factor"
  )

  expect_true({
    df = mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",st = "05/06/2010 12:00:33.212", et = "05/06/2010 12:00:33.212", label = factor("walking"), x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                         timestamp_col = 1,
                         datetime_format = "%m/%d/%Y %H:%M:%OS",
                         start_time_col = 2,
                         stop_time_col = 3,
                         annotation_name_col = 4,
                         timezone = Sys.timezone(),
                         other_cols_order = NA,
                         file_type = "annotation")
    ncol(df) == 4
  },
  info = "annotation: drop useless columns"
  )

  expect_true({
    df = mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",st = "05/06/2010 12:00:33.212", et = "05/06/2010 12:00:33.212", label = factor("walking"), x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                         timestamp_col = 1,
                         datetime_format = "%m/%d/%Y %H:%M:%OS",
                         start_time_col = 2,
                         stop_time_col = 3,
                         annotation_name_col = 4,
                         timezone = Sys.timezone(),
                         other_cols_order = c(1,2,3,4, 's'),
                         file_type = "annotation")
    ncol(df) == 4
  },
  info = "annotation: ignore unknown other col names"
  )

  expect_true({
    df = mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",st = "05/06/2010 12:00:33.212", et = "05/06/2010 12:00:33.212", label = factor("walking"), x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                         timestamp_col = 1,
                         datetime_format = "%m/%d/%Y %H:%M:%OS",
                         start_time_col = 2,
                         stop_time_col = 3,
                         annotation_name_col = 4,
                         timezone = Sys.timezone(),
                         other_cols_order = c(1,2,3, 5,6),
                         file_type = "annotation")
    ncol(df) == 6
  },
  info = "annotation: ignore the first four columns appear in other col names"
  )

  expect_true({
    df = mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",st = "05/06/2010 12:00:33.212", et = "05/06/2010 12:00:33.212", label = factor("walking"), x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                         timestamp_col = 1,
                         datetime_format = "%m/%d/%Y %H:%M:%OS",
                         start_time_col = 2,
                         stop_time_col = 3,
                         annotation_name_col = 4,
                         timezone = Sys.timezone(),
                         other_cols_order = c(5, 1,2,3, 5,5,5, 6),
                         file_type = "annotation")
    ncol(df) == 6
  },
  info = "annotation: ignore repeated columns in other cols order"
  )
})
