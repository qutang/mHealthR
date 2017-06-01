library(mHealthR)

context("mhealth.convert")

test_that("argument validation", {

  expect_null(
      mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                      datetime_format = "%m/%d/%Y %H:%M:%OS",
                      timezone = Sys.timezone(),
                      file_type = "sensor"),
      info = "Missing required column"
  )

  expect_null(
    mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                    required_cols = c(1, 2, 3, 4),
                    timezone = Sys.timezone(),
                    file_type = "sensor"),
    info = "Missing datetime format"
  )

  expect_null(
    mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                    required_cols = c(5,2,3,4),
                    datetime_format = "%m/%d/%Y %H:%M:%OS",
                    timezone = Sys.timezone(),
                    file_type = "sensor"),
    info = "Non-existing required column"
  )

  expect_null(
    mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                    required_cols = c("st", "x", "y", "z"),
                    datetime_format = "%m/%d/%Y %H:%M:%OS",
                    timezone = Sys.timezone(),
                    file_type = "sensor"),
    info = "Non-existing required column in string"
  )

  expect_null(
    mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",et = "05/06/2010 12:00:33.212", label = "walking", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                    required_cols = c(1,2),
                    datetime_format = "%m/%d/%Y %H:%M:%OS",
                    timezone = Sys.timezone(),
                    file_type = "annotation"),
    info = "Not enough required columns"
  )
})

test_that("correct cases", {

  expect_true(
    {df = mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",st = "05/06/2010 12:00:33.212", et = "05/06/2010 12:00:33.212", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                     required_cols = c(1, 2, 3 , 4),
                     group_cols = c(10),
                     datetime_format = "%m/%d/%Y %H:%M:%OS",
                     timezone = Sys.timezone(),
                     file_type = "annotation")
    mhealth.validate(df, file_type = "annotation")
    },
    info = "group column not existing"
  )

  expect_true({
    df = mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",st = "05/06/2010 12:00:33.212", et = "05/06/2010 12:00:33.212", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                    required_cols = c(1, 2, 3 , 4),
                    group_cols = NA,
                    datetime_format = "%m/%d/%Y %H:%M:%OS",
                    timezone = Sys.timezone(),
                    file_type = "annotation")
    mhealth.validate(df, file_type = "annotation")
  },
  info = "group column illegal"
  )

  expect_true({
    df = mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                         required_cols = c(1, 2, 3, 4), datetime_format = "%m/%d/%Y %H:%M:%OS",
                         timezone = Sys.timezone(),
                         file_type = "sensor")
    mhealth.validate(df, file_type = "sensor")
  }, info = "Actigraph CSV normal")

  expect_true({
    df = mhealth.convert(data.frame(x = 1.2, y = 1.3, ts = "05/06/2010 12:00:33.212", z = 4.5, stringsAsFactors = FALSE),
                         required_cols = c(3, 1, 2, 4), datetime_format = "%m/%d/%Y %H:%M:%OS",
                         timezone = Sys.timezone(),
                         file_type = "sensor")
    mhealth.validate(df, file_type = "sensor")
  }, info = "Actigraph CSV wrong order")

  expect_true({
    df = mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",st = "05/06/2010 12:00:33.212", et = "05/06/2010 12:00:33.212", label = "walking", x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                         required_cols = c(1,2,3,4),
                         group_cols = c(5,6,7),
                         datetime_format = "%m/%d/%Y %H:%M:%OS",
                         timezone = Sys.timezone(),
                         file_type = "annotation")
    mhealth.validate(df, file_type = "annotation", group_cols = c(5,6,7))
  },
    info = "Normal annotation"
  )

  expect_true({
    df = mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",st = "05/06/2010 12:00:33.212", et = "05/06/2010 12:00:33.212", label = factor("walking"), x = 1.2, y = 1.3, z = 4.5, stringsAsFactors = FALSE),
                         required_cols = c(1, 2, 3, 4),
                         datetime_format = "%m/%d/%Y %H:%M:%OS",
                         timezone = Sys.timezone(),
                         file_type = "annotation")
    mhealth.validate(df, file_type = "annotation")
  },
  info = "annotation with factor"
  )

  expect_true({
    df = mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",st = "05/06/2010 12:00:33.212", et = "05/06/2010 12:00:33.212", label = factor("walking"), x = 1.2, y = 1.3, z = factor("P1"), stringsAsFactors = FALSE),
                         required_cols = c(1, 2, 3, 4),
                         group_cols = c(7),
                         datetime_format = "%m/%d/%Y %H:%M:%OS",
                         timezone = Sys.timezone(),
                         file_type = "annotation")
    ncol(df) == 5
  },
  info = "annotation: group columns with character"
  )

  expect_true({
    df = mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",x = 1.2, y = 1.3, z = 4.5, INDEX = 1, SEGMENT = "1", stringsAsFactors = FALSE),
                         required_cols = c(1, 2, 3, 4),
                         group_cols = c(5,6),
                         datetime_format = "%m/%d/%Y %H:%M:%OS",
                         timezone = Sys.timezone(),
                         file_type = "sensor")
    ncol(df) == 6
  },
  info = "sensor with group columns"
  )

  expect_true({
    df = mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",st = "05/06/2010 12:00:33.212", et = "05/06/2010 12:00:33.212", corr = 1.2, place = factor("home"), stringsAsFactors = FALSE),
                         required_cols = c(1:5),
                         datetime_format = "%m/%d/%Y %H:%M:%OS",
                         timezone = Sys.timezone(),
                         file_type = "feature")
    ncol(df) == 5
  },
  info = "feature: required columns have numeric and categoric"
  )

  expect_true({
    df = mhealth.convert(data.frame(ts = "05/06/2010 12:00:33.212",st = "05/06/2010 12:00:33.212", et = "05/06/2010 12:00:33.212", corr = 1.2, place = factor("home"), stringsAsFactors = FALSE),
                         required_cols = c(1,2,3,5,4),
                         datetime_format = "%m/%d/%Y %H:%M:%OS",
                         timezone = Sys.timezone(),
                         file_type = "feature")
    ncol(df) == 5
  },
  info = "feature: switch order of required columns "
  )
})
