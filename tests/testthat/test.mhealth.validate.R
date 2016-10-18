library(mHealthR)

context('mhealth.validate')

test_that("wrong input", {
  expect_false(mhealth.validate(list(), file_type = "sensor"))
  expect_false(mhealth.validate(1, file_type = "annotation"))
  expect_false(mhealth.validate(c("string"), file_type = "feature"))
  expect_false(mhealth.validate(factor("c"), file_type = "event"))
})

test_that("filename wrong format", {
  expect_false(mhealth.validate("string", file_type = "sensor"), info = "Invalid pattern")
  expect_false(
    mhealth.validate(
      "datatype.DATAID.2015-32-12-42-32-43-111-M0040.sensor.csv",
      file_type = "sensor"
    ),
    info = "Invalid date"
  )
  expect_false(
    mhealth.validate(
      "datatype.DATAID.2015-12-12-12-12-43-222-T0030.sensor.csv",
      file_type = "annotation"
    ),
    info = "Invalid tz"
  )
  expect_false(
    mhealth.validate(
      "datatype.DATAID.2015-12-12-12-12-43-222-P0030.sens.csv",
      file_type = "sensor"
    ),
    info = "Invalid file type"
  )
  expect_false(
    mhealth.validate(
      "datatype.data_id.2015-12-12-12-12-12-222-P0030.annotation.csv",
      file_type = "annotation"
    ),
    info = "Invalid ID section"
  )
  expect_false(
    mhealth.validate(
      "datatype.DATAID.2015-12-12-12-12-12-222-P0030.annotation.csv",
      file_type = "sensor"
    ),
    info = "Miss match file type"
  )
  expect_false(
    mhealth.validate(
      "datatype.DATAID.2015-12-12-12-12-12-222-P0030.annotation.txt",
      file_type = "annotation"
    ),
    info = "Unknown file extension"
  )
  expect_false(
    mhealth.validate(
      "data_type.DATAID.2015-12-12-12-12-12-222-P0030.annotation.csv",
      file_type = "annotation"
    ),
    info = "Invalid name section"
  )
})

test_that("filename correct format", {
  expect_true(
    mhealth.validate(
      "datatype.DATAID.2015-12-12-12-12-12-222-P0030.annotation.csv",
      file_type = "annotation"
    ),
    info = "not gzipped"
  )
  expect_true(
    mhealth.validate(
      "datatype.DATAID.2015-12-12-12-12-12-222-P0030.annotation.csv.gz",
      file_type = "annotation"
    ),
    info = "gzipped"
  )
  expect_true(
    mhealth.validate(
      "data-type.DATAID-V1.2015-12-12-12-12-12-222-P0030.annotation.csv.gz",
      file_type = "annotation"
    ),
    info = "dash in name and ID section"
  )
  expect_true(
    mhealth.validate(
      "data-type.DATAID-V1.2015-12-12-12-12-12-222-P0030.feature.csv.gz",
      file_type = "feature"
    ),
    info = "dash in name and ID section"
  )
})

test_that("dataframe wrong format", {
  expect_false(mhealth.validate(data.frame(), "sensor"), info = "wrong nunmber of columns for sensor")
  expect_false(mhealth.validate(data.frame(ts = 1, X = 2), "sensor"), info = "wrong timestamp column")
  expect_false(mhealth.validate(data.frame(ts = "2015-12-12 12:22:22.334", X = 4, stringsAsFactors = FALSE), "sensor"), info = "wrong timestamp header")
  expect_false(mhealth.validate(data.frame(HEADER_TIME_STAMP = "2015-12-12 12:223:22.334", X = 4, stringsAsFactors = FALSE), "sensor"), info = "wrong timestamp format")
  expect_false(mhealth.validate(data.frame(HEADER_TIME_STAMP = "2015-12-12 12:22:22.334", X = "1", stringsAsFactors = FALSE), "sensor"), info = "invalid column, should be numeric")
  expect_false(mhealth.validate(data.frame(HEADER_TIME_STAMP = "2015-12-12 12:22:22.334", X = "2015-12-12 12:22:22.334", stringsAsFactors = FALSE), "annotation"), info = "wrong number of columns for annotation")
  expect_false(mhealth.validate(data.frame(HEADER_TIME_STAMP = "2015-12-12 12:22:22.334", X = "2015-12-12 12:22:22.334",  Y = "2015-12-12 12:22:22.334", Z = 1, stringsAsFactors = FALSE), "annotation"), info = "wrong column header for annotation")
  expect_false(mhealth.validate(data.frame(HEADER_TIME_STAMP = "2015-12-12 12:22:22.334", START_time = "2015-12-12 12:22:22.334",  STOP_TIME = "2015-12-12 12:22:22.334", LABEL_NAME = 1, stringsAsFactors = FALSE), "annotation"), info = "wrong column style")
  expect_false(mhealth.validate(data.frame(HEADER_TIME_STAMP = "2015-12-12 12:22:22.334", START_TIME = "2015-12-12 12:22:22.334",  STOP_TIME = "2015-12-12 12:22:22.334", LABEL_NAME = "walking", INDEX = NA, stringsAsFactors = FALSE), file_type = "annotation", group_cols = c(5)), info = "wrong group column format")
  expect_false(mhealth.validate(data.frame(HEADER_TIME_STAMP = "2015-12-12 12:22:22.334", START_TIME = "2015-12-12 12:22:22.334",  STOP_TIME = "2015-12-12 12:22:22.334", LABEL_NAME = "walking", INDEX = 1, stringsAsFactors = FALSE), file_type = "annotation", group_cols = c(8)), info = "non-existing group column")
})

test_that("dataframe correct format", {
  expect_true(mhealth.validate(data.frame(HEADER_TIME_STAMP = "2015-12-12 12:22:22.334", X = 1, stringsAsFactors = FALSE), "sensor"), info = "sensor")
  expect_true(mhealth.validate(data.frame(HEADER_TIME_STAMP = "2015-12-12 12:22:22.334", START_TIME = "2015-12-12 12:22:22.334",  STOP_TIME = "2015-12-12 12:22:22.334", LABEL_NAME = "walking", stringsAsFactors = FALSE), "annotation"), info = "annotation")
  expect_true(mhealth.validate(data.frame(HEADER_TIME_STAMP = "2015-12-12 12:22:22.334", START_TIME = "2015-12-12 12:22:22.334",  STOP_TIME = "2015-12-12 12:22:22.334", Z = 1, stringsAsFactors = FALSE), "feature"), info = "feature")
  expect_true(mhealth.validate(data.frame(HEADER_TIME_STAMP = "2015-12-12 12:22:22.334", Z = 1, stringsAsFactors = FALSE), "event"), info = "event")
  expect_true(mhealth.validate(data.frame(HEADER_TIME_STAMP = "2015-12-12 12:22:22.334", START_TIME = "2015-12-12 12:22:22.334",  STOP_TIME = "2015-12-12 12:22:22.334", LABEL_NAME = "walking", INDEX = 1, stringsAsFactors = FALSE), file_type = "annotation", group_cols = c(5)), info = "numeric group column")
  expect_true(mhealth.validate(data.frame(HEADER_TIME_STAMP = "2015-12-12 12:22:22.334", START_TIME = "2015-12-12 12:22:22.334",  STOP_TIME = "2015-12-12 12:22:22.334", LABEL_NAME = "walking", INDEX = 1, stringsAsFactors = FALSE), file_type = "annotation", group_cols = c("INDEX")), info = "string group column name")
  expect_true(mhealth.validate(data.frame(HEADER_TIME_STAMP = "2015-12-12 12:22:22.334", START_TIME = "2015-12-12 12:22:22.334",  STOP_TIME = "2015-12-12 12:22:22.334", LABEL_NAME = "walking", INDEX = 1, stringsAsFactors = FALSE), file_type = "annotation", group_cols = c(1, 5)), info = "ignore required columns in group columns")
  expect_true(mhealth.validate(data.frame(HEADER_TIME_STAMP = "2015-12-12 12:22:22.334", X = 1.2, Y = 2.4, INDEX = 1, SEGMENT = "A", stringsAsFactors = FALSE), file_type = "sensor", group_cols = c('INDEX', 'SEGMENT')), info = "multiple group columns and string group column")
})
