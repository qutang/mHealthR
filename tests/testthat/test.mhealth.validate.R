library(mHealthR)

context('mhealth.validate')

test_that("wrong input", {
  expect_false(mhealth.validate(list(), file_type = "sensor"))
  expect_false(mhealth.validate(1, file_type = "annotation"))
  expect_false(mhealth.validate(c("string"), file_type = "feature"))
  expect_false(mhealth.validate(factor("c"), file_type = "event"))
})

test_that("wrong format", {
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

test_that("correct format", {
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
