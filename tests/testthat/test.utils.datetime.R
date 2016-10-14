library(mHealthR)

context('utils.datetime')

test_that("format time zone", {
  expect_equal(utils.formatTimeZone(-5), "M0500")
  expect_equal(utils.formatTimeZone(5), "P0500")
  expect_equal(utils.formatTimeZone(5.5), "P0530")
  expect_equal(utils.formatTimeZone(-5.15), "M0500")
  expect_equal(utils.formatTimeZone(-4.25), "M0415")
})

test_that("parse time zone", {
  expect_equal(utils.parseTimeZone("M0500"), -5)
  expect_equal(utils.parseTimeZone("P0500"), 5)
  expect_equal(utils.parseTimeZone("P0530"), 5.5)
  expect_equal(utils.parseTimeZone("M0545"), -5.75)
  expect_equal(utils.parseTimeZone("M0415"), -4.25)
})
