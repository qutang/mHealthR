library(mHealthR)

context('mhealth.validate')

test_that("wrong input", {
  expect_error(mhealth.validate(list()))
})
