# median.factor -------------------------------------------
test_that("median.factor(), calls are done via median()", {
  expect_equal(median(1:5), 3)
  expect_equal(median(1:6), 3.5)
  expect_true(median(ordered(letters[1:5])) == "c")
  expect_true(median(ordered(letters[1:4])) == "c")
  expect_true(median(ordered(letters[1:4]), ordered_low = TRUE) == "b")
  expect_true(median(ordered(letters[1:5])) == "c")

  expect_true(median(ordered(c(a = "a", b = "b"))) == "b")
  expect_true(is.na(median(ordered(c(letters[1:4], NA)))))
  expect_true(is.na(median(ordered(NA), na.rm = TRUE)))
  expect_error(
    median(factor(letters[1:4])),
    "median is not defined for unordered factors"
  )
})
