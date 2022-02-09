test_that("are_clusters_identical() works", {

  a <- c(1, 2, 2, 3, 3, 3)
  permuts_a <- list(
    c(1, 3, 3, 2, 2, 2),
    c(2, 1, 1, 3, 3, 3),
    c(2, 3, 3, 1, 1, 1),
    c(3, 1, 1, 2, 2, 2),
    c(3, 2, 2, 1, 1, 1)
  )
  expect_true(are_clusters_identical(a, a))
  for(i in seq_along(permuts_a)){
    expect_true(are_clusters_identical(a, permuts_a[[i]]))
  }
  a_other_inds <- c(4, 1, 1, 5, 5, 5)
  expect_true(are_clusters_identical(a, a_other_inds))

  b_not_a1 <- c(1, 2, 2, 1, 1, 1)
  b_not_a2 <- c(1, 1, 2, 3, 3, 3)
  b_not_a3 <- c(5, 1, 2, 2, 3, 4)
  expect_false(are_clusters_identical(a, b_not_a1))
  expect_false(are_clusters_identical(a, b_not_a2))
  expect_false(are_clusters_identical(a, b_not_a3))

})

test_that("get_cov_matrices() works", {
  LTSigma <- structure(c(
    0.0034, 0.0194, 0.0035, -7e-04, 0.0051, -0.0013,
    0.0022, 0.0141, 0.0016, -0.0023, 0.0084, 0, 0.0012, 0.0012, 7e-04,
    0.0044, 0.0152, 0.0033
  ), .Dim = c(3L, 6L))
  res_expected <- list(structure(c(
    0.0034, -7e-04, -0.0023, -7e-04, 0.0022, 0.0012,
    -0.0023, 0.0012, 0.0044
  ), .Dim = c(3L, 3L)), structure(c(
    0.0194,
    0.0051, 0.0084, 0.0051, 0.0141, 0.0012, 0.0084, 0.0012, 0.0152
  ), .Dim = c(3L, 3L)), structure(c(
    0.0035, -0.0013, 0, -0.0013,
    0.0016, 7e-04, 0, 7e-04, 0.0033
  ), .Dim = c(3L, 3L)))
  expect_equal(
    get_cov_matrices(LTSigma, 3),
    res_expected
  )
})
