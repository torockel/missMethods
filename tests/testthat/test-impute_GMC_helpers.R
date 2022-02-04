test_that("change_nr() works", {
  a <- c(1, 2, 2, 3, 3, 3)
  b <- c(3, 2, 2, 1, 1, 1)
  expect_equal(
    change_nr(a, b, i = 1),
    b
  )
  expect_equal(
    change_nr(a, b, i = 4),
    b
  )
  d <- c(5, 3, 1, 1, 1, 4)
  expect_equal(
    change_nr(a, d, i = 1),
    c(5, a[2:length(a)])
  )
  d2 <- c(4, 1, 1, 5, 5, 5)
  change_nr(a, d2, 1)
})

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
