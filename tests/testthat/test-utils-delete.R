## get_NA_indices() -----------------------------------------------------------
calc_n_index <- function(NA_indices, index_nr) {
  sum(vapply(NA_indices, function(x) any(x == index_nr), logical(1)))
}

check_index <- function(NA_indices, index_nr, lower_bound, upper_bound) {
  n <- calc_n_index(NA_indices, index_nr)
  expect_true(lower_bound <= n)
  expect_true(n <= upper_bound)
}

test_that("get_NA_indices() works with comibinations of stochastic and prob", {
  N <- 1000
  n <- 10
  p <- 0.1
  set.seed(12345)

  # stochastic + prob = NULL
  n_mis <- sum(replicate(
    N, length(get_NA_indices(stochastic = TRUE, n = n, p = p, prob = NULL))
  ))
  expect_true(stats::qbinom(1e-10, n * N, p) <= n_mis)
  expect_true(n_mis <= stats::qbinom(1e-10, n * N, p, FALSE))

  # stochastic + prob = seq_len(n)
  NA_indices <- replicate(
    N, get_NA_indices(stochastic = TRUE, n = n, p = p, prob = seq_len(n))
  )
  n_mis <- sum(vapply(NA_indices, length, integer(1)))
  expect_true(stats::qbinom(1e-10, n * N, p) <= n_mis)
  expect_true(n_mis <= stats::qbinom(1e-10, n * N, p, FALSE))

  check_index(NA_indices, 1,
              stats::qbinom(1e-10, n * N, p * 1 / sum(seq_len(n))),
              stats::qbinom(1e-10, n * N, p* 1 / sum(seq_len(n)), FALSE))
  check_index(NA_indices, 10,
              stats::qbinom(1e-10, n * N, p * 10 / sum(seq_len(n))),
              stats::qbinom(1e-10, n * N, p* 10 / sum(seq_len(n)), FALSE))


  # stochastic = FALSE + prob = NULL
  n_mis <- replicate(
    N, length(get_NA_indices(stochastic = FALSE, n = n, p = p, prob = NULL))
  )
  expect_true(all(n_mis == 1L))


  # stochastic = FALSE + prob = seq_len(n)
  NA_indices <- replicate(
    N, get_NA_indices(stochastic = FALSE, n = n, p = p, prob = seq_len(n))
  )
  n_mis <- vapply(NA_indices, length, integer(1))
  expect_true(all(n_mis == 1L))

  check_index(NA_indices, 1,
              stats::qbinom(1e-10, n * N, p * 1 / sum(seq_len(n))),
              stats::qbinom(1e-10, n * N, p* 1 / sum(seq_len(n)), FALSE))
  check_index(NA_indices, 10,
              stats::qbinom(1e-10, n * N, p * 10 / sum(seq_len(n))),
              stats::qbinom(1e-10, n * N, p* 10 / sum(seq_len(n)), FALSE))

})

test_that("get_NA_indices() scales prob correctly", {

  N <- 1000
  n <- 4
  p <- 3 / 4
  set.seed(12345)

  # stochastic + prob = seq_len(n)
  NA_indices <- replicate(
    N, get_NA_indices(stochastic = TRUE, n = n, p = p, prob = seq_len(n))
  )
  n_mis <- sum(vapply(NA_indices, length, integer(1)))
  expect_true(stats::qbinom(1e-10, n * N, p) <= n_mis)
  expect_true(n_mis <= stats::qbinom(1e-10, n * N, p, FALSE))

  check_index(NA_indices, 1,
              stats::qbinom(1e-10, N, 1/3),
              stats::qbinom(1e-10, N, 1/3, FALSE))
  check_index(NA_indices, 2,
              stats::qbinom(1e-10, N, 2/3),
              stats::qbinom(1e-10, N, 2/3, FALSE))
  expect_equal(calc_n_index(NA_indices, 4), N)
  expect_equal(calc_n_index(NA_indices, 3), N)

  # stochastic = FALSE + prob = seq_len(n)
  # prob is directly passed to base::sample()
  # just trust in R Core!

})


# check find_groups ---------------------------------------
test_that("find_groups() issues a warning for constant x", {
  expect_warning(
    find_groups(
      x = rep(1, 4), cutoff_fun = median, prop = 0.5,
      use_lpSolve = TRUE,
      ordered_as_unordered = FALSE
    ),
    "grouping not possible, because x is constant"
  )

  expect_warning(
    find_groups(
      x = factor("a"), cutoff_fun = median, prop = 0.5,
      use_lpSolve = TRUE,
      ordered_as_unordered = FALSE
    ),
    "grouping not possible, because x is constant"
  )
})

test_that("find_groups() works in general", {
  expect_equal(
    find_groups(
      x = 1:5, cutoff_fun = median, prop = 0.5,
      use_lpSolve = TRUE,
      ordered_as_unordered = FALSE
    ),
    list(g1 = 1:2, g2 = 3:5)
  )
})

test_that("find_groups() treats unordered factors correct", {
  expect_equal(
    find_groups(
      x = factor(1:5), cutoff_fun = median, prop = 0.5,
      use_lpSolve = TRUE,
      ordered_as_unordered = FALSE
    ),
    list(g1 = 1:3, g2 = 4:5)
  )

  expect_equal(
    find_groups(
      x = factor(1:5), cutoff_fun = median, prop = 0.5,
      use_lpSolve = FALSE,
      ordered_as_unordered = FALSE
    ),
    list(g1 = 1:3, g2 = 4:5)
  )
})

test_that("find_groups() treats ordered factors correct", {
  expect_equal(
    find_groups(
      x = ordered(1:5), cutoff_fun = median, prop = 0.5,
      use_lpSolve = TRUE,
      ordered_as_unordered = FALSE
    ),
    list(g1 = 1:2, g2 = 3:5)
  )

  expect_equal(
    find_groups(
      x = ordered(1:5), cutoff_fun = median, prop = 0.5,
      use_lpSolve = TRUE,
      ordered_as_unordered = TRUE
    ),
    list(g1 = 1:3, g2 = 4:5)
  )
})

# check find_groups_by_cutoff_val -------------------------
test_that("find_groups_by_cutoff_val()", {
  expect_equal(
    find_groups_by_cutoff_val(df_with_ord_factors[, "X"], "d"),
    list(g1 = 1:3, g2 = 4:20)
  )
  expect_equal(
    find_groups_by_cutoff_val(df_XY_100[, "X"], 25),
    list(g1 = 1:24, g2 = 25:100)
  )
  expect_equal(
    find_groups_by_cutoff_val(c(20:29, 19:48), 25),
    list(g1 = c(1:5, 11:16), g2 = c(6:10, 17:40))
  )

  expect_equal(
    find_groups_by_cutoff_val(c(rep(0, 10), 1), 0),
    list(g1 = c(1:10), g2 = 11)
  )
})

# check find_groups_by_prop -------------------------
test_that("find_groups_by_prop()", {
  expect_equal(
    find_groups_by_prop(ordered(letters[1:20]), 0.25),
    list(g1 = 1:5, g2 = 6:20)
  )
  expect_equal(
    find_groups_by_prop(1:100, 0.4),
    list(g1 = 1:40, g2 = 41:100)
  )
  expect_equal(
    find_groups_by_prop(c(20:29, 19:48), 0.1),
    list(g1 = c(1:2, 12:13), g2 = c(3:11, 14:40))
  )


  # check lpSolve -----------------------------------------
  expect_equal(
    find_groups_by_prop(ordered(letters[1:20]), 0.25,
      use_lpSolve = TRUE
    ),
    list(g1 = 1:5, g2 = 6:20)
  )
  expect_equal(
    find_groups_by_prop(ordered(letters[1:20]), 0.25,
      use_lpSolve = FALSE
    ),
    list(g1 = 1:5, g2 = 6:20)
  )
  expect_equal(
    find_groups_by_prop(1:100, 0.4,
      use_lpSolve = TRUE
    ),
    list(g1 = 1:40, g2 = 41:100)
  )
  expect_equal(
    find_groups_by_prop(c(20:29, 19:48), 0.1,
      use_lpSolve = TRUE
    ),
    list(g1 = c(1:2, 12:13), g2 = c(3:11, 14:40))
  )

  # problematic for very basic heuristic:
  expect_equal(
    find_groups_by_prop(c("c", "c", "a", "d", "c"), 0.1,
      use_lpSolve = FALSE
    ),
    list(g1 = c(1:2, 5), g2 = 3:4)
  )
  # better use_lpSolve:
  expect_equal(
    find_groups_by_prop(c("c", "c", "a", "d", "c"), 0.1,
      use_lpSolve = TRUE
    ),
    list(g1 = 3, g2 = c(1:2, 4:5))
  )

  # g1 not empty, if x not constant
  expect_equal(
    find_groups_by_prop(ordered(letters[1:20]), 0.0005,
      use_lpSolve = TRUE
    ),
    list(g1 = 1, g2 = 2:20)
  )
  expect_equal(
    find_groups_by_prop(ordered(letters[1:20]), 0.0005,
      use_lpSolve = FALSE
    ),
    list(g1 = 1, g2 = 2:20)
  )
})


# check find_groups_by_values -----------------------------
test_that("find_groups_by_values()", {
  expect_equal(
    find_groups_by_values(
      ordered(letters[1:20]),
      c("a", "b", "e")
    ),
    list(g1 = c(1, 2, 5), g2 = c(3, 4, 6:20))
  )
  expect_equal(
    find_groups_by_values(
      1:100,
      c(5:10, 25)
    ),
    list(g1 = c(5:10, 25), g2 = c(1:4, 11:24, 26:100))
  )
})

# check calc_n_mis_g1 --------------------------------
test_that("calc_n_mis_g1() works", {
  expect_equal(calc_n_mis_g1(50, 0.8, 50, 50, 4), 40)
  expect_equal(calc_n_mis_g1(50, 0.25, 50, 20, 4), 12)
  expect_equal(calc_n_mis_g1(50, 0.45, 50, 30, 3), 22)
  expect_equal(calc_n_mis_g1(20, 1 / 17, 80, 20, 4), 1)

  expect_equal(calc_n_mis_g1(50, 0.8, 0, 50, 4), 50)
})
