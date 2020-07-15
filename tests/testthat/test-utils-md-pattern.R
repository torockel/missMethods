test_that("find_md_patterns()", {
  M <- matrix(c(
    TRUE, TRUE,
    FALSE, TRUE,
    TRUE, TRUE
  ), byrow = TRUE, ncol = 2)
  correct_res <- list(
    pattern_matrix = matrix(c(
      TRUE, TRUE,
      FALSE, TRUE
    ), byrow = TRUE, ncol = 2),
    pattern_obj = list(`1` = c(1, 3), `2` = c(2))
  )
  expect_equal(find_md_patterns(M), correct_res)
})

test_that("find_pot_donor_pattern_nrs()", {
  pattern_matrix <- matrix(c(
    TRUE, TRUE,
    TRUE, FALSE,
    FALSE, TRUE,
    FALSE, FALSE
  ), byrow = TRUE, ncol = 2)
  expect_equal(find_pot_donor_pattern_nrs(pattern_matrix, c(TRUE, TRUE)), 4)
  expect_equal(find_pot_donor_pattern_nrs(pattern_matrix, c(TRUE, FALSE)), c(3, 4))
  expect_equal(find_pot_donor_pattern_nrs(pattern_matrix, c(FALSE, TRUE)), c(2, 4))
  expect_equal(find_pot_donor_pattern_nrs(pattern_matrix, c(FALSE, FALSE)), 1:4)
})
