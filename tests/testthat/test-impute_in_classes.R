test_that("get_split_indices() works with data frames", {
  expect_identical(
    get_split_indices(df_classes_test, "X"),
    list(`1` = c(1L, 2L), `2` = c(3L, 4L, 5L))
  )

  expect_identical(
    get_split_indices(df_classes_test, "X"),
    get_split_indices(df_classes_test, 1)
  )

  expect_identical(
    get_split_indices(df_classes_test, "Y"),
    list(`3` = c(2L, 3L, 5L), `4` = 4L, `5` = 1L)
  )

  expect_identical(
    get_split_indices(df_classes_test, "Y"),
    get_split_indices(df_classes_test, 2)
  )

  expect_identical(
    get_split_indices(df_classes_test, c("X", "Y")),
    list(`1.3` = 2L, `2.3` = c(3L, 5L), `2.4` = 4L, `1.5` = 1L)
  )

  expect_identical(
    get_split_indices(df_classes_test, c("X", "Y")),
    get_split_indices(df_classes_test, 1:2)
  )
})

test_that("get_split_indices() works with matrices", {
  expect_identical(
    get_split_indices(matrix_classes_test, "X"),
    get_split_indices(df_classes_test, "X")
  )

  expect_identical(
    get_split_indices(matrix_classes_test, "Y"),
    get_split_indices(df_classes_test, "Y")
  )

  expect_identical(
    get_split_indices(matrix_classes_test, c("X", "Y")),
    get_split_indices(df_classes_test, c("X", "Y"))
  )

  expect_identical(
    get_split_indices(matrix_classes_test, 1:2),
    get_split_indices(df_classes_test, c("X", "Y"))
  )
})

test_that("get_split_indices() works with tibbles", {
  expect_identical(
    get_split_indices(tbl_classes_test, "X"),
    get_split_indices(df_classes_test, "X")
  )

  expect_identical(
    get_split_indices(tbl_classes_test, "Y"),
    get_split_indices(df_classes_test, "Y")
  )

  expect_identical(
    get_split_indices(tbl_classes_test, c("X", "Y")),
    get_split_indices(df_classes_test, c("X", "Y"))
  )

  expect_identical(
    get_split_indices(tbl_classes_test, 1:2),
    get_split_indices(df_classes_test, c("X", "Y"))
  )
})


test_that("get_split_indices() drops row.names", {
  df_with_rownames <- data.frame(X = c(1, 1, 2), row.names = letters[1:3])
  expect_identical(
    get_split_indices(df_with_rownames, "X"),
    list(`1` = c(1L, 2L), `2` = 3L)
  )
})


test_that("get_split_indices() argument breaks works", {
  expect_identical(
    get_split_indices(df_classes_test, "Y", breaks = 2),
    list(`(3,4]` = c(2L, 3L, 4L, 5L), `(4,5]` = 1L)
  )

  expect_identical(
    get_split_indices(tbl_classes_test, "Y", breaks = 2),
    get_split_indices(matrix_classes_test, "Y", breaks = 2)
  )

  expect_identical(
    get_split_indices(tbl_classes_test, "Y", breaks = 2),
    get_split_indices(df_classes_test, "Y", breaks = 2)
  )
})
