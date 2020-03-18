count_NA <- function(ds) colSums(is.na(ds))


# define some complete data frames for testing ------------
df_XY_2 <- data.frame(X = 1:2, Y = 101:102)
df_XY_20 <- data.frame(X = 1:20, Y = 101:120)
df_XY_100 <- data.frame(X = 1:100, Y = 101:200)
df_XYZ_100 <- data.frame(X = 1:100, Y = 101:200, Z = 300:201)
df_XY_X_constant <- data.frame(X = rep(1, 20), Y = 101:120)
df_XY_X_one_outlier <- data.frame(X = c(rep(1, 19), 5), Y = 101:120)
df_XY_X_unequal_dummy <- data.frame(X = c(rep(0, 10), 1), Y = 101:111)
df_with_ord_factors <- data.frame(X = ordered(letters[1:20]), Y = 1:20)
df_with_unord_factor <- data.frame(X = factor(letters[1:20],
  ordered = FALSE
), Y = 1:20)
df_XY_X_binary <- data.frame(X = c(rep(1, 10), rep(0, 10)), Y = 1:20)

# define some incomplete data frames for testing ----------

df_XY_X_miss <- df_XY_100
df_XY_X_miss[c(1, 3, 5, 20:40), "X"] <- NA

df_XY_X_miss_50_obs <- df_XY_100
df_XY_X_miss_50_obs[1:50, "X"] <- NA

df_XY_X_miss_one_comp_obs <- df_XY_100
df_XY_X_miss_one_comp_obs[1:99, "X"] <- NA

df_XY_XY_miss <- df_XY_X_miss
df_XY_XY_miss[c(2, 4, 5, 30:50), "Y"] <- NA

df_XY_no_comp_obs <- df_XY_XY_miss
df_XY_no_comp_obs[c(1:31, 50:100), "X"] <- NA

df_XY_miss_with_comp_chars <- cbind(df_XY_XY_miss, char_col = rep("a", 100))

df_ordered <- data.frame(let = ordered(letters), LET = ordered(LETTERS))
df_ordered_miss <- df_ordered
df_ordered_miss[1:10, "let"] <- NA
df_ordered_miss[5:20, "LET"] <- NA

df_with_ord_factors_miss <- df_with_ord_factors
df_with_ord_factors_miss[2:5, "X"] <- NA
df_with_ord_factors_miss[12:15, "Y"] <- NA


# define some special cases -------------------------------
# one completely missing column
df_one_comp_missing_col <- data.frame(
  X = c(rep(NA, 10), 11:20),
  Y = rep(NA, 20)
)


# first row comp missing
df_first_row_comp_missing <- data.frame(X = c(NA, 2:10), Y = c(NA, 102:110))

# no complete obs
df_no_comp_obs <- data.frame(
  X = c(rep(NA, 11), 12:20),
  Y = c(101:110, rep(NA, 10))
)
# all values NA
df_all_NA <- data.frame(X = rep(NA, 10), Y = rep(NA, 10))



# define some matrices for testing ------------------------
matrix_100_2 <- matrix(1:200, nrow = 100)
matrix_20_10 <- matrix(c(1:100, 200:101), nrow = 20)

# define some incomplete matrices for testing ------------------------
matrix_100_2_miss <- matrix_100_2
matrix_100_2_miss[is.na(df_XY_XY_miss)] <- NA


# define some complete tibbles for testing ------------
tbl_XY_100 <- tibble::tibble(X = 1:100, Y = 101:200)
tbl_XYZ_100 <- tibble::tibble(X = 1:100, Y = 101:200, Z = 300:201)


# define some incomplete tibbles for testing ----------

tbl_XY_X_miss <- tbl_XY_100
tbl_XY_X_miss[c(1, 3, 5, 20:40), "X"] <- NA

tbl_XY_XY_miss <- tbl_XY_X_miss
tbl_XY_XY_miss[c(2, 4, 5, 30:50), "Y"] <- NA
