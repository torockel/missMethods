# missMethods 0.3.0

## Update of delete_ functions

### User-visible changes

* All `delete_` functions have the argument `n_mis_stochastic` now. For some
  functions, this is only a renaming of the old `stochastic` argument (e.g.
  `delete_MCAR()`), for others this is completely new. The new name emphasis 
  that this argument controls if the *number of missing values* is stochastic 
  or deterministic. 
* `delete_MAR_1_to_x()` and `delete_MNAR_1_to_x()` get a new argument 
  `x_stochastic` along the line of `n_mis_stochastic`.
* Add the (package-wide) option `missMethods.warn.too.high.p` to control the 
  displaying of warnings for too high values of `p` (the probability for a 
  value to be missing).

### Internal

* `delete_values()` and `get_NA_indices()` centralize many steps of the old 
  (not exported) `delete_` functions. 
* All `delete_MAR_` and `delete_MNAR_` functions and `delete_MCAR()` call the 
  new `delete_values()` function now. 
* Most of the `delete_` functions use the new `get_NA_indices()` to determine
  the missing values. 

## Miscellaneous

* Evaluation functions can now compare a data frame with a matrix (thanks to 
  Marie Feldhoff for the suggestion).
* `impute_EM()` now returns the number of performed EM iterations as attribute.
* `delete_rank()` now hands the agrument `ties.method` over to `rank()`.
* Fix two tests for `delete_one_group()` (wrong argument `FUN` instead of 
  `cutoff_fun`).
* Correct documentation of `median.factor()` (thanks to @labachevskij).


# missMethods 0.2.0

## New functions

* `impute_in_classes()` allows to apply any imputation function inside imputation classes 
* `impute_hot_deck_in_classes()` hot deck imputation inside of imputation classes (adjustment cells)
* `impute_EM()` imputes values using EM parameter estimates
* `imputed_expected_values()` imputes expected values from a multivariate normal distribution
* `impute_LS_adaptive()` performs LSimpute_adaptive as described by Bo et al. (2004)
* `impute_LS_array()` performs LSimpute_array as described by Bo et al. (2004)
* `impute_LS_combined()` performs LSimpute_combined as described by Bo et al. (2004)
* `impute_LS_gene()` performs LSimpute_gene as described by Bo et al. (2004)

## Miscellaneous

* add `cov_only` and `cor_only` as `parameter` in  `evaluate_imputation_parameters()`
* improve vignette "Generating missing values" (add connections to Santos et al. (2019))
* rename of `cols` variables: now all should be named `cols_mis`, `cols_ctrl` etc.
* rename of `ds` variables: now all should be named `ds_imp`, `ds_orig` etc.
* rename of `pars` variables: now all should be named `pars_est` or `pars_true`
* sampling of sRHD type `cols_seq` is now correct, if the donor is only one numeric value
* use markdown for documentation of new functions

# missMethods 0.1.0

## New functions

Functions for the creation of missing values:

* `delete_MAR_censoring()` and `delete_MNAR_censoring()` create missing (not) at random values using a censoring mechanism
* `delete_MAR_one_group()` and `delete_MNAR_one_group()` create missing (not) at random values by deleting values in one of two groups
* `delete_MAR_rank()` and `delete_MNAR_rank()` create missing (not) at random values using a ranking mechanism

Functions for evaluation:

* `evaluate_imputation_parameters()` compares estimated parameters after imputation to true parameters

## New features

* `delete_MAR_1_to_x()` and `delete_MNAR_1_to_x()` can now handle (unordered) factors
* new criteria for `evaluate_imputed_values()` and `evaluate_parameters()`: six forms of NRMSE, nr_equal, nr_NA and precision
* `evaluate_imputed_values()`: add argument `cols_which` to select columns for evaluation. 

## Miscellaneous

* all `delete_` functions now take the same first three arguments: `ds`, `p`, `cols_mis`
* package now on GitHub and CRAN

# missMethods 0.0.1

## Implemented functions

Functions for the creation of missing values:

* `delete_MCAR()` creates missing completely at random values in different ways
* `delete_MAR_1_to_x()` and `delete_MNAR_1_to_x()` create missing (not) at random values using a 1:x mechanism

Functions for imputation:

* `impute_mean()`, `impute_median()`, `impute_mode()` different forms of mean, median and mode imputation
* `impute_sRHD()` simple Random Hot-Deck imputation with the possibility to specify a donor limit
* `apply_imputation()` a function to apply aggregating functions for imputation

Functions for evaluation:

* `evaluate_imputed_values()` compares imputed to true values
* `evaluate_parameters()` compares estimated to true parameters

Miscellaneous:

* `median.factor()` computes medians for ordered factors

