#' Simulate missing data
#'
#' This function first creates a complete data set via [mvtnorm::rmvnorm()].
#' Then it will create missing values via a `delete_` function. The `delete_`
#' function is specified with `mech_type`.
#'
#' @inheritParams delete_MCAR
#' @param n number of observations for the complete data set
#' @param mean mean vector for simulation of complete data
#' @param sigma covariance matrix for simulation of complete data
#' @param mech_type type of missing data mechanism. This can be any "ending" of
#'   a `delete_` function name, e.g. "MCAR" or "MAR_1_to_x".
#' @param p_overall Only used, if `mech_type = MCAR`. See [delete_MCAR()] for details.
#' @param mvtnorm_method `method` argument from [mvtnorm::rmvnorm()]
#' @param ... Further arguments for `delete_` function
#'
#' @return A list with
#'  * `ds_comp` the complete data set
#'  * `pars_true` the true parameters for the simulation of the data set
#'  *  `ds_mis` the data set with missing values
#' @export
#'
#' @examples
#' simulate_data(100, rep(0, 3))
#' # create MNAR censored data with 20 % missing values
#' simulate_data(100, rep(0, 4), mech_type = "MNAR_censoring", p = 0.2)
#' @md
simulate_data <- function(n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)),
                          mech_type = "MCAR", p = 0.1, cols_mis = seq_len(length(mean)),
                          n_mis_stochastic = FALSE, p_overall = FALSE,
                          mvtnorm_method = "eigen", ...) {
  ds_sim <- list(
    ds_comp = mvtnorm::rmvnorm(n = n, mean = mean, sigma = sigma, method = mvtnorm_method),
    pars_true = list(mean = mean, sigma = sigma)
  )
  ds_sim$ds_mis <- delete_values(
    mech_type = mech_type, ds = ds_sim$ds_comp, p = p, cols_mis = cols_mis,
    n_mis_stochastic = n_mis_stochastic, p_overall = p_overall, ...
    )
  ds_sim
}
