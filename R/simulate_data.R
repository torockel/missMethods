
simulate_data <- function(n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)),
                          mech_type = "MCAR", p = 0.1, cols_mis = seq(1, length(mean) - 1, 2),
                          cols_ctrl = seq(2, length(mean), 2), n_mis_stochastic = FALSE, p_overall = FALSE,
                          mvtnorm_method = "eigen", ...) {
  ds_sim <- list(
    ds_comp = mvtnorm::rmvnorm(n = n, mean = mean, sigma = sigma, method = mvtnorm_method),
    pars_true = list(mean = mean, sigma = sigma)
  )
  ds_sim$ds_mis <- delete_values(
    mech_type = mech_type, ds = ds_sim$ds_comp, p = p, cols_mis = cols_mis,
    cols_ctrl = cols_ctrl, n_mis_stochastic = n_mis_stochastic, p_overall = p_overall, ...
    )
  ds_sim
}
