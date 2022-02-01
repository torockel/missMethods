weighted_av_gmc <- function(row_values, gmc_parameters, k) {
  denominator <- 0
  numerator <- 0
  for (i in seq_len(k)) {
    weighted_density_i <- gmc_parameters$lambda[i] *
      mixtools::dmvnorm(row_values[[i]], gmc_parameters$mu[[i]], gmc_parameters$sigma[[i]])
    denominator <- denominator + weighted_density_i
    numerator <- numerator + row_values[[i]] * weighted_density_i
  }
  numerator / denominator
}
