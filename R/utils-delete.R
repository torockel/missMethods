## Indices for deleting -------------------------------------------------------

get_NA_indices <- function(n_mis_stochastic, n = length(indices), p = n_mis / n,
                           prob = NULL,
                           n_mis = round(n * p),
                           indices = seq_len(n)) {
  stopifnot(
    is.logical(n_mis_stochastic), length(n_mis_stochastic) == 1L,
    missing(p) || (is.numeric(p) && length(p) == 1L),
    is.null(prob) || n == length(prob),
    is.numeric(n_mis), length(n_mis) == 1L,
    n == length(indices)
  )

  ## Check for too high p or n_mis (and just it) ------------------------------
  n_mis_max <- sum(prob > 0)
  if (!is.null(prob) && n_mis_max < n * p) {
    max_p <- n_mis_max / n
    warning("p = ", p, " is too high for the chosen mechanims (and data);",
            "it will be reduced to ", max_p)
    p <- max_p
    n_mis <- n_mis_max
  }

  ## Check for Inf in prob ----------------------------------------------------
  prob_inf <- is.infinite(prob)
  if (!is.null(prob) && any(prob_inf)) {
    n_inf <- sum(prob_inf)
    if (n_inf >= n_mis) {
      # Only indices with prob == Inf should have missing values
      p <- p * n / n_inf
      return(Recall(
        n_mis_stochastic, p = p, n_mis = n_mis,
        indices = which(prob_inf), prob = NULL
      ))
    } else { # Less prob_inf then missing values
      # All indices with prob == Inf should be NA
      na_indices_inf <- which(prob_inf)
      # Remove this indices and probs
      indices <- indices[!prob_inf]
      prob <- prob[!prob_inf]
      # Adjust p and n_mis
      p <- (n * p - n_inf) / (n - n_inf)
      n_mis <- n_mis - n_inf
      further_na_indices <- Recall(n_mis_stochastic, p = p, prob = prob, n_mis = n_mis, indices = indices)
      return(c(na_indices_inf, further_na_indices))
    }
  }

  ## Check for too high prob values -------------------------------------------
  prob_scaled <- prob / sum(prob)
  if(!is.null(prob) && any(prob_scaled > 1 / (n * p))) {
    waring("some prob values are too high and will be scaled down.")
  }

  ## Get NA indices -----------------------------------------------------------
  if (n_mis_stochastic) {
    if (is.null(prob)) {
      na_indices <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(p, 1 - p))
      # na_indices <- stats::runif(n) < p # old
    } else {
      # First: Normalize prob
      prob <- prob / sum(prob)
      # Do we need to scale prob?
      while (any(prob > 1 / (n * p))) {
        # Scale down to big probs
        to_big <- prob > 1 / (n * p)
        prob[to_big] <- 1 / (n * p)
        # Scale up other probs (if they are not 0 and not already scaled)
        scaled <- prob >= 1 / (n * p)
        neq_0_not_scaled <- prob > 0 & !scaled
        if (any(neq_0_not_scaled)) {
          prob[neq_0_not_scaled] <- prob[neq_0_not_scaled] *
            (1 - sum(scaled) * 1 / (n * p)) / sum(prob[neq_0_not_scaled])
        } else {
          # We would have to delete values from objects with prob == 0!
          # This should never happen (check for too high p!)!
          # prob[prob == 0] <- (1 - sum(scaled) * 1 / (n * p)) / sum(prob == 0)
          break()
        }
      }
      # After this loop all probs should be <= 1/(n*p) and sum(prob) == 1
      if (any(prob > 1 / (n * p)) || isFALSE(all.equal(sum(prob), 1))) {
        stop("We have a problem with prob; did you specify 'p' correctly?")
      }

      prob <- prob * n * p

      # get NA indices
      na_indices <- stats::runif(n) < prob
    }
    na_indices <- indices[na_indices]
  } else { # not n_mis_stochastic
    na_indices <- resample(indices, n_mis, prob = prob)
  }
  na_indices
}


## Finding groups -------------------------------------------------------------

find_groups <- function(x, cutoff_fun, prop, use_lpSolve,
                        ordered_as_unordered, ...) {
  unique_x <- unique(x)
  nr_unique <- length(unique_x)
  if (nr_unique == 1) {
    warning("grouping not possible, because x is constant")
    groups <- list(g1 = seq_along(x), g2 = NULL)
  } else if (is.factor(x) && (ordered_as_unordered || !is.ordered(x))) {
    groups <- find_groups_by_prop(x, prop = prop, use_lpSolve)
  } else { # not an (unordered) factor and not constant
    cutoff_fun <- match.fun(cutoff_fun)
    groups <- find_groups_by_cutoff_val(x, cutoff_fun(x, ...))
  }
  groups
}


find_groups_by_cutoff_val <- function(x, cutoff_val) {
  # get rows below the cutoff value
  below <- x < cutoff_val
  # get rows greater than or equal to the cutoff value ("above")
  above <- which(!below)
  below <- which(below)
  # cure problem: no values below (but only if cols_ctrl[i] is not constant!)
  if ((length(below) == 0L) && length(unique(x)) != 1L) {
    # instead of only below, now accept below and equal
    below <- x <= cutoff_val
    above <- which(!below) # and above will be only above (not equal anymore)
    below <- which(below)
  }
  list(g1 = below, g2 = above)
}


find_groups_by_prop <- function(x, prop, use_lpSolve = TRUE) {
  ux <- unique(x) # order: first occurring in x
  nr_ux <- tabulate(match(x, ux))
  if (use_lpSolve) {
    if (requireNamespace("lpSolve", quietly = TRUE)) {
      solution <- lpSolve::lp(
        direction = "min", objective.in = nr_ux,
        const.mat = matrix(nr_ux, nrow = 1),
        const.dir = ">=",
        const.rhs = prop * length(x),
        all.bin = TRUE
      )
      if (solution$status == 0) {
        g1_ux <- ux[solution$solution > 0]
      } else {
        stop("lpSolve found no solution")
      }
    } else { # no lpSolve installed
      stop("Package \"lpSolve\" needed. Either install it or use \"use_lpSolve = FALSE\".")
    }
  } else {
    g1_ux <- cumsum(nr_ux) <= ceiling(prop * length(x))
    g1_ux <- ux[g1_ux]
  }
  # g1 should not be empty, but only if there is more than one different value in x
  if (length(g1_ux) == 0L && length(unique(x)) != 1) {
    g1_ux <- ux[1]
  }
  find_groups_by_values(x, g1_ux)
}


find_groups_by_values <- function(x, values) {
  g1 <- x %in% values
  g2 <- which(!g1)
  g1 <- which(g1)
  list(g1 = g1, g2 = g2)
}


## More helpers ---------------------------------------------------------------

calc_n_mis_g1 <- function(nr_g1, p_mis_g1,
                           nr_g2, n_mis, x) {
  if (n_mis == 0L) {
    n_mis_g1 <- 0L
  } else if (nr_g2 == 0L) {
    n_mis_g1 <- n_mis
  } else {
    n_mis_g1_ceil <- ceiling(nr_g1 * p_mis_g1)
    n_mis_g1_floor <- floor(nr_g1 * p_mis_g1)
    odds_ceil <- n_mis_g1_ceil / nr_g1 /
      ((n_mis - n_mis_g1_ceil) / nr_g2)
    odds_floor <- n_mis_g1_floor / nr_g1 /
      ((n_mis - n_mis_g1_floor) / nr_g2)
    if (abs(1 / x - odds_ceil) < abs(1 / x - odds_floor)) {
      n_mis_g1 <- n_mis_g1_ceil
    } else {
      n_mis_g1 <- n_mis_g1_floor
    }
  }
  n_mis_g1
}
