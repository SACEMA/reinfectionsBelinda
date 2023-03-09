#https://github.com/avehtari/rhat_ess/tree/master/code
is_constant <- function(x, tol = .Machine$double.eps) {
  abs(max(x) - min(x)) < tol
}

backtransform_ranks <- function(r, c = 3/8) {
  S <- length(r)
  (r - c) / (S - 2 * c + 1)
}

rhat_rfun <- function(sims) {
  if (anyNA(sims)) {
    return(NA)
  }
  if (any(!is.finite(unlist(sims)))) {
    return(NaN)
  }
  if (is_constant(sims)) {
    return(NA)
  }
  
  if (is.vector(sims)) {
    dim(sims) <- c(length(sims), 1)
  }
  chains <- ncol(sims)
  
  
  n_samples <- nrow(sims)
  
  chain_mean <- numeric(chains)
  chain_var <- numeric(chains)
  
  for (i in seq_len(chains)) {
    chain_mean[i] <- mean(sims[, i])
    chain_var[i] <- var(sims[, i])
  }
  
  var_between <- n_samples * var(chain_mean)
  var_within <- mean(chain_var)
  sqrt((var_between / var_within + n_samples - 1) / n_samples)

}

z_scale <- function(x) {
  r <- rank(x, ties.method = 'average')
  z <- qnorm(backtransform_ranks(r, c=3/8))
  z[is.na(x)] <- NA
  if (!is.null(dim(x))) {
    # output should have the input dimension
    z <- array(z, dim = dim(x), dimnames = dimnames(x))
  }
  z
}

split_chains <- function(sims) {
  # split Markov chains
  # Args:
  #   sims: a 2D array of samples (# iter * # chains)
  if (is.vector(sims)) {
    dim(sims) <- c(length(sims), 1)
  }
  niter <- dim(sims)[1]
  if (niter == 1L) return(sims)
  half <- niter / 2
  cbind(sims[1:floor(half), ], sims[ceiling(half + 1):niter, ])
}

rhat <- function(sims) {
  bulk_rhat <- rhat_rfun(z_scale(split_chains(sims)))
  sims_folded <- abs(sims - median(sims))
  tail_rhat <- rhat_rfun(z_scale(split_chains(sims_folded)))
  max(bulk_rhat, tail_rhat)
}


#################################################### ESS
ess_rfun <- function(sims) {
  if (is.vector(sims)) {
    dim(sims) <- c(length(sims), 1)
  }
  # chains = M and n_samples = N in the paper
  chains <- ncol(sims)
  n_samples <- nrow(sims)
  if (n_samples < 3L || anyNA(sims)) {
    return(NA)
  }
  if (any(!is.finite(sims))) {
    return(NaN)
  }
  if (is_constant(sims)) {
    return(NA)
  }
  # acov[t,m] = s_m^2 \rho_{m,t} in the paper
  acov <- lapply(seq_len(chains), function(i) autocovariance(sims[, i]))
  acov <- do.call(cbind, acov) * n_samples / (n_samples - 1)
  chain_mean <- apply(sims, 2, mean)
  # mean_var = W in the paper
  mean_var <- mean(acov[1, ]) 
  # var_plus = \hat{var}^{+} in the paper
  var_plus <- mean_var * (n_samples - 1) / n_samples
  if (chains > 1)
    var_plus <- var_plus + var(chain_mean)
  
  # Geyer's initial positive sequence
  rho_hat_t <- rep.int(0, n_samples)
  t <- 0
  rho_hat_even <- 1
  rho_hat_t[t + 1] <- rho_hat_even
  rho_hat_odd <- 1 - (mean_var - mean(acov[t + 2, ])) / var_plus
  rho_hat_t[t + 2] <- rho_hat_odd
  while (t < nrow(acov) - 5 && !is.nan(rho_hat_even + rho_hat_odd) &&
         (rho_hat_even + rho_hat_odd > 0)) {
    t <- t + 2
    rho_hat_even = 1 - (mean_var - mean(acov[t + 1, ])) / var_plus
    rho_hat_odd = 1 - (mean_var - mean(acov[t + 2, ])) / var_plus
    if ((rho_hat_even + rho_hat_odd) >= 0) {
      rho_hat_t[t + 1] <- rho_hat_even
      rho_hat_t[t + 2] <- rho_hat_odd
    }
  }
  max_t <- t
  # this is used in the improved estimate (see below)
  if (rho_hat_even>0)
    rho_hat_t[max_t + 1] <- rho_hat_even
  
  # Geyer's initial monotone sequence
  t <- 0
  while (t <= max_t - 4) {
    t <- t + 2
    if (rho_hat_t[t + 1] + rho_hat_t[t + 2] >
        rho_hat_t[t - 1] + rho_hat_t[t]) {
      rho_hat_t[t + 1] = (rho_hat_t[t - 1] + rho_hat_t[t]) / 2;
      rho_hat_t[t + 2] = rho_hat_t[t + 1];
    }
  }
  # nominal sample size S=MN
  S <- chains * n_samples
  # Geyer's truncated estimate is
  #   tau_hat <- -1 + 2 * sum(rho_hat_t[1:max_t])
  # We use an improved estimate, which is equivalent to taking average
  # of truncation with lag max_t and with max_t+1 and which reduces
  # variance in antithetic case
  tau_hat <- -1 + 2 * sum(rho_hat_t[1:max_t]) + rho_hat_t[max_t+1]
  # Safety check for negative values and with max ess equal to ess*log10(ess)
  tau_hat <- max(tau_hat, 1/log10(S))
  ess <- S / tau_hat
  ess
}

autocovariance <- function(y) {
  N <- length(y)
  M <- fft_next_good_size(N)
  Mt2 <- 2 * M
  yc <- y - mean(y)
  yc <- c(yc, rep.int(0, Mt2 - N))
  transform <- fft(yc)
  ac <- fft(Conj(transform) * transform, inverse = TRUE)
  # use "biased" estimate as recommended by Geyer (1992)
  ac <- Re(ac)[1:N] / (N^2 * 2)
  ac
}

fft_next_good_size <- function(N) {
  # Find the optimal next size for the FFT so that
  # a minimum number of zeros are padded.
  if (N <= 2)
    return(2)
  while (TRUE) {
    m = N
    while ((m %% 2) == 0) m = m / 2
    while ((m %% 3) == 0) m = m / 3
    while ((m %% 5) == 0) m = m / 5
    if (m <= 1)
      return(N)
    N = N + 1
  }
}

ess_bulk <- function(sims) {
  ess_rfun(z_scale(split_chains(sims)))
}

ess_tail <- function(sims) {
  I05 <- (sims <= quantile(sims, 0.05)) + 0
  q05_ess <- ess_rfun(split_chains(I05))
  I95 <- (sims <= quantile(sims, 0.95)) + 0
  q95_ess <- ess_rfun(split_chains(I95))
  min(q05_ess, q95_ess)
}

save(ess_tail, ess_bulk, fft_next_good_size, autocovariance, ess_rfun, rhat, split_chains, z_scale, rhat_rfun, backtransform_ranks, is_constant, file = "utils/fxn_convergence_diagnostics.RData")
