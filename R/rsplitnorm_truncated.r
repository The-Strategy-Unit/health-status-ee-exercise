# README
# Generate truncated split normal values constrained to [0, 100]

# packages
library("dplyr")
library("fanplot")


# generate truncated split normal values ----
# param: n, type: real, number of observations required
# param: sd, type: real, uncertainty indicator
# param: skew, type: real, inverse skewness indicator [-1, 1]
# param: sd1, type: real, standard deviation left-hand side
# param: sd2, type: real, standard deviation right-hand side
# param: min, type: real, lower bound
# param: max, type: real, upper bound
# returns: vector of simulated values (samples), rtype: vector
rspnorm_trunc <- function(
  n,
  mode = 0,
  sd = 1,
  skew = 0,
  sd1 = NULL,
  sd2 = NULL,
  min = 0,
  max = 100
) {

  # generate samples with rejection sampling
  samples <- numeric(n)
  n_remaining <- n

  while (n_remaining > 0) {

    # generate candidates (oversample to reduce iterations)
    n_candidates <- n_remaining * 2
    candidates <- fanplot::rsplitnorm(
      n = n_candidates,
      mode = mode,
      sd = sd,
      skew = skew,
      sd1 = sd1,
      sd2 = sd2
    )
    # Keep only valid samples
    valid <- candidates >= min & candidates <= max
    valid_samples <- candidates[valid]

    # fill in as many as we can
    n_to_add <- min(n_remaining, length(valid_samples))
    idx_start <- n - n_remaining + 1
    idx_end <- idx_start + n_to_add - 1
    samples[idx_start:idx_end] <- valid_samples[1:n_to_add]

    n_remaining <- n_remaining - n_to_add
  }

  return(samples)
}

# Example - generate 1e3 samples constrained to [0, 100]
x <- rspnorm_trunc(n = 1e3, mode = 90, sd1 = 8, sd2 = 8, min = 0, max = 100)
y <- fanplot::rsplitnorm(n = 1e3, mode = 90, sd1 = 8, sd2 = 8)

# verify all values are [min, max]
summary(x)
all(x >= 0 & x <= 100)  # should be TRUE

summary(y)
all(y >= 0 & y <= 100)
