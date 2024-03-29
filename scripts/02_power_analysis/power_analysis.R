## David Klinges
## Power analysis for vertical strat forest collapse


## 1. Workspace prep ---------------

library(pwr)

cat("Conducting power analysis...\n")

## Power Analyses ---------------

## Linear models

# Some helpful resources

# https://www.statmethods.net/stats/power.html
# https://data-se.netlify.app/2018/07/24/power-calculation-for-the-general-linear-model/

# u is the number of predictors (k) minus 1, that’s the numerator degrees of 
# freedom (df). The intercepts counts a predictor, too in this case!
# v is the denominator df, defined as n − p, where n is the sample size


if (is.na(n_sensors)) {
  if (all(complete.cases(c(budget, cost_per_sensor)))) {
    n_sensors <- budget / cost_per_sensor
  }
}
# k=3 predictors, n=100 observations, R^2 = .1, sig.level = .05 (default)

n_predictors <- length(chosen_layers)

pwr_out <- pwr.f2.test(u = n_predictors - 1,
            v = n_sensors - n_predictors,
            f2 = r2 / (1 - r2),
            sig.level = 0.05,
            power = power)

if (pwr_out$power < 0.5) {
  stop("Your chosen set of parameters has a power estimate of ", round(pwr_out$power, 2),
  ", which is too low (less than 0.5)")
}

if (pwr_out$power >= 0.5 & pwr_out$power < 0.8) {
  cat("WARNING: Your chosen set of parameters has a power estimate of ", round(pwr_out$power, 2),
      ", which is somewhat low (less than 0.8)")
} 

if (pwr_out$power >= 0.8) {
  cat("Your chosen set of parameters has a power estimate of ", round(pwr_out$power, 2),
      ", which is high, nice!")
} 
