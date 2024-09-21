## Power analysis for vertical strat forest collapse


## 1. Workspace prep ---------------

suppressWarnings(library(pwr))

cat("Conducting power analysis...\n")

## Power Analyses ---------------

## Linear models

# Some helpful resources

# https://www.statmethods.net/stats/power.html
# https://data-se.netlify.app/2018/07/24/power-calculation-for-the-general-linear-model/

# u is the number of predictors (k) minus 1, that’s the numerator degrees of 
# freedom (df). The intercepts counts a predictor, too in this case!
# v is the denominator df, defined as n − p, where n is the sample size

# k=3 predictors, n=100 observations, R^2 = .1, sig.level = .05 (default)


if (all(complete.cases(chosen_layers))) {
  n_predictors <- length(chosen_layers)
}

if (complete.cases(n_sites, n_predictors)) {
  pwr_out <- pwr.f2.test(u = n_predictors - 1,
                         v = n_sites - n_predictors,
                         f2 = r2 / (1 - r2),
                         sig.level = 0.05,
                         power = power) 
}

if (all(is.na(chosen_layers))) {
  pwr_out <- pwr.f2.test(u = NULL,
                         v = n_sites,
                         f2 = r2 / (1 - r2),
                         sig.level = 0.05,
                         power = power)
  
  cat("According to power analysis with your specified inputs, you can afford to have", floor(pwr_out$u + 1), "predictors provided to chosen_layers (set_parameters.R; L82).\n")
}

if (is.na(n_sites)) {
  pwr_out <- pwr.f2.test(u = n_predictors - 1,
                         v = NULL,
                         f2 = r2 / (1 - r2),
                         sig.level = 0.05,
                         power = power)
  
  if (all(complete.cases(c(budget, cost_per_sensor)))) {
    n_sites_max <- budget / cost_per_sensor
    
    n_sites <- ceiling(pwr_out$v - n_predictors)
    if (n_sites > n_sites_max) {
      warning("Your provided budget only allows for ", n_sites_max, " sensors, but the combination of the number of chosen_layers, r2, and power all would require at least ", n_sites, " sensors. We strongly suggest modifying your inputs.")
    }
  }
}

if (is.null(power)) {
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
}
