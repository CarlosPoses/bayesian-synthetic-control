# Function to generate data at one time
# Compute explained variance by observed, unobserved factors and residual error term in that one-time period.
gen_factor <- function(n_units = 30,
                       n_obs_cov = 5, n_unobs_cov = 5,
                       sd_error_term = 20,
                       obs_covariates,
                       unobs_covariates,
                       min_obs = 5,
                       max_obs = 10,
                       min_unobs = 5,
                       max_unobs = 10){

  
obs_effect <- runif(n_obs_cov, min = min_obs, max = max_obs)
unobs_effect <- runif(n_unobs_cov, min = min_unobs, max = max_unobs)
time_trend <- runif(1, min = 1, max = 5) # equivalent to intercept
outcome <- rep(NA, n_units)
for (i in 1:n_units){
outcome[i] <- time_trend + obs_covariates[,i]%*%obs_effect + unobs_covariates[,i]%*%unobs_effect + rnorm(1, mean = 0, sd = sd_error_term)
}

obs_covariates <- t(obs_covariates)
unobs_covariates <- t(unobs_covariates)
# A.Naming columns of covariates
colnames(obs_covariates) <- paste0("obs_cov", 1:n_obs_cov)
colnames(unobs_covariates) <- paste0("unobs_cov", 1:n_unobs_cov)
# B. Storing column names
names_obs_cov <- colnames(obs_covariates)
names_unobs_cov <- colnames(unobs_covariates)
# C. Creating data frame
data <- data.frame(outcome, obs_covariates, unobs_covariates, time_trend)
# D. Creating formulas
formula_obs <- paste("outcome ~ ", paste(names_obs_cov, collapse = "+"), sep = "")
formula_unobs <- paste("outcome ~ ", paste(names_unobs_cov, collapse = "+"), sep = "")
formula <- paste("outcome ~ ", paste(names_obs_cov, collapse = "+"), "+", paste(names_unobs_cov, collapse = "+"), sep = "")
# E. Fitting linear models
mod <- lm(formula, data = data)
mod_obs <- lm(formula_obs, data = data)
mod_unobs <- lm(formula_unobs, data = data)
models <- list(mod, mod_obs, mod_unobs)
# F. Extracting R-squared values
r2 <- c(summary(mod)$adj.r.squared, summary(mod_obs)$adj.r.squared, summary(mod_unobs)$adj.r.squared)
names(r2) <- c("Total", "Observed", "Unobserved")

# G. Returning R-squared values and models
return(list(r2 = r2, models = models, obs_effect = obs_effect, unobs_effect = unobs_effect))
}


# Compute mean and sd of explained variance by observed, unobserved and residual error across a time-series

compute_avg_time_series <- function(length_time_series, args){
  ## Parameters that vary across individuals, constant across time
  obs_covariates  <- matrix(runif(args$n_units*args$n_obs_cov, min = 0, max = 20), args$n_obs_cov, args$n_units)
  unobs_covariates <- matrix(runif(args$n_units*args$n_unobs_cov, min = 0, max = 20), args$n_unobs_cov, args$n_units)
  results <- matrix(nrow = length_time_series, ncol = 3)
  for (i in 1:length_time_series){
    results[i,] <-  gen_factor(n_units = args$n_units, n_obs_cov = args$n_obs_cov, n_unobs_cov = args$n_unobs_cov,
                               sd_error_term = args$sd_error_term, obs_covariates = obs_covariates,
                               unobs_covariates = unobs_covariates,
                               min_obs = args$min_obs,
                               max_obs = args$max_obs,
                               min_unobs = args$min_unobs,
                               max_unobs = args$max_unobs)$r2
  }
  colnames(results) <- c("Total", "Observed", "Unobserved")
  result <- as.data.frame(results) %>% 
    pivot_longer(cols = everything(), names_to = "mod", values_to = "r2") %>% 
    group_by(mod) %>%
    summarize(mean = mean(r2), sd = sd(r2))
  return(result)
}

# For each time-series, compute mean SD
# Then compute mean mean, mean sd, for each model, across X iterations
summary_across_iterations <- function(n_iterations = 1000, ...){
  args <- list(...)
  results <- replicate(n_iterations, compute_avg_time_series(length_time_series = 20, args), simplify = FALSE)
  combined_df <- bind_rows(results, .id = "id")
  result <- combined_df %>% 
    group_by(mod) %>% 
    summarize(mean_timeseriesmean = mean(mean),
              mean_timeseriessd = mean(sd),
              sd_timeseriesmean = sd(mean),
              sd_timeseriessd = sd(sd))
  return(result)
}
