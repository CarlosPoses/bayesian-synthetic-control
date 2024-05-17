rm(list=ls())

####  Function to generate data according to parameters specific for each condition
#### A description of the data-generating process can be found in Section 4 of main manuscript

gen_dat <- function(n_periods_pre = 30, # number of preintervention periods
                    n_periods_post = 10, # number of postintervention periods
                    n_donors = 20, # number of untreated units
                    size_ob = 45, # percentage of variance explained by observed covariates
                    outcomes_used = "5means", # how pretreatment outcomes are included in matrix of pretreatment features
                    seed = NULL, # seed for reproducibility
                    standardize = FALSE){ # standardize the data
  
  # Set seed for reproducibility
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  # Set parameters for data-generation process based on percentage of outcome variance 
  # explained by observerd covariates
  if(size_ob == 45){
    min_obs <- 5
    max_obs <- 10
    min_unobs <- 5
    max_unobs <- 10
    sd_error_term <- 35
  } 
  if(size_ob == 20){
    min_obs <- 5
    max_obs <- 10
    min_unobs <- 10
    max_unobs <- 15
    sd_error_term <- 40
  }
  if(size_ob == 70){
    min_obs <- 10
    max_obs <- 15
    min_unobs <- 5
    max_unobs <- 10
    sd_error_term <- 40
  }
  
  # Fixed number of observed and unobserved covariates
  n_obs_cov <- 5
  n_unobs_cov <- 5
  # Compute # periods and # total units (treated + untreated)
  n_periods <- n_periods_pre + n_periods_post 
  n_units <- n_donors + 1
  
  ## Gnerate random parameters equal across individuals, varying across time (time-trends and 
  # effects of observed and unobserved covariates
  time_trend <- matrix(runif(n_periods, min = 1, max = 5), nrow = n_periods, ncol = 1)
  obs_effect <- matrix(runif(n_obs_cov*n_periods, min = min_obs, max = max_obs), nrow = n_periods, ncol = n_obs_cov)
  unobs_effect <- matrix(runif(n_unobs_cov*n_periods, min = min_unobs, max = max_unobs), nrow = n_periods, ncol = n_unobs_cov)
  
  ## Gnerate random parameters that vary across individuals, constant across time
  ## (These are values for observed and unobserved covariates, for each unit)
  obs_covariates  <- matrix(runif(n_units*n_obs_cov, min = 0, max = 20), nrow = n_obs_cov, ncol = n_units)
  unobs_covariates <- matrix(runif(n_units*n_unobs_cov, min = 0, max = 20), nrow = n_unobs_cov, ncol = n_units)
  
  # Create a period X units matrix of outcomes
  outcomes <- mat.or.vec(nr = n_periods, nc = n_units)
  
  # Generate outcomes according to data-generatin process, based on previously defined
  # observed covariates and unobserved covariates
  for (i in 1:n_periods){
    for(j in 1:n_units){
      outcomes[i, j] <- time_trend[i] + obs_covariates[, j]%*%obs_effect[i, ] + unobs_covariates[, j]%*%unobs_effect[i, ] + rnorm(1, mean = 0, sd = sd_error_term)
    }
  }
 
  # Check the treated unit can be reproduce by a convex synthetic control
  ## First, compute means of outcomes for each unit across all periods
  means <- colMeans(outcomes)
  percentiles <- quantile(means, c(0.05, 0.95))  # compute 0.05 and 0.95 percentiles
  index_treated <- sample(n_units, 1)  # Sample a single index to be the treated unit
  
  # Check if the mean of index_treated is within the desired percentiles
  while (means[index_treated] < percentiles[1] | means[index_treated] > percentiles[2]) {
    index_treated <- sample((n_donors + 1), 1)  # Resample index_treated
  }
  
  # Extract outcomes, outcomes preintervention, and outcomes postintervention, for both
  # treated unit (Y1) and untreated units (Y0)
  Y0 <- outcomes[, -index_treated]
  Y1 <- matrix(outcomes[, index_treated], ncol = 1)
  outcomes_pre <- outcomes[1:n_periods_pre, ]
  outcomes_post <- outcomes[(n_periods_pre+1):n_periods, ]
  Y0_pre <- outcomes_pre[, -index_treated]
  Y1_pre <- matrix(outcomes_pre[, index_treated], ncol = 1)
  Y1_post <- matrix(outcomes_post[, index_treated], ncol = 1)
  
  # Create matrix of pretreatment characteristics X0 and X1, depending on
  # how preintervention outcomes are included
  
  # 1. Summary outcomes (only if some outcomes used)
  if(outcomes_used == "all"){
    summary_outcomes <- outcomes_pre
  }
  if(outcomes_used == "5means"){
    if(n_periods_pre == "5"){
    summary_outcomes <- outcomes_pre
    } else {
    rows_per_object <- nrow(outcomes_pre)%/%5 # Important: only use outcome size which are multiples of 5
    for (i in 1:5) {
      start_row <- (i - 1) * rows_per_object + 1
      end_row <- min(i * rows_per_object, nrow(matrix))
      assign(paste0("subset", i), colMeans(outcomes_pre[start_row:end_row, ]))
    }
    summary_outcomes <- rbind(subset1, subset2, subset3, subset4, subset5)
    }
  }
  
  # 2. Bind with obs covariates
  if(outcomes_used != "none"){
  pretreatment_feat <- rbind(obs_covariates, summary_outcomes)
  X0 <- pretreatment_feat[, -index_treated]
  X1 <- matrix(pretreatment_feat[, index_treated], ncol = 1)
  }
  
  # 3. No pretreament outcomes; only observed covariates
  if(outcomes_used == "none"){
    X0 <- obs_covariates[, -index_treated]
    X1 <- matrix(obs_covariates[, index_treated], ncol = 1)
  }  
  
  # Variable weights forced to equality
  v <- rep(1, nrow(X0))
  
  # Standardized data 
  if(standardize){ # scale and center
  X <- pensynth:::standardize_X(X1, X0)
  } else { # only scale, no center (default)
  X_all <- cbind(X1, X0)
  sc <- apply(X_all, 1, sd)
  X <- list()
  X$X1 <- X1/sc
  X$X0 <- X0/sc
  }
  
  # Save data
  data <- list(v = v, 
               X0 = X$X0, 
               X1 = X$X1, 
               Y0_pre = Y0_pre, 
               Y1_pre = Y1_pre, 
               Y1_post = Y1_post,
               Y0 = Y0, 
               Y1 = Y1)
  return(data)
}
