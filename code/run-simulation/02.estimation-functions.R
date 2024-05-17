#### Estimate Standard SCM and extract results from Synth
get_synth <- function(data, custom.v = TRUE){
  
  # If custom.v is TRUE, use custom weights. Else, let Synth automatically estimate variable weights
  if(custom.v){
  freq_synt <- synth(X1 = data$X1, # preintervention covariates
                     X0 = data$X0, 
                     Z0 = data$Y0_pre, # preintervention outcomes 
                     Z1 = data$Y1_pre,
                     custom.v = data$v)
  } else {
    freq_synt <- synth(X1 = data$X1, # preintervention covariates
                     X0 = data$X0, 
                     Z0 = data$Y0_pre, # preintervention outcomes 
                     Z1 = data$Y1_pre)}

  return(freq_synt)
}


### Estimated Bayesian SCM and extract results from Bayesian models: MAP and SAMPLE

get_bayes_result <- function(data, mle = FALSE, map = FALSE,
                               sample = FALSE,
                               model, ...){
  # store data in format required for Stan, including correct variable names
  data_list <- list(
    n_pre_features = nrow(data$X0),
    n_donors = ncol(data$X0),
    n_pre = nrow(data$Y0_pre), 
    n_time = nrow(data$Y0),
    pre_features_treated = as.vector(data$X1),
    pre_features_donors = data$X0,
    outcome_donors_pre = data$Y0_pre,
    outcome_treated_pre = data$Y1_pre,
    outcome_donors_all = data$Y0)
  
  # Load Stan model stored in code/stan
  mod.path <- paste0("code/stan/", model)
  mod <- file.path(mod.path) |> cmdstan_model()
  
  # Extract either MAP estimates, or estimate results sampling from the posterior
  if(map == TRUE){
  res <- mod$optimize(data = data_list,
                        jacobian = TRUE,
                      ...) 
  }
  if(sample == TRUE){
  res <- mod$sample(data = data_list, chains = 4, init = 1,  parallel_chains = 4,
                        refresh = 500, iter_warmup = 10000, iter_sampling = 10000,
                        show_messages = FALSE,
                    ...)
  }
  return(res)
}

# Modify function to supress warnings and error messages from output
get_bayes <- purrr::quietly(get_bayes_result)




