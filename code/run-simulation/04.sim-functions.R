
# Run simulation function
run_simulation <- function(row_id, folder, 
                           model_names = c("horseshoe_simplex.stan", "horseshoe.stan")) {
  pars <- grid[row_id,]
  try(data <- gen_dat(n_periods_pre = pars$n_periods_pre, 
                  n_periods_post = pars$n_periods_post, 
                  n_donors = pars$n_donors, 
                  size_ob = pars$size_ob, 
                  outcomes_used = pars$outcomes_used,
                  seed = pars$seed,
                  standardize = FALSE))

  # Synth model
  standard <- tryCatch(error = standard_error_handler,
                       get_synth(data))

  # Bayesian models
  map_horseshoe_simp <- tryCatch(error = bayes_error_handler,
                                 get_bayes(data, map = TRUE, model = model_names[1], seed = pars$seed))
  sample_horseshoe_simp <- tryCatch(error = bayes_error_handler,
                                    get_bayes(data, sample = TRUE, model = model_names[1], seed = pars$seed))
  map_horseshoe <- tryCatch(error = bayes_error_handler,
                            get_bayes(data, map = TRUE, model = model_names[2], seed = pars$seed))
  sample_horseshoe <- tryCatch(error = bayes_error_handler,
                               get_bayes(data, sample = TRUE, model = model_names[2], seed = pars$seed))
  # Messages
  messages <- list(
    map_horseshoe_simp = map_horseshoe_simp$messages,
    sample_horseshoe_simp =sample_horseshoe_simp$messages,
    map_horseshoe = map_horseshoe_simp$messages,
    sample_horseshoe = sample_horseshoe$messages)
  
  # Bayesian models results
  map_horseshoe_simp_r <- map_horseshoe_simp$result
  try(sample_horseshoe_simp_r <- sample_horseshoe_simp$result$summary())
  map_horseshoe_r <- map_horseshoe$result
  try(sample_horseshoe_r <- sample_horseshoe$result$summary())
  
  # Diagnostics
  try(diagnostics <- list(
    sample_horseshoe_simp = sample_horseshoe_simp$result$diagnostic_summary(quiet = TRUE),
    sample_horseshoe = sample_horseshoe$result$diagnostic_summary(quiet = TRUE)))
  
  # real conterfactual
  results <- tryCatch(error = results_error_handler,
    list(
    # DATA
    conterfactual_post = data$Y1_post,
    conterfactual = data$Y1,
    pretreatment_outcome_treated = data$Y1_pre,
    pretreatment_outcome_control = data$Y0_pre,
    covariates_treated = data$X1,
    covariates_control = data$X0,
   # convex_hull = pensynth::in_convex_hull(data$X1, data$X0),
    ### WEIGHTS
    weights_standard = standard$solution.w,
    weights_horseshoe = sample_horseshoe_r %>% filter(str_detect(variable, "unit_w")) %>% select(variable, mean, median, sd, q5, q95),
    weights_horseshoe_simp = sample_horseshoe_simp_r %>% filter(str_detect(variable, "unit_w")) %>% select(variable, mean, median, sd, q5, q95),
    ## Estimated counterfactuals
    counterfactual_standard = data$Y0%*%standard$solution.w,
    counterfactual_horseshoe = sample_horseshoe_r %>% filter(str_detect(variable, "synt")) %>% select(variable, mean, median, sd, q5, q95),
    counterfactual_horseshoe_simp = sample_horseshoe_simp_r %>% filter(str_detect(variable, "synt")) %>% select(variable, mean, median, sd, q5, q95),
    ## MAP_estimates
    map_horseshoe = map_horseshoe_r$mle(),
    map_horseshoe_simp = map_horseshoe_simp_r$mle(),
    messages = messages,
    diagnostics = diagnostics,
    seed = pars$seed))
  

  # create folder if it doesn't exist
  results_folder <- paste0("sim-output/", folder, "/")
  if(dir.exists(results_folder) == FALSE){
    dir.create(results_folder)
  }
  # filenames
  filename <- paste0(results_folder, "row", row_id, "_iter", pars[1], "_pre", pars[2], "_don", pars[4], "_size", pars[5], "_out", pars[[6]])
  saveRDS(results, file = paste0(filename, ".rds"))
  
  # clean memory
  gc()
  
}


