# Functions to handle errors.

## Standard SCM
standard_error_handler <- function(cnd){
  standard <- list()
  standard$solution.w <- rep(0, 15)
  return(standard$solution.w)}

## Bayesian SCM
bayes_error_handler <- function(cnd){
  return(list(result = "error", output = "error", warnings = "error", messages = "error"))
}

## Results error handler
results_error_handler <- function(cnd){
  results <- list(
    # DATA
    conterfactual_post = "error",
    conterfactual = "error",
    pretreatment_outcome_treated = "error",
    pretreatment_outcome_control = "error",
    covariates_treated = "error",
    covariates_control = "error",
    convex_hull = "error",
    ### WEIGHTS
    weights_standard = "error",
    weights_horseshoe = "error",
    weights_horseshoe_simp = "error",
    ## Estimated counterfactuals
    counterfactual_standard = "error",
    counterfactual_horseshoe = "error",
    counterfactual_horseshoe_simp = "error",
    ## MAP_estimates
    map_horseshoe = "error",
    map_horseshoe_simp = "error",
    messages = "error",
    diagnostics = "error",
    seed = "error")
  return(results)
}
