# Take as input the file name and returns different structured datasets

process_files <- function(file.name, folder = "sim-output/complete_simulation/", out = NULL){
  
  # Read the file stored in the correct folder, based on the file name
  model <- readRDS(paste0(folder, file.name))
  
  # Extract conditions based on the strings of file names and the way they are saved
  file.name_char <- stringr::str_split_1(file.name, "_")
  file.name_char[1:5] <- stringr::str_replace_all(file.name_char[1:5], "[^0-9.-]", "")
  file.name_char[6] <- stringr::str_replace_all(file.name_char[6], ".rds", "")
  # Store them in a conditions dataframe
  conditions <- data.frame(row = file.name_char[1],
                           iter = file.name_char[2],
                           n_preint = file.name_char[3],
                           n_don = file.name_char[4],
                           size_ob = file.name_char[5],
                           output_means = file.name_char[6],
                           file_name = file.name)
  
  # Return different objects depending on desired output, specified by out ""
  # We always save the file.name, because it is useful for merging datasets afterwards
  
  if(out == "conditions"){
    return(conditions)
  }
  
  # Dataframe of weights estimates
  if(out == "weights"){
    # Extract stored weights for the horseshoe and horseshoe simplex models
    df1 <-  model$weights_horseshoe
    df2 <- model$weights_horseshoe_simp
    # Joing them by 'variable' (i.e., the name of the parameter estimated)
    weights_df <- df1 %>% 
      left_join(df2, by = "variable", suffix = c("_hs", "_simp")) 

    # Add other estimates (standard and MAP), and file name
    weights_df <- weights_df %>% 
      mutate(standard_weights = model$weights_standard[,1],
             map_simp = model$map_horseshoe_simp[grep("unit_w",names(model$map_horseshoe_simp))],
             map_hs = model$map_horseshoe[grep("unit_w",names(model$map_horseshoe))],
             file_name = file.name)
    
    return(weights_df)
  }
  
  # Dataframe of effects estimates (actual and estimated counterfactuals)
  
  if(out == "effects"){
    effect_df <- 
      model$counterfactual_horseshoe %>% 
      left_join(model$counterfactual_horseshoe_simp, by = "variable", suffix = c("_hs", "_simp")) %>% 
      mutate(time = 1:length(model$conterfactual),
             counterfactual = model$conterfactual[,1],
             counterfactual_standard = model$counterfactual_standard[,1],
             counterfactual_map_simp = model$map_horseshoe_simp[grep("synt",names(model$map_horseshoe_simp))],
             counterfactual_map_hs = model$map_horseshoe[grep("synt",names(model$map_horseshoe))],
             period = ifelse(time <= conditions$n_preint, "pre", "post"),
             file_name = file.name)
    return(effect_df)
  }
 
  
  # Dataframe of covariates estimates for treated and control units
  if(out == "covariates"){
    covariates_df <- data.frame(covariates_treated = model$covariates_treated,
                             #   convex_hull = model$convex_hull,
                                covariates_control = model$covariates_control,
                                file_name = file.name) %>% 
      pivot_longer(cols = starts_with("cov"), names_to = "covariate", values_to = "value")
    return(covariates_df)
  }
  
  # Dataframe of pretreatment outcomes
  if(out == "pretreatment_outcomes"){
    pretreatment_outcomes <- cbind(model$pretreatment_outcome_treated, model$pretreatment_outcome_control)
    pretreatment_outcomes <- as.data.frame(pretreatment_outcomes) %>% 
      mutate(time = 1:nrow(pretreatment_outcomes),
             file_name = file.name) %>% 
    pivot_longer(cols = starts_with("V"), names_to = "unit", values_to = "value") %>% 
    mutate(unit = ifelse(unit == "V1", "treated", unit)) 
    return(pretreatment_outcomes)
  }
    
  # List of messages and dialogues
  if(out == "mess_and_diag")
  mess_and_diag <- list(model$messages, model$diagnostics,
                        file_name = file.name)
}

