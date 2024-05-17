
data {
int < lower = 1 > n_pre_features; // Number of predicotrs 
int < lower = 1 > n_donors; // Size of donor pool
int < lower = 1 > n_pre; // Number of preintervention time periods
int < lower = 1 > n_time; // Number of time periods
vector[n_pre_features] pre_features_treated; // California predictors (treated units).
matrix[n_pre_features, n_donors] pre_features_donors; // Donor predictors 
matrix[n_pre,n_donors] outcome_donors_pre; //Donor outcomes
matrix[n_time,n_donors] outcome_donors_all; //Donor outcomes
}

parameters {
vector[n_donors] unit_weights; // unit weights
vector<lower=0>[n_donors] lambda;
real<lower=0> tau;
real<lower=0> sigma; // 
}

model {
  lambda ~ cauchy(0,1);
  tau ~ cauchy(0,1);
  for (i in 1:n_donors) {
    unit_weights[i] ~ normal(0, lambda[i]*tau);
  }
  pre_features_treated ~ normal(pre_features_donors*unit_weights, sigma); 
}

generated quantities {
vector[n_time] synt_control; // vector of synthetic controls
for (i in 1:n_time) {
synt_control[i] = outcome_donors_all[i,]*unit_weights; // value of synthetic contorl is y_donors*weights
}
}



