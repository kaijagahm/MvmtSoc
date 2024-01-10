// Degree model
data { //analogous to the data block in the R file.
  int<lower=0> N; //This is the number of data points
  vector[N] PC1; // Here is our independent variable (PC1)
  //int<lower=1, upper=3> type[N]; // Another independent variable, categorical (1: feeding, 2: flight, 3: roosting)
  int<lower=0> degree[N]; // And our dependent variable (degree)
  int<lower=0> popsize[N]; // And the other portion of our dependent variable, pop size
}

parameters{
  real alpha; // Intercept
  real beta_PC1; // Slope for PC1
  //vector[3] beta_type; // Slopes for "type" levels
}

model{
  // Priors
  alpha ~ normal(50, 50);
  beta_PC1 ~ normal(0, 10);
  //beta_type ~ normal(0, 10);

  // Likelihood
  vector[N] lin_pred;
  lin_pred = alpha + beta_PC1*PC1;
  //lin_pred = alpha + beta_PC1*PC1 + beta_type[type];
  degree ~ binomial_logit(popsize, lin_pred);
}

// generated quantities {
//   // Predicted probabilities
//   vector[N] p = inv_logit(alpha + beta*PC1);
// 
//   // Simulate new data points
//   int<lower=0> sim_degree[N];
//   sim_degree = binomial_rng(popsize, p);
// }
