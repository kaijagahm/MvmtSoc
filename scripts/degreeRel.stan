// Another try at the degree model, but with relative degree instead of a binomial

data { //analogous to the data block in the R file.
  int<lower=0> N; //This is the number of data points
  vector[N] PC1; // Here is our independent variable (PC1)
  int<lower=1, upper=3> type[N]; // Another independent variable, categorical (1: feeding, 2: flight, 3: roosting)
  vector<lower=0>[N] degreeRel; // And our dependent variable (degree)
}

parameters{
  real alpha; // Intercept
  real beta_PC1; // Slope for PC1
  vector[3] beta_type; // Slopes for "type" levels
  real sigma; // Variance
}

model{
  // Priors
  alpha ~ normal(50, 50);
  beta_PC1 ~ normal(0, 10);
  beta_type ~ normal(0, 10);
  sigma ~ gamma(2, 3);

  // Likelihood
  degreeRel ~ normal(alpha + beta_PC1*PC1 + beta_type[type], sigma);
}
