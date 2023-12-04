// Penguins Model

data { //analogous to the data block in the R file.
  int<lower=0> N; //This is the number of data points
  vector[N] bodymass; // Here is our independent variable (body mass, g)
  int<lower=1, upper=3> species[N]; // Another independent variable, categorical (1: adelie, 2: chinstrap, 3: gentoo)
  vector[N] flipperlength; // And our dependent variable (flipper length, mm)
}

parameters {
  vector[3] alpha; // Three different intercepts, one for each species
  vector[3] beta_bodymass; // Beta is the effect of body mass on flipper length. Allowing a different slope for each species here.
  vector<lower=0>[3] sigma; // variance
  
  // Single parameters that will be used to create the distributions from which the species-specific alpha and beta values will be drawn
  real mu_alpha;
  real<lower=0> sigma_alpha;
  real mu_beta;
  real<lower=0> sigma_beta;
}

// Now comes the actual model. Here we define the priors as coming from distributions,
// and then write out our model equation.
model {
  //Priors
  // Set up the priors for the alphas
  // From looking at the graphs, intercept should be between 100 and 300 probably, so mean around 200. 
  mu_alpha ~ normal(200, 50);
  sigma_alpha ~ gamma(2, 100); //and it should be allowed to vary by about 100 in each direction. So we want a gamma distribution with mean 100ish
  alpha ~ normal(mu_alpha, sigma_alpha); // 0 is mean, 10 is standard deviation
  
  // Set up the priors for the betas
  mu_beta ~ normal(0, 5); # slopes can be positive or negative, no prior expectation of which one. Realistically the mean slope is definitely going to fall between -5 and 5, which is probably already a wider range than we need.
  sigma_beta ~ gamma(4, 5); # chosen because its mean is around 15, which seems like a good sigma. The idea with mu_beta and sigma_beta is to approximate normal(0, 15).
  beta_bodymass ~ normal(mu_beta, sigma_beta); // Slopes will vary, with a mean and sd

  // Here sigma is drawn from a gamma prior. It doesn't need to be...
  // But - sigma has a lower limit of 0, so drawing from a normal isn't going to help your model fit.
  sigma ~ gamma(3,2); //gamma is often good for sigma because it doesn't go below 0.

  //Here is where it all comes together!
  flipperlength ~ normal(alpha[species] + beta_bodymass[species] .* bodymass, sigma[species]);
}

// generated quantities {
//   vector[N] linpred = alpha[species] + beta_bodymass[species] .* bodymass;
//   vector[N] epred = linpred; // No inverse link function to apply here
//   array[N] real prediction = normal_rng(epred, sigma[species]);
// }
