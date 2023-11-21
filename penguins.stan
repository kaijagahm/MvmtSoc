// Penguins Model

data { //analogous to the data block in the R file.
  int<lower=0> N; //This is the number of data points
  vector[N] bodymass; // Here is our independent variable (body mass, g)
  int<lower=1, upper=3> species[N]; // Another independent variable, categorical (1: adelie, 2: chinstrap, 3: gentoo)
  vector[N] flipperlength; // And our dependent variable (flipper length, mm)
}

parameters {
  vector[3] alpha; // Three different intercepts, one for each species
  vector[3] beta_bodymass; // Beta is the effect of body mass on flipper length
  vector<lower=0>[3] sigma; // variance
}

// Now comes the actual model. Here we define the priors as coming from distributions,
// and then write out our model equation.
model {
  //Priors
  alpha ~ normal(200,100); // 0 is mean, 10 is standard deviation
  beta_bodymass ~ normal(0,1); // Same here

  // Here sigma is drawn from a gamma prior. It doesn't need to be...
  // But - sigma has a lower limit of 0, so drawing from a normal isn't going to help your model fit.
  sigma ~ gamma(3,2); //gamma is often good for sigma because it doesn't go below 0.

  //Here is where it all comes together!
  flipperlength ~ normal(alpha[species] + beta_bodymass[species] .* bodymass, sigma[species]);
}
