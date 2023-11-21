# Testing out Bayesian analysis here
library(rstan)
library(MCMCvis)
library(shinystan)
library(palmerpenguins) # going to use the palmerpenguins data
library(tidyverse)
library(bayesplot)

# How does body mass affect flipper length for the different species?
# Dependent variable: flipper length (mm).
# Continuous variable; cannot be negative.
hist(penguins$flipper_length_mm)

# Stan does best when covariates are centered at 0, and generally conform to a normal distribution.
hist(penguins$body_mass_g)
penguins <- penguins %>%
  mutate(body_mass_g_cntr = body_mass_g - mean(body_mass_g, na.rm = T)) %>% # I don't know how this is going to handle NA's. We'll cross that bridge when we come to it I guess.
  filter(!is.na(flipper_length_mm), !is.na(species), !is.na(body_mass_g_cntr))

# XXX do we also need to center the y variable?

# Define a named list of data to pass to stan
input_data <- list(
  N = nrow(penguins), # the number of data points
  bodymass = penguins$body_mass_g_cntr, # independent variable
  species = as.integer(penguins$species), # converting the factor to integer
  flipperlength = penguins$flipper_length_mm # dependent variable  # XXX do i need to center this too?
)

### Run Stan model ###
penguins_stan <- stan(
  file = "penguins.stan", # name of the Stan program file
  data = input_data, # named list of data (defined above)
  chains = 4, # number of Markov chains to run
  warmup = 1500, # number of warmup iterations per chain
  iter = 4000, # total number of iterations per chain
  cores = 4 # number of cores (should use 1 per chain)
)

### Analyze output ###
MCMCsummary(penguins_stan, params = c("alpha", "beta_bodymass", "sigma"))
MCMCplot(penguins_stan, params = c("alpha", "beta_bodymass", "sigma"))
launch_shinystan(penguins_stan)
