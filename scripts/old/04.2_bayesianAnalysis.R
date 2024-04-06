# Testing out Bayesian analysis in parallel
library(tidyverse)
library(rstan)
library(cmdstanr)
library(rstan)
library(brms)
library(posterior)
library(bayesplot)
library(tidybayes)
library(MCMCvis)
theme_set(theme_tidybayes())
load("data/derived/forModeling.Rda")
forModeling$type <- factor(forModeling$type)

modelingSubset <- forModeling %>%
  sample_n(400)

# Degree ------------------------------------------------------------------
# Define a named list of data to pass to stan
data_degree <- list(
  N = nrow(modelingSubset), # the number of data points
  PC1 = modelingSubset$PC1, # independent variable
  #type = as.integer(modelingSubset$type), # converting the factor to integer
  degree = modelingSubset$degree, # dependent variable  
  popsize = modelingSubset$n # pop size per season (part of dependent variable)
)

mod_degree <- stan(
  file = "scripts/degree.stan", # name of the Stan program file
  data = data_degree, # named list of data (defined above)
  chains = 4, # number of Markov chains to run
  warmup = 1000, # number of warmup iterations per chain
  iter = 4000, # total number of iterations per chain
  cores = 4 # number of cores (should use 1 per chain)
)

launch_shinystan(mod_degree)

data_degreeRel <- list(
  N = nrow(modelingSubset),
  PC1 = modelingSubset$PC2,
  type = as.integer(modelingSubset$type),
  degreeRel = modelingSubset$degreeRelative
)

mod_degreeRel <- stan(
  file = "scripts/degreeRel.stan",
  data = data_degreeRel,
  chains = 4,
  warmup = 1000,
  iter = 4000,
  cores = 4
)
