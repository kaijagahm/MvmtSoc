library(palmerpenguins)

library(dplyr)
library(tidyr)
library(purrr)

library(ggplot2)
library(patchwork)

library(cmdstanr)
library(rstan)
library(brms)

library(posterior)
library(bayesplot)

library(tidybayes)

theme_set(theme_tidybayes())

set.seed(256)

# Tutorial: https://rpubs.com/ma-riviere/stan-predict

# How does body mass affect flipper length for the different species?
# Dependent variable: flipper length (mm).
# Continuous variable; cannot be negative.
hist(penguins$flipper_length_mm)

hist(penguins$body_mass_g)
penguins_data <- penguins %>%
  mutate(body_mass_kg = body_mass_g/1000,
         body_mass_kg_cntr = body_mass_kg - mean(body_mass_kg, na.rm = T)) %>% 
  filter(!is.na(flipper_length_mm), !is.na(species), !is.na(body_mass_kg_cntr)) %>%
  mutate(ID = row_number()) %>%
  select(ID, species, body_mass_kg_cntr, flipper_length_mm)

# Define a named list of data to pass to stan
input_data <- list(
  N = nrow(penguins_data), # the number of data points
  bodymass = penguins_data$body_mass_kg_cntr, # independent variable
  species = as.integer(penguins_data$species), # converting the factor to integer
  flipperlength = penguins_data$flipper_length_mm # dependent variable  # XXX do i need to center this too?
)

# The low-level Stan interfaces (e.g. rstan & cmdstanr) don’t have the required machinery to generate predictions (e.g. something like posterior_epred). Making predictions requires running data through the model, which is defined in our Stan code. R doesn’t have access to that.

# There are generally 4 ways to go about making predictions from an rstan / cmdstanr model:

# Modifying the model’s Stan code to give it the ability to generate predictions (the recommended way)
# Creating a new model Stan model just to generate predictions
# Re-creating the model in R and using it to generate predictions based on the Stan model’s posterior draws
# Hijacking brms machinery and letting it do the work for us

# Went back and added generated quantities to the stan code

### Run Stan model ###
penguins_stan <- stan(
  file = "penguins.stan", # name of the Stan program file
  data = input_data, # named list of data (defined above)
  chains = 4, # number of Markov chains to run
  warmup = 1500, # number of warmup iterations per chain
  iter = 4000, # total number of iterations per chain
  cores = 4 # number of cores (should use 1 per chain)
)

(penguins_rstan_draws <- penguins_stan %>% 
    posterior::as_draws_df() %>% 
    posterior::subset_draws(c("alpha", "beta_bodymass", "sigma")))

ests <- left_join(penguins_data, spread_rvars(penguins_stan, epred[ID], prediction[ID], ndraws = 500, seed = 256), join_by(ID))

wrap_plots(
  mcmc_hist(penguins_rstan_draws, facet_args = list(nrow = ncol(penguins_rstan_draws))),
  mcmc_trace(penguins_rstan_draws, facet_args = list(nrow = ncol(penguins_rstan_draws))),
  widths = c(1, 1.5)
)

make_prediction_plot <- function(dat, epred_name = ".epred") {
  return(
    ggplot(dat, aes(x = body_mass_kg_cntr, y = flipper_length_mm, color = species))
    + geom_line(aes(y = .data[[epred_name]], group = paste(species, .draw)), alpha = .1) 
    + geom_point(data = penguins_data)
    + scale_color_brewer(palette = "Dark2")
  )
}

penguins_data %>%
  left_join(spread_draws(penguins_stan, epred[ID], ndraws = 100, seed = 256),
            join_by(ID)) %>%
  make_prediction_plot("epred")

# alternative, writing it all out (and this time let's use 200 draws instead of 100)
penguins_data %>%
  left_join(spread_draws(penguins_stan, epred[ID], ndraws = 200, seed = 256),
            join_by(ID)) %>%
  ggplot(aes(x = body_mass_kg_cntr, y = flipper_length_mm, color = species))+
  geom_line(aes(y = epred, group = paste(species, .draw)), alpha = 0.1)+
  geom_point(data = penguins_data)+
  scale_color_brewer(palette = "Dark2")

### Analyze output ###
MCMCsummary(penguins_stan, params = c("alpha", "beta_bodymass", "sigma"))
MCMCplot(penguins_stan, params = c("alpha", "beta_bodymass", "sigma"))
launch_shinystan(penguins_stan)
