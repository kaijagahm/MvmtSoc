# MWE for Bayesian model
library(palmerpenguins)
library(rstan)
library(MCMCvis)
library(shinystan)
library(ggplot2)
library(bayesplot)
library(tidybayes)

theme_set(theme_tidybayes())

# How does body mass affect flipper length for the different species?
# Dependent variable: flipper length (mm).
# Independent variable: body mass (kg, converted from g)
penguins <- penguins %>%
  mutate(body_mass_kg = body_mass_g/1000, # convert from g to kg
         body_mass_kg_cntr = body_mass_kg - mean(body_mass_kg, na.rm = T)) %>% # center the values
  filter(!is.na(flipper_length_mm), !is.na(species), !is.na(body_mass_kg_cntr)) # remove missing values

# Define a named list of data to pass to stan
input_data <- list(
  N = nrow(penguins), # the number of data points
  bodymass = penguins$body_mass_kg_cntr, # independent variable
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

### Prep for visualizing output ###
tidybayes::get_variables(penguins_stan)
tidy <- penguins_stan %>%
  spread_draws(alpha[species], beta_bodymass[species], sigma[species])
qi <- tidy %>% median_qi(species_mean = alpha + beta_bodymass, .width = c(.95, .8, .5))
qi %>%
  ggplot(aes(y = species, x = species_mean, xmin = .lower, xmax = .upper))+
  geom_pointinterval()

# tidy %>% # XXX this takes too long to run!
#   mutate(species_mean = alpha + beta_bodymass) %>%
#   filter(.draw %% 2 == 0) %>%
#   ggplot(aes(y = species, x = species_mean))+
#   stat_halfeye()

# XXX moving on to the next part of the analysis, the stanfit object no longer works.
posterior_samples <- as.data.frame(penguins_stan)
mean_alpha <- colMeans(dplyr::select(posterior_samples, starts_with("alpha")))
mean_beta <- colMeans(dplyr::select(posterior_samples, starts_with("beta")))
cred_int_alpha <- apply(dplyr::select(posterior_samples, starts_with("alpha")), 2, function(x) quantile(x, c(0.025, 0.975)))
cred_int_beta <- apply(dplyr::select(posterior_samples, starts_with("beta")), 2, function(x) quantile(x, c(0.025, 0.975)))

plot_data <- data.frame(
  x = rep(penguins$body_mass_kg_cntr, each = 3),
  species = rep(1:3, times = nrow(penguins))
)

plot_data$y_pred <- mean_alpha[plot_data$species] + mean_beta[plot_data$species] * plot_data$x
plot_data$y_lower <- cred_int_alpha[1, plot_data$species] + cred_int_beta[1, plot_data$species] * plot_data$x
plot_data$y_upper <- cred_int_alpha[2, plot_data$species] + cred_int_beta[2, plot_data$species] * plot_data$x
plot_data <- plot_data %>%
  mutate(species = case_when(species == 1 ~ "Adelie",
                             species == 2 ~ "Chinstrap",
                             species == 3 ~ "Gentoo"))

penguins %>%
  ggplot()+
  geom_point(aes(x = body_mass_kg_cntr, y = flipper_length_mm, col = factor(species)))+
  geom_line(data = plot_data, aes(x = x, y = y_pred, group = species, color =factor(species)))+
  geom_ribbon(data = plot_data, aes(x = x, ymin = y_lower, ymax = y_upper, group = species, fill = factor(species)), alpha= 0.2) # this technically works, but we have ridiculously narrow uncertainties in one spot.
