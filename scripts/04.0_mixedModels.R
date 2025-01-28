library(car) # for p-values in mixed models?
library(tidyverse)
library(sf)
library(lme4) # for mixed-effects models
library(easystats) # for modeling and comparisons
library(performance) # for modeling and comparisons
library(lmerTest) # for p-values in mixed models?
library(glmmTMB) # more complicated than lme4; allows for beta distributions
library(DHARMa) # for testing glmmTMB models
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
library(emmeans) # estimated marginal means/trends
library(ggeffects)
library(gtsummary)
library(here)
library(targets)

# Set ggplot theme to classic
theme_set(theme_classic())

# Load the data from the targets pipeline
tar_load(linked)
tar_load(cc)
situcolors <- c(cc$feedingColor, cc$flightColor, cc$roostingColor)
seasoncolors <- c(cc$breedingColor, cc$summerColor, cc$fallColor)

# Add number of individuals -----------------------------------------------
tar_load(season_names)
tar_load(flightGraphs)
tar_load(feedingGraphs)
tar_load(roostingGraphs)
ns <- data.frame(seasonUnique = season_names,
                  Fl = map_dbl(flightGraphs, ~length(igraph::V(.x))),
                  Fe = map_dbl(feedingGraphs, ~length(igraph::V(.x))),
                  Ro = map_dbl(roostingGraphs, ~length(igraph::V(.x)))) %>%
  pivot_longer(cols = -seasonUnique, names_to = "situ", values_to = "nInNetwork")
linked <- linked %>%
  left_join(ns, by = c("seasonUnique", "situ"))

# Let's examine zeroes for the social network measures. I know that when we calculate the social networks, we had a lot of zeroes for both degree and strength. But most of the individuals that didn't have network connections probably aren't our focal individuals for the movement measures.
# linked %>% filter(z_deg == 0 | z_str == 0) # nobody
# linked %>% filter(degree == 0, strength == 0) # just one individual in one season
# linked %>% filter(is.na(z_deg) | is.na(z_str)) # just one individual in one season
# linked %>% filter(is.na(z_deg) | is.na(z_str)) # just one individual in one season

# Let's remove that individual in case she poses a problem for modeling
linked <- linked %>%
  filter(!is.na(z_deg), !is.na(z_str))
nrow(linked)

linked %>% filter(is.infinite(z_deg) | is.infinite(z_str)) # likewise, removing the infinite individual
linked <- linked %>%
  filter(!is.infinite(z_deg), !is.infinite(z_str))

linked2 <- linked
write_rds(linked2, file = here("data/linked2.RDS"))

# Models: space use only --------------------------------------------------

## Degree ------------------------------------------------------------------
### Observed ----------------------------------------------------------------
sp_deg_obs_1 <- glmmTMB(degree ~ space_use + situ + season + (1|seasonUnique) + (1|Nili_id), data = linked2)
check_predictions(sp_deg_obs_1)
check_model(sp_deg_obs_1) 
summary(sp_deg_obs_1) 

sp_deg_obs_2 <- glmmTMB(degree ~ space_use*situ + season + (1|seasonUnique) + (1|Nili_id), data = linked2, family = gaussian())
summary(sp_deg_obs_2)
sp_deg_obs_2a <- glmmTMB(normDegree ~ space_use*situ + season + (1|seasonUnique) + (1|Nili_id), data = linked2, family = gaussian())
summary(sp_deg_obs_2a) ### normalization

sp_deg_obs_2b <- glmmTMB(degree ~ space_use*situ + season + nInNetwork + (1|seasonUnique) + (1|Nili_id), data = linked2, family = gaussian())
summary(sp_deg_obs_2b)
summary(sp_deg_obs_2)
  
sp_deg_obs_mod <- sp_deg_obs_2 # without the nInNetwork--should be accounted for by seasonUnique random effect
sp_deg_obs_mod

### Intentional ----------------------------------------------------------------
sp_deg_int_1 <- glmmTMB(z_deg ~ space_use + situ + season + (1|seasonUnique) + (1|Nili_id), data = linked2, family = gaussian())
summary(sp_deg_int_1)

sp_deg_int_2 <- glmmTMB(z_deg ~ space_use*situ + season +(1|seasonUnique) + (1|Nili_id), data = linked2, family = gaussian())
summary(sp_deg_int_2)

sp_deg_int_mod <- sp_deg_int_2

## Strength ------------------------------------------------------------------

### Observed ----------------------------------------------------------------
sp_str_obs_1 <- glmmTMB(strength ~ space_use + situ + season + (1|seasonUnique) + (1|Nili_id), data = linked2, family = gaussian())
summary(sp_str_obs_1)

sp_str_obs_2 <- glmmTMB(strength ~ space_use*situ + season + (1|seasonUnique) + (1|Nili_id), data = linked2, family = gaussian())
summary(sp_str_obs_2)

sp_str_obs_2a <- glmmTMB(normStrength ~ space_use*situ + season + (1|seasonUnique) + (1|Nili_id), data = linked2, family = gaussian())
summary(sp_str_obs_2a) ### normalized

sp_str_obs_2b <- glmmTMB(strength ~ space_use*situ + season + nInNetwork + (1|seasonUnique) + (1|Nili_id), data = linked2, family = gaussian())
summary(sp_str_obs_2b) 
summary(sp_str_obs_2)

sp_str_obs_mod <- sp_str_obs_2

### Intentional ----------------------------------------------------------------
sp_str_int_1 <- glmmTMB(z_str ~ space_use + situ + season + (1|seasonUnique) + (1|Nili_id), data = linked2, family = gaussian())
summary(sp_str_int_1)
check_model(sp_str_int_1)

sp_str_int_2 <- glmmTMB(z_str ~ space_use*situ + season + (1|seasonUnique) + (1|Nili_id), data = linked2, family = gaussian())
summary(sp_str_int_2)
check_model(sp_str_int_2)

sp_str_int_mod <- sp_str_int_2

save(sp_deg_obs_mod, file = here("data/sp_deg_obs_mod.Rda"))
save(sp_deg_int_mod, file = here("data/sp_deg_int_mod.Rda"))
save(sp_str_obs_mod, file = here("data/sp_str_obs_mod.Rda"))
save(sp_str_int_mod, file = here("data/sp_str_int_mod.Rda"))