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
tar_load(ns)

linked2 <- linked %>%
  left_join(ns, by = c("seasonUnique", "situ")) %>%
  mutate(seasonUnique = factor(seasonUnique, levels = season_names))

write_rds(linked2, file = here("data/created/linked2.RDS"))

# Models: space use only --------------------------------------------------

## Degree ------------------------------------------------------------------
### Observed ----------------------------------------------------------------
sp_deg_obs <- glmmTMB(degree ~ space_use*situ + season + (1|seasonUnique) + (1|Nili_id), data = linked2, family = gaussian())
summary(sp_deg_obs)
  
sp_deg_obs_mod <- sp_deg_obs # without the nInNetwork--should be accounted for by seasonUnique random effect
sp_deg_obs_mod

### Intentional ----------------------------------------------------------------
sp_deg_int <- glmmTMB(z_deg ~ space_use*situ + season +(1|seasonUnique) + (1|Nili_id), data = linked2, family = gaussian())
summary(sp_deg_int)

sp_deg_int_mod <- sp_deg_int

## Strength ------------------------------------------------------------------

### Observed ----------------------------------------------------------------
sp_str_obs <- glmmTMB(strength ~ space_use*situ + season + (1|seasonUnique) + (1|Nili_id), data = linked2, family = gaussian())
summary(sp_str_obs)

sp_str_obs_mod <- sp_str_obs

### Intentional ----------------------------------------------------------------
sp_str_int <- glmmTMB(z_str ~ space_use*situ + season + (1|seasonUnique) + (1|Nili_id), data = linked2, family = gaussian())
summary(sp_str_int)
check_model(sp_str_int)

sp_str_int_mod <- sp_str_int

save(sp_deg_obs_mod, file = here("data/created/sp_deg_obs_mod.Rda"))
save(sp_deg_int_mod, file = here("data/created/sp_deg_int_mod.Rda"))
save(sp_str_obs_mod, file = here("data/created/sp_str_obs_mod.Rda"))
save(sp_str_int_mod, file = here("data/created/sp_str_int_mod.Rda"))