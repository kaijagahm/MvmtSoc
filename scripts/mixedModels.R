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
summary(sp_deg_obs)
  
sp_deg_obs_mod <- sp_deg_obs # without the nInNetwork--should be accounted for by seasonUnique random effect
sp_deg_obs_mod

### Non-incidental ----------------------------------------------------------------
sp_deg_int <- glmmTMB(z_deg ~ space_use*situ + season +(1|seasonUnique) + (1|Nili_id), data = linked2, family = gaussian())
summary(sp_deg_int)

sp_deg_int_mod <- sp_deg_int

## Strength ------------------------------------------------------------------

### Observed ----------------------------------------------------------------
sp_str_obs <- glmmTMB(strength ~ space_use*situ + season + (1|seasonUnique) + (1|Nili_id), data = linked2, family = gaussian())
summary(sp_str_obs)

sp_str_obs_mod <- sp_str_obs

### Non-incidental ----------------------------------------------------------------
sp_str_int <- glmmTMB(z_str ~ space_use*situ + season + (1|seasonUnique) + (1|Nili_id), data = linked2, family = gaussian())
summary(sp_str_int)
check_model(sp_str_int)

sp_str_int_mod <- sp_str_int

save(sp_deg_obs_mod, file = here("data/created/sp_deg_obs_mod.Rda"))
save(sp_deg_int_mod, file = here("data/created/sp_deg_int_mod.Rda"))
save(sp_str_obs_mod, file = here("data/created/sp_str_obs_mod.Rda"))
save(sp_str_int_mod, file = here("data/created/sp_str_int_mod.Rda"))


# Revisions: models with core area and home range separately --------------
tar_load(space_use)
tar_load(linked)

new <- linked %>%
  select(Nili_id, seasonUnique, degree, strength, z_deg, z_str, season, situ) %>%
  distinct() %>%
  left_join(select(space_use, Nili_id, seasonUnique, homeRange_log, coreAreaFidelity))

# Home range
## Degree
### Observed
hr_deg_obs <- glmmTMB(degree ~ homeRange_log*situ + season + (1|seasonUnique) + (1|Nili_id), data = new, family = gaussian())

### Non-incidental
hr_deg_int <- glmmTMB(z_deg ~ homeRange_log*situ + season + (1|seasonUnique) + (1|Nili_id), data = new, family = gaussian())

## Strength
### Observed
hr_str_obs <- glmmTMB(strength ~ homeRange_log*situ + season + (1|seasonUnique) + (1|Nili_id), data = new, family = gaussian())

### Non-incidental
hr_str_int <- glmmTMB(z_str ~ homeRange_log*situ + season + (1|seasonUnique) + (1|Nili_id), data = new, family = gaussian())

save(hr_deg_obs, file = here("data/created/hr_deg_obs.Rda"))
save(hr_deg_int, file = here("data/created/hr_deg_int.Rda"))
save(hr_str_obs, file = here("data/created/hr_str_obs.Rda"))
save(hr_str_int, file = here("data/created/hr_str_int.Rda"))

# Core area fidelity
## Degree
### Observed
caf_deg_obs <- glmmTMB(degree ~ coreAreaFidelity*situ + season + (1|seasonUnique) + (1|Nili_id), data = new, family = gaussian())

### Non-incidental
caf_deg_int <- glmmTMB(z_deg ~ coreAreaFidelity*situ + season + (1|seasonUnique) + (1|Nili_id), data = new, family = gaussian())

## Strength
### Observed
caf_str_obs <- glmmTMB(strength ~ coreAreaFidelity*situ + season + (1|seasonUnique) + (1|Nili_id), data = new, family = gaussian())

### Non-incidental
caf_str_int <- glmmTMB(z_str ~ coreAreaFidelity*situ + season + (1|seasonUnique) + (1|Nili_id), data = new, family = gaussian())

save(caf_deg_obs, file = here("data/created/caf_deg_obs.Rda"))
save(caf_deg_int, file = here("data/created/caf_deg_int.Rda"))
save(caf_str_obs, file = here("data/created/caf_str_obs.Rda"))
save(caf_str_int, file = here("data/created/caf_str_int.Rda"))