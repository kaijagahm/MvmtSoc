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
# THIS SCRIPT IS FOR TESTING OUT MODELS. FINAL MODEL CODE GOES OVER INTO FUNCTIONS.R TO BE INCORPORATED INTO THE TARGETS PIPELINE.
# Updated 2024-06-10: removing roost_div as a predictor

# Set ggplot theme to classic
theme_set(theme_classic())

# Load the data from the targets pipeline (wow this is so much easier!!!)
tar_load(linked)
tar_load(cc)
situcolors <- c(cc$feedingColor, cc$flightColor, cc$roostingColor)
seasoncolors <- c(cc$breedingColor, cc$summerColor, cc$fallColor)

# Examine response variable distributions ---------------------------------

# Normalized degree and strength ------------------------------------------
# linked %>%
#   ggplot(aes(x = degree, col = season, group = seasonUnique))+
#   geom_density()+
#   facet_wrap(~type, scales = "free") # very distinct distributions, but since we will have a random effect for seasonUnique, that should cover it... 
# 
# linked %>%
#   ggplot(aes(x = strength, col = season, group = seasonUnique))+
#   geom_density()+
#   facet_wrap(~type, scales = "free") 
# 
# # Degree and strength z-scores (non-normalized) ---------------------------
# linked %>%
#   ggplot(aes(x = z_deg, col = season, group = seasonUnique))+
#   geom_density()+
#   facet_wrap(~type, scales = "free") # these are weird distributions. I wonder what model family we should use here...
# 
# linked %>%
#   ggplot(aes(x = z_str, col = season, group = seasonUnique))+
#   geom_density()+
#   facet_wrap(~type, scales = "free")

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

# Modeling: not corrected for space use -----------------------------------
tar_load(allMetrics)
# Degree -----------------------------------------------------------------
deg_base <- glmmTMB(normDegree ~ situ + movement + space_use + age_group + season + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
check_predictions(deg_base) # bleh
simulationOutput <- DHARMa::simulateResiduals(deg_base)
plot(simulationOutput, pch=".") # not great but not terrible.

deg_full <- glmmTMB(normDegree ~ situ*season*movement + situ*season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian()) # not converging, hmmm...
simulationOutput <- DHARMa::simulateResiduals(deg_full)
plot(simulationOutput, pch=".") # not great but not terrible.
check_collinearity(deg_full) # remove situ*season*movement first

deg_1 <- glmmTMB(normDegree ~ situ*season + situ*movement + season*movement + situ*season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_1) # now remove situ*season*space.

deg_2 <- glmmTMB(normDegree ~ situ*season + situ*movement + season*movement + situ*space_use + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_2) # now remove situ*season

deg_3 <- glmmTMB(normDegree ~ situ*movement + season*movement + situ*space_use + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_3) # this looks pretty good, let's look at the summary
summary(deg_3) # season*space can be removed

deg_4 <- glmmTMB(normDegree ~ situ*movement + season*movement + situ*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_4) # looks pretty good
summary(deg_4) # everything else is at least marginally significant, though movement*season is really borderline...

deg_mod <- deg_4

# Strength ---------------------------------------------------------------
str_base <- glmmTMB(normStrength ~ situ + movement + space_use + age_group + season + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
check_predictions(str_base) # not bad, actually!
simulationOutput <- DHARMa::simulateResiduals(str_base)
plot(simulationOutput, pch=".") # nice, pretty good!

str_full <- glmmTMB(normStrength ~ situ*season*movement + situ*season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
simulationOutput <- DHARMa::simulateResiduals(str_full)
dev.off()
plot(simulationOutput, pch=".") # huh, not the worst actually!
check_collinearity(str_full) # the worst is situ*season*space

str_1 <- glmmTMB(normStrength ~ situ*season*movement + situ*season + situ*space_use + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
check_collinearity(str_1) # now we can remove situ*season*movement

str_2 <- glmmTMB(normStrength ~ situ*season + situ*movement + season*movement + situ*space_use + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
check_collinearity(str_2) # now we can remove situ*season

str_3 <- glmmTMB(normStrength ~ situ*movement + season*movement + situ*space_use + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
check_collinearity(str_3) # movement*season is technically now the highest, but they're very close so let's just look at the summary
summary(str_3) # space*season and situ*space and situ*movement and movement*season are all non-significant.

str_4 <- glmmTMB(normStrength ~ situ + season + movement + space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
summary(str_4) # done

str_mod <- str_4

# Modeling: z-scores (corrected for space use) ----------------------------
# This time we're using non-normalized degree and strength values, since using z-scores ends up normalizing each individual against itself.

# Visualize some of the z score deviations
tar_load(allMetrics)
vultures <- sample(unique(allMetrics$Nili_id), 10)
allMetrics %>% filter(Nili_id %in% vultures) %>% ggplot(aes(x = Nili_id, y = wrapped_degree, group = interaction(situ, Nili_id), col = situ))+geom_violin()+facet_wrap(~season, scales = "free")+geom_point(aes(x = Nili_id, y = degree)) # individuals generally *do* have degrees higher than expected by chance.

# Degree ---------------------------------------------------------------
deg_z_base <- glmmTMB(z_deg ~ situ + movement + space_use + age_group + season + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
check_predictions(deg_z_base) # ick

deg_z_full <- glmmTMB(z_deg ~ situ*season*movement + situ*season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_predictions(deg_z_full) # not any better
simulationOutput <- DHARMa::simulateResiduals(deg_z_full)
dev.off()
plot(simulationOutput, pch=".") # actually not as terrible as I'd feared
check_collinearity(deg_z_full) # first get rid of situ*season*movement

deg_z_1 <- glmmTMB(z_deg ~ situ*season + situ*movement + season*movement + situ*season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_z_1) # now remove situ*season*space

deg_z_2 <- glmmTMB(z_deg ~ situ*season + situ*movement + season*movement + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_z_2) # now remove situ*season

deg_z_3 <- glmmTMB(z_deg ~situ*movement + season*movement + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_z_3) # now let's take a look at what's significant
summary(deg_z_3) # season*space is totally n.s., as well as movement*season

deg_z_4 <- glmmTMB(z_deg ~situ*movement + situ*space_use + age_group + season + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
summary(deg_z_4) # everything else is still at least marginally significant
check_collinearity(deg_z_4) # situ*space is collinear though... but I'm gonna leave it because it's still below 5.

# so let's go with deg_z_9a
deg_z_mod <- deg_z_4

# Strength ---------------------------------------------------------------
str_z_base <- glmmTMB(z_str ~ situ + movement + space_use + age_group + season + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
check_predictions(str_z_base) # oh no...
linked %>% ggplot(aes(x = z_str, col = situ, group = interaction(situ, seasonUnique)))+geom_density()

str_z_full <- glmmTMB(z_str ~ situ*season*movement + situ*season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_predictions(str_z_full) # not any better
simulationOutput <- DHARMa::simulateResiduals(str_z_full)
dev.off()
plot(simulationOutput, pch=".") # also really bad. I wonder what I should do about this...
check_collinearity(str_z_full) # first remove situ*season*movement

str_z_1 <- glmmTMB(z_str ~ situ*season + situ*movement + season*movement + situ*season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(str_z_1) # now remove situ*season*space

str_z_2 <- glmmTMB(z_str ~ situ*season + situ*movement + season*movement + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(str_z_2) # now remove situ*season

str_z_3 <- glmmTMB(z_str ~ situ*movement + season*movement + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(str_z_3) # now can remove movement*season

str_z_4 <- glmmTMB(z_str ~ situ*movement + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(str_z_4) # now let's look at significance
summary(str_z_4) # space*season is not significant

str_z_5 <- glmmTMB(z_str ~ situ*movement + situ*space_use + age_group + season + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(str_z_5)
summary(str_z_5) # situ*movement is also n.s.

str_z_6 <- glmmTMB(z_str ~ movement + situ*space_use + age_group + season + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
summary(str_z_6) # remaining interaction is sig.

str_z_mod <- str_z_6


# Models: space use only --------------------------------------------------

## Degree ------------------------------------------------------------------

### Observed ----------------------------------------------------------------
sp_deg_obs_1 <- glmmTMB(degree ~ space_use + situ + (1|seasonUnique) + (1|Nili_id), data = linked)
check_predictions(sp_deg_obs_1)
check_model(sp_deg_obs_1) # fails linearity and normality but the others look decent.
summary(sp_deg_obs_1) # actually no effect of space use here at all.

sp_deg_obs_2 <- glmmTMB(degree ~ space_use*situ + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
summary(sp_deg_obs_2) # still no main effect of space use, but it does interact with the seasons, so we should keep that.

sp_deg_obs_3 <- glmmTMB(degree ~ space_use*situ + season + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
summary(sp_deg_obs_3) # season doesn't add anything

sp_deg_obs_4 <- glmmTMB(degree ~ space_use*situ + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
summary(sp_deg_obs_4) # there's a significant age effect. Does it make sense to add age here, if we're not going to talk about it?

sp_deg_obs_mod <- sp_deg_obs_2 # I don't think we need to get more complex than this.
# And actually, come to think of it, maybe all the model selection is not necessary. We should just do the same thing for all of them. space_use*situ, so they're easily comparable.

### Intentional ----------------------------------------------------------------
sp_deg_int_1 <- glmmTMB(z_deg ~ space_use + situ + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
summary(sp_deg_int_1)

sp_deg_int_2 <- glmmTMB(z_deg ~ space_use*situ + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
summary(sp_deg_int_2)

sp_deg_int_mod <- sp_deg_int_2

## Strength ------------------------------------------------------------------

### Observed ----------------------------------------------------------------
sp_str_obs_1 <- glmmTMB(strength ~ space_use + situ + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
summary(sp_str_obs_1)

sp_str_obs_2 <- glmmTMB(strength ~ space_use*situ + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
summary(sp_str_obs_2)

sp_str_obs_mod <- sp_str_obs_2

### Intentional ----------------------------------------------------------------
sp_str_int_1 <- glmmTMB(z_str ~ space_use + situ + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
summary(sp_str_int_1)
check_model(sp_str_int_1)

sp_str_int_2 <- glmmTMB(z_str ~ space_use*situ + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
summary(sp_str_int_2)
check_model(sp_str_int_2) # XXX this one doesn't converge and I don't understand why.

sp_str_int_mod <- sp_str_int_2