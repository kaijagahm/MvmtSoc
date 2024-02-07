library(car) # for p-values in mixed models?
library(tidyverse)
library(sf)
library(factoextra)
library(ggfortify)
library(ggpmisc) # for more details on a linear regression plot
library(lme4) # for mixed-effects models
library(ggpubr) # for adding p-values to boxplots
library(rstatix) # for adding p-values to boxplots
library(easystats) # for modeling and comparisons
library(performance) # for modeling and comparisons
library(lmerTest) # for p-values in mixed models?
library(glmmTMB) # more complicated than lme4; allows for beta distributions
library(DHARMa) # for testing glmmTMB models
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
library(sjPlot)
library(broom.mixed)
library(jtools) # similar to sjplot, for forest plots
library(emmeans) # estimated marginal means/trends

# Set ggplot theme to classic
theme_set(theme_classic())

# Load network metrics and movement variables
load("data/calcSocial/networkMetrics.Rda")
networkMetrics <- networkMetrics %>% rename("seasonUnique" = "season")
load("data/new_movement_vars.Rda")
load("data/dataPrep/season_names.Rda")
load("data/derived/cc.Rda")

# Join the two datasets (there will be 3 rows per individual per season, for the 3 different situations)
linked <- new_movement_vars %>%
  left_join(networkMetrics, by = c("Nili_id", "seasonUnique"))
nrow(linked) == 3*nrow(new_movement_vars)

# Housekeeping
linked <- linked %>%
  mutate(season = stringr::str_extract(seasonUnique, "[a-z]+"),
         year = as.numeric(stringr::str_extract(seasonUnique, "[0-9]+")),
         seasonUnique = factor(seasonUnique, levels = season_names),
         sex = factor(sex),
         season = factor(season, levels = c("breeding", "summer", "fall")))

# Check for NA's (we shouldn't have any because of the direction of the join)
colSums(is.na(linked)) # awesome

# Create a shorter version of "type" for easier interpretability
linked <- linked %>%
  mutate(situ = case_when(type == "flight" ~ "Fl", 
                          type == "feeding" ~ "Fe",
                          type == "roosting" ~ "Ro"))
table(linked$situ)


# Examine response variable distributions ---------------------------------
linked %>%
  ggplot(aes(x = normDegree, col = season, group = seasonUnique))+
  geom_density()+
  scale_color_manual(name = "Season", values = c(cc$breedingColor, cc$summerColor, cc$fallColor))+
  theme_minimal() # what is up with the summers??? Why do they have so much higher degree in summer? This much difference seems unexpected.

linked %>%
  ggplot(aes(x = normStrength, col = season, group = seasonUnique))+
  geom_density()+
  scale_color_manual(name = "Season", values = c(cc$breedingColor, cc$summerColor, cc$fallColor))+
  theme_minimal() # XXX what is up with the summers??? Why do they have so much higher degree in summer? This much difference seems unexpected.

# Let's examine zeroes for the social network measures. I know that when we calculate the social networks, we had a lot of zeroes for both degree and strength. But most of the individuals that didn't have network connections probably aren't our focal individuals for the movement measures.
networkMetrics %>% filter(degree == 0 | strength == 0) # lots of rows
linked %>% filter(degree == 0 | strength == 0) # just one individual in one season!

# # XXX It is possible this will pose a problem for modeling. I wonder if it's better to remove the zero or do some kind of other transformation?
# I think I can just remove it for now
linked <- linked %>%
  filter(normDegree > 0, normStrength > 0)

# Modeling ----------------------------------------------------------------
degree_base <- lmer(normDegree ~ situ + movement + roost_div + space_use_log + age_group + season + (1|seasonUnique)+(1|Nili_id), data = linked)
check_model(degree_base) # the good news is that the variance inflation factor is quite low, so these predictors aren't prohibitively highly correlated (yay!!!!). The bad news is that the posterior predictive check and the residuals vs. fitted values plots look... bad. 
plot(DHARMa::simulateResiduals(degree_base), pch=".")








## Degree ------------------------------------------------------------------
# 2023-06-05 going back to gaussian for now
degree_noint <- lmer(degree_scl ~ situ + PC1 + PC2 + age_group + season + (1|seasonUnique) + (1|Nili_id), data = forModeling)
summary(degree_noint)
#check_model(degree_noint) # looks okay, candidate. No correlated predictors.
plot(DHARMa::simulateResiduals(degree_noint), pch=".")

# PC1*situ*season: there might be different relationships between PC1 and degree in different seasons and situations
# PC1*age_group*season: adults and juveniles might differ in their relationships between PC1 and degree, and those differences could be different by season

degree_max_max <- lmer(degree_scl ~ PC1*situ*season*age_group + PC2*situ*season*age_group + (1|seasonUnique) + (1|Nili_id), data = forModeling)
# check_model(degree_max_max)
# plot(DHARMa::simulateResiduals(degree_max_max), pch=".")
summary(degree_max_max) # Can start by removing PC1*situ*season and PC2*situ*season.

degree_max <- lmer(degree_scl ~ PC1*situ*season + PC2*situ*season + age_group + (1|seasonUnique) + (1|Nili_id), data = forModeling)
check_model(degree_max)
plot(DHARMa::simulateResiduals(degree_max), pch=".")
summary(degree_max) # Can start by removing PC1*situ*season and PC2*situ*season.

degree_2 <- lmer(degree_scl ~ PC1*situ + PC1*season + PC2*situ + PC2*season + age_group + (1|seasonUnique) + (1|Nili_id), data = forModeling)
summary(degree_2) # Can remove PC2*season and PC1*season

degree_3 <- lmer(degree_scl ~ PC1*situ + PC2*situ + age_group + season + (1|seasonUnique) + (1|Nili_id), data = forModeling)
summary(degree_3) # don't actually need the season effect here at all

# degree_4 <- lmer(degree_scl ~ PC1*situ + PC2*situ + age_group + (1|seasonUnique) + (1|Nili_id), data = forModeling)
# summary(degree_4)

compare_performance(degree_noint, degree_max_max, degree_max, degree_2, degree_3, rank = T)

degree_mod <- degree_3 #xxx this is weird, why is the maximal model retained?

#xxx trying out emmeans:
degree_test <- lmer(degree_scl ~ PC1*situ + PC2*situ + (1|Nili_id), data = forModeling)
(degree.emm <- emmeans(degree_test, "situ", var = "PC2") )
pairs(degree.emm)
fiber.lm <- lm(strength ~ diameter*machine, data=fiber)
# Obtain slopes for each machine ...
( fiber.emt <- emtrends(fiber.lm, "machine", var = "diameter") )
# ... and pairwise comparisons thereof
pairs(fiber.emt)
#### xxx end

## Strength ------------------------------------------------------------------
# Ok, for the sake of this conference, though, I'm going to go back to a gaussian.
strength_noint <- lmer(strength_scl ~ PC1 + PC2 + situ + season + age_group + (1|seasonUnique) + (1|Nili_id), data = forModeling)
plot(DHARMa::simulateResiduals(strength_noint), pch=".") # YUCK
check_model(strength_noint) #YUCK

strength_noint_log <- lmer(log(strength) ~ PC1 + PC2 + situ + season + age_group + (1|seasonUnique) + (1|Nili_id), data = forModeling)
plot(DHARMa::simulateResiduals(strength_noint_log), pch=".")
check_model(strength_noint_log) #This looks decent actually! Will need to check what happens if I remove the outliers.

outliers <- which(resid(strength_noint_log) < -2) # four outliers

strength_noint_log_nooutliers <- lmer(log(strength) ~ PC1 + PC2 + situ + season + age_group + (1|seasonUnique) + (1|Nili_id), data = forModeling[-outliers,])
plot(DHARMa::simulateResiduals(strength_noint_log_nooutliers), pch=".") 
check_model(strength_noint_log_nooutliers) # still looks like a good fit! will need to double-check the outliers vs. no outliers version for the final winning model, but for now this seems like the way to go.

# Okay, time to do the rest of the models. I'm going to name them without log in the name just to make things easier.
strength_max <- lmer(log(strength) ~ PC1*situ*season + PC2*situ*season + age_group + (1|seasonUnique) + (1|Nili_id), data = forModeling)
summary(strength_max) # Can remove PC2*situ*season

strength_2 <- lmer(log(strength) ~ PC1*situ*season + PC2*situ + PC2*season + age_group + (1|seasonUnique) + (1|Nili_id), data = forModeling)
summary(strength_2) # can remove PC2*season and PC2*situ

strength_3 <- lmer(log(strength) ~ PC1*situ*season + PC2 + age_group + (1|seasonUnique) + (1|Nili_id), data = forModeling)
summary(strength_3) # I'm going to keep PC1*situ*season even though only one of them is marginally significant--I'll just talk about these.

strength_mod <- strength_3

check_model(strength_mod) # no major outliers

## Evenness ----------------------------------------------------------------
# Turns out, if we invert the evenness distribution (and call it unevenness!) and then log-transform it, we get what looks like an almost perfect gaussian!
hist(forModeling$evenness) # original
hist(-1*(forModeling$evenness-1)) # inverted--right-skewed distribution is easier to model
hist(log(-1*(forModeling$evenness-1))) # beautiful! Now, this may not be true of each of the groups, but it's a start.

eData <- forModeling %>%
  mutate(evenness_inverted_log = log(-1*(evenness-1)),
         evenness_inverted_log_scaled = datawizard::standardize(evenness_inverted_log))

evenness_noint <- lmer(evenness_scl ~ situ + PC1 + PC2 + age_group + season + (1|seasonUnique) + (1|Nili_id), data = eData)
check_model(evenness_noint)
plot(DHARMa::simulateResiduals(evenness_noint), pch=".") # this does not look good, still. The homogeneity of variance assumption is really violated.
# 
# 
# evenness_max <- lmer(evenness_scl ~ PC1*season*situ + PC1*season*age_group + PC1*situ*age_group + PC2*season*situ + PC2*season*age_group + PC2*situ*age_group + season*situ*age_group + (1|seasonUnique) + (1|Nili_id), data = forModeling)
# #check_model(evenness_max)
# summary(evenness_max) # can remove situ*age*PC2, season*situ*PC2, PC1*situ*age_group, PC1*season*age_group, PC1*season*situ
# 
# evenness_2 <- lmer(evenness_scl ~ PC1*season + PC1*situ + season*situ + PC1*age_group + season*age_group + situ*age_group + PC2*season + PC2*situ + PC2*season*age_group + PC2*age_group + situ*age_group + season*situ*age_group + (1|seasonUnique) + (1|Nili_id), data = forModeling)
# summary(evenness_2) # want to remove more 2-way interactions but have to get rid of the three-ways first. Let's do season*age_group*PC2 first I guess
# 
# evenness_3 <- lmer(evenness_scl ~ PC1*season + PC1*situ + season*situ + PC1*age_group + season*age_group + situ*age_group + PC2*season + PC2*situ + PC2*age_group + situ*age_group + season*situ*age_group + (1|seasonUnique) + (1|Nili_id), data = forModeling)
# summary(evenness_3) # now the two-ways become significant. Just for kicks, let's get rid of the last 3-way interaction.
# 
# evenness_4 <- lmer(evenness_scl ~ PC1*season + PC1*situ + season*situ + PC1*age_group + season*age_group + situ*age_group + PC2*season + PC2*situ + PC2*age_group + situ*age_group + (1|seasonUnique) + (1|Nili_id), data = forModeling)
# summary(evenness_4) # can remove situ*PC2, PC1*season
# 
# evenness_5 <- lmer(evenness_scl ~ PC1*situ + season*situ + PC1*age_group + season*age_group + situ*age_group + PC2*season + PC2*age_group + situ*age_group + (1|seasonUnique) + (1|Nili_id), data = forModeling)
# summary(evenness_5) # time to stop!
# 
# compare_performance(evenness_noint, evenness_max, evenness_2, evenness_3, evenness_4, evenness_5, rank = T)
# 
# evenness_mod <- evenness_2
evenness_mod <- NULL # for now

# Get model effects ----------------------------------------------------------
# We now have 3 models. Let's compile and tidy their outputs.
mods <- list("d" = degree_mod, "s" = strength_mod, "e" = evenness_mod)
save(mods, file = "data/derived/mods.Rda")

# here's info on how to interpret that log-transformed regression: https://data.library.virginia.edu/interpreting-log-transformations-in-a-linear-model/