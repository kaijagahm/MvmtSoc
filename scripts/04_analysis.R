library(car) # for p-values in mixed models?
# In lieu of tidyverse
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

load("data/linked.Rda")

# Set ggplot theme to classic
theme_set(theme_classic())

load("data/derived/cc.Rda")

# Here's an interesting reference article for some modeling stuff: https://biol607.github.io/lab/12_gzlm.html
linked <- linked %>%
  mutate(season = factor(season, levels = c("breeding", "summer", "fall")))

# Remove individuals that don't have movement information
colSums(is.na(linked)) # lots of NA's
linked <- linked %>%
  filter(!is.na(PC1)) # note that because I joined the sex/age information along with the movement information (instead of along with the social information), if I ever want to *not* filter out those individuals, I'll have to go back and add sex/age info for the additional birds. Don't need this for now, so I'm skipping it. 
colSums(is.na(linked)) # okay now we have no more NA's except in sex. Yay!

# Mixed models -------------------------------------------------------------------
# Response variables:
# Relative degree
# Relative strength
# Evenness

# Predictors:
# PC1 (continuous)
# PC2 (continuous)
# age (cat: adult/juv_sub)
# sex (cat: m/f)
# season (cat: b/nb)
# year (cat)
# mean points per day (continuous)
# Nili_id (random effect) (1|Nili_id)

# Question from vulture meeting--should I care about high VIF values for interaction terms?
# No, they're fine: https://stats.stackexchange.com/questions/52856/vif-values-and-interactions-in-multiple-regression, https://stats.stackexchange.com/questions/274320/how-to-deal-with-interaction-terms-vif-score.
# So, I think I need to rethink the model selection here. Maybe do it just based on significance?

# Make situation as a factor so we can include it in the model -------------
linked <- linked %>%
  mutate(situ = case_when(type == "flight" ~ "Fl", 
                          type == "feeding" ~ "Fe",
                          type == "roosting" ~ "Ro")) %>%
  mutate(seasonUnique = paste(year, season, sep = "_"))
table(linked$situ) # as expected, we have more data for the roost network than for feeding and flight.

# Scale the response variables: degree, strength, evenness
scaled_d <- scale(linked$degree)
scaled_s <- scale(linked$strength)
scaled_e <- scale(linked$evenness)
save(scaled_d, file = "data/scaled_d.Rda")
save(scaled_s, file = "data/scaled_s.Rda")
save(scaled_e, file = "data/scaled_e.Rda")

linked$degree_scl <- as.vector(scaled_d)
linked$strength_scl <- as.vector(scaled_s)
linked$evenness_scl <- as.vector(scaled_e)

getOption("rstudio.help.showDataPreview")# create and save a new dataset for modeling, so we can load it later
minstrength <- linked %>% filter(strength != 0) %>% pull(strength) %>% min()
forModeling <- linked %>%
  mutate(strength = strength + (minstrength/2))
save(forModeling, file = "data/forModeling.Rda")

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
save(mods, file = "data/mods.Rda")

# here's info on how to interpret that log-transformed regression: https://data.library.virginia.edu/interpreting-log-transformations-in-a-linear-model/