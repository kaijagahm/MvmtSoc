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

load("data/linked.Rda")

# Set ggplot theme to classic
theme_set(theme_classic())

load("data/cc.Rda")

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
forModeling <- linked
save(forModeling, file = "data/forModeling.Rda")

## Degree ------------------------------------------------------------------
# 2023-06-05 going back to gaussian for now
degree_noint <- lmer(degree_scl ~ situ + PC1 + PC2 + age_group + season + (1|seasonUnique) + (1|Nili_id), data = forModeling)
summary(degree_noint)
#check_model(degree_noint) # looks okay, candidate. No correlated predictors.
plot(DHARMa::simulateResiduals(degree_noint), pch=".")

degree_max <- lmer(degree_scl ~ situ*PC1*age_group + situ*PC1*season + situ*PC2*age_group + situ*PC2*season + PC1*age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = forModeling)
#check_model(degree_max) # still looks basically ok, except for the vif's, which we can ignore
summary(degree_max) # okay, we can remove most of these. Going to replace them with two-way interactions and then remove duplicates.

degree_2 <- lmer(degree_scl ~ situ*PC1*age_group + situ*PC1 + situ*season + PC1*season + situ*PC2 + situ*age_group + PC2*age_group + situ*PC2 + PC2*season + PC1*age_group + age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = forModeling)
summary(degree_2) #things that can be removed: situ*PC1*age

degree_3 <- lmer(degree_scl ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC1*season + situ*PC2 + PC2*age_group + PC2*season + age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = forModeling)
summary(degree_3) # now would like to remove some two-way interactions, but can't do that until we remove more three-ways. I don't feel that there's much justification in keeping situ*age_group*season with those marginal values, so let's start by removing those.

degree_4 <- lmer(degree_scl ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC1*season + situ*PC2 + PC2*age_group + PC2*season + age_group*season + PC2*age_group*season + (1|seasonUnique) + (1|Nili_id), data = forModeling)
summary(degree_4) # next would like to remove age_group*PC2 or season*PC2, but can't do that without removing the 3-way interaction first.

degree_5 <- lmer(degree_scl ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC1*season + situ*PC2 + PC2*age_group + PC2*season + age_group*season + (1|seasonUnique) + (1|Nili_id), data = forModeling)
summary(degree_5) # Ok, this free us up to remove some other things, starting with season*PC2.

degree_6 <- lmer(degree_scl ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC1*season + situ*PC2 + PC2*age_group + age_group*season + (1|seasonUnique) + (1|Nili_id), data = forModeling)
summary(degree_6) # hmm not entirely clear what to remove next--let's remove PC1*season first I guess because the effect sizes are smaller?

degree_7 <- lmer(degree_scl ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + situ*PC2 + PC2*age_group + age_group*season + (1|seasonUnique) + (1|Nili_id), data = forModeling)
summary(degree_7) # now I guess we could try getting rid of situ*PC2, but probably tbh we don't want to do either this or the previous one.

degree_8 <- lmer(degree_scl ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC2*age_group + age_group*season + (1|seasonUnique) + (1|Nili_id), data = forModeling)
summary(degree_8) # I guess we could also remove situ*age...

degree_9 <- lmer(degree_scl ~ situ*PC1 + PC1*age_group + situ*season + PC2*age_group + age_group*season + (1|seasonUnique) + (1|Nili_id), data = forModeling)
summary(degree_9) # I don't want to remove situ*season because that effect size on summer is HUGE. So let's stop here.

compare_performance(degree_noint, degree_max, degree_2, degree_3, degree_4, degree_5, degree_6, degree_7, degree_8, degree_9, rank = T)

degree_mod <- degree_max #xxx this is weird, come back to it
#check_model(degree_mod) # that looks pretty glorious, except for the VIF's, which we can probably ignore anyway.

## Strength ------------------------------------------------------------------
# Ok, for the sake of this conference, though, I'm going to go back to a gaussian.
strength_noint <- lmer(strength_scl ~ PC1 + PC2 + situ + season + age_group + (1|seasonUnique) + (1|Nili_id), data = forModeling)
plot(DHARMa::simulateResiduals(strength_noint), pch=".") # YUCK
check_model(strength_noint) #YUCK

# what about log-transforming strength? Will have to do this to the un-scaled values.
# Seems to be a good idea to add a tiny bit to all of the data, not just the zeroes.
minstrength <- forModeling %>%
  filter(strength != 0) %>%
  pull(strength) %>% min()
sData <- forModeling %>%
  mutate(strength = strength + minstrength) %>%
  mutate(logstrength = log(strength))

strength_noint_log <- lmer(logstrength ~ PC1 + PC2 + situ + season + age_group + (1|seasonUnique) + (1|Nili_id), data = sData)
plot(DHARMa::simulateResiduals(strength_noint_log), pch=".")
check_model(strength_noint_log) #This looks decent actually! Will need to check what happens if I remove the outliers.

outliers <- which(resid(strength_noint_log) < -2) # four outliers

strength_noint_log_nooutliers <- lmer(logstrength ~ PC1 + PC2 + situ + season + age_group + (1|seasonUnique) + (1|Nili_id), data = sData[-outliers,])
plot(DHARMa::simulateResiduals(strength_noint_log_nooutliers), pch=".") 
check_model(strength_noint_log_nooutliers) # still looks like a good fit! will need to double-check the outliers vs. no outliers version for the final winning model, but for now this seems like the way to go.

# Okay, time to do the rest of the models. I'm going to name them without log in the name just to make things easier.
strength_max <- lmer(logstrength ~ situ*PC1*age_group + situ*PC1*season + situ*PC2*age_group + situ*PC2*season + PC1*age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = sData)
check_model(strength_max) # still looks okay!
summary(strength_max) # looks like we can remove almost all of these, starting with the ones that aren't even marginal: PC1:age:season and situ:PC1:age

strength_2 <- lmer(logstrength ~ situ*PC1 + situ*age_group + PC1*age_group + situ*PC1*season + situ*PC2*age_group + situ*PC2*season + PC1*season + age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = sData)
summary(strength_2) # now removing situ*season*PC2.

strength_3 <- lmer(logstrength ~ situ*PC1 + situ*age_group + PC1*age_group + situ*PC1*season + situ*PC2*age_group + situ*PC2 + situ*season + PC2*season + PC1*season + age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = sData)
summary(strength_3) # now I think we can remove situ*age*PC2

strength_4 <- lmer(logstrength ~ situ*PC1 + situ*age_group + PC1*age_group + situ*PC1*season + situ*PC2 + situ*age_group + PC2*age_group + situ*PC2 + situ*season + PC2*season + PC1*season + age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = sData)
summary(strength_4) # now we can remove situ*PC1*season

strength_5 <- lmer(logstrength ~ situ*PC1 + situ*age_group + PC1*age_group + situ*PC1 + situ*season + PC1*season + situ*PC2 + situ*age_group + PC2*age_group + situ*PC2 + situ*season + PC2*season + PC1*season + age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = sData)
summary(strength_5) # that seems like a good place to stop

compare_performance(strength_noint_log, strength_max, strength_2, strength_3, strength_4, strength_5, rank = T)

strength_mod <- strength_3
check_model(strength_mod)

outliers <- which(resid(strength_mod) < -2)
strength_mod_nooutliers <- lmer(logstrength ~ situ*PC1 + situ*age_group + PC1*age_group + situ*PC1*season + situ*PC2*age_group + situ*PC2 + situ*season + PC2*season + PC1*season + age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = sData[-outliers,])

check_model(strength_mod_nooutliers) # I suspect this is going to be the same, but I'm going to make a note to save this one separately and double check the results.

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
mods <- list("d" = degree_mod, "s" = strength_mod, "s_nooutliers" = strength_mod_nooutliers, "e" = evenness_mod)
save(mods, file = "data/mods.Rda")
save(sData, file = "data/sData.Rda") # for the strength models
 
# here's info on how to interpret that log-transformed regression: https://data.library.virginia.edu/interpreting-log-transformations-in-a-linear-model/