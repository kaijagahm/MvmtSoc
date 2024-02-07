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
normDegree_years_seasons_flight <- linked %>%
  filter(type == "flight") %>%
  mutate(season = factor(season, levels = c("fall", "breeding", "summer"))) %>%
  ggplot(aes(x = normDegree, col = season, group = seasonUnique))+
  geom_density(linewidth = 1.5)+
  theme_classic()+
  scale_color_manual(name = "Season", values = c(cc$fallColor, cc$breedingColor, cc$summerColor))+
  ylab("Frequency")+
  xlab("Degree (normalized)")+
  facet_wrap(~season)+
  theme(legend.position = "none", text = element_text(size = 20))
ggsave(normDegree_years_seasons_flight, filename = "fig/normDegree_years_seasons_flight.png", width = 9, height = 7)

normDegree_years_seasons_feeding <- linked %>%
  filter(type == "feeding") %>%
  mutate(season = factor(season, levels = c("fall", "breeding", "summer"))) %>%
  ggplot(aes(x = normDegree, col = season, group = seasonUnique))+
  geom_density(linewidth = 1.5)+
  theme_classic()+
  scale_color_manual(name = "Season", values = c(cc$fallColor, cc$breedingColor, cc$summerColor))+
  ylab("Frequency")+
  xlab("Degree (normalized)")+
  facet_wrap(~season)+
  theme(legend.position = "none", text = element_text(size = 20))
ggsave(normDegree_years_seasons_feeding, filename = "fig/normDegree_years_seasons_feeding.png", width = 9, height = 7)

normDegree_years_seasons_roosting <- linked %>%
  filter(type == "roosting") %>%
  mutate(season = factor(season, levels = c("fall", "breeding", "summer"))) %>%
  ggplot(aes(x = normDegree, col = season, group = seasonUnique))+
  geom_density(linewidth = 1.5)+
  theme_classic()+
  scale_color_manual(name = "Season", values = c(cc$fallColor, cc$breedingColor, cc$summerColor))+
  ylab("Frequency")+
  xlab("Degree (normalized)")+
  facet_wrap(~season)+
  theme(legend.position = "none", text = element_text(size = 20))
ggsave(normDegree_years_seasons_roosting, filename = "fig/normDegree_years_seasons_roosting.png", width = 9, height = 7)

# Let's examine zeroes for the social network measures. I know that when we calculate the social networks, we had a lot of zeroes for both degree and strength. But most of the individuals that didn't have network connections probably aren't our focal individuals for the movement measures.
networkMetrics %>% filter(degree == 0 | strength == 0) # lots of rows
linked %>% filter(degree == 0 | strength == 0) # just one individual in one season!

# # XXX It is possible this will pose a problem for modeling. I wonder if it's better to remove the zero or do some kind of other transformation?
# I think I can just remove it for now
linked <- linked %>%
  filter(normDegree > 0, normStrength > 0)

# Modeling ----------------------------------------------------------------
degree_base <- glmmTMB(normDegree ~ situ + movement + roost_div + space_use_log + age_group + season + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
check_model(degree_base) # the good news is that the variance inflation factor is quite low, so these predictors aren't prohibitively highly correlated (yay!!!!). The bad news is that the posterior predictive check and the residuals vs. fitted values plots look... bad. 
check_predictions(degree_base) # yeah this model is not a good fit for the data. No wonder, given the really squinched up values in the summers. I would have thought that including season and situation as predictors would fix this, but apparently not.
simulationOutput <- DHARMa::simulateResiduals(degree_base)
plot(simulationOutput, pch=".")
plotQQunif(simulationOutput)
plotResiduals(simulationOutput)

# What if there are different relationships between movement and degree in different seasons and situations?
degree_full <- glmmTMB(normDegree ~ situ*movement + situ*roost_div + situ*space_use_log + season*movement + season*roost_div + season*space_use_log + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian()) # this will almost certainly have very high VIFs
check_model(degree_full) # now we have some high VIFs
check_collinearity(degree_full) # the highest collinearity interaction is situ:space_use_log

degree_1 <- glmmTMB(normDegree ~ situ*movement + situ*roost_div + season*movement + season*roost_div + season*space_use_log + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
check_collinearity(degree_1) # looks like we should move the interaction of season with each of the three predictors

degree_2 <- glmmTMB(normDegree ~ situ*movement + situ*roost_div + space_use_log + season + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
check_collinearity(degree_2) # this seems a lot better
check_model(degree_2) # ok yeah this looks a lot better

# what about adding back the interaction with space use and situation?
degree_3 <- glmmTMB(normDegree ~ situ*movement + situ*roost_div + situ*space_use_log + season + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
check_collinearity(degree_3) # nope nope that's bad too

# Okay, it looks like we're going with degree_2! Let's take a look at the summary and remove any other non-significant interactions
summary(degree_2) # all remaining interactions are significant, so I want to keep them. We actually notice that season is NOT significant here, which is very odd, I think. But I don't want to remove the main effect, so we'll keep it.

degree_mod <- degree_2
# What is the relationship between movement and degree
d_eff_movement <- as.data.frame(ggeffect(degree_mod, terms = c("movement")))
ggplot(d_eff_movement, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_line(linewidth = 1)+
  geom_point(data = linked, aes(x = movement, y = normDegree), alpha = 0.5, size=  0.7)+
  ylab("Degree (normalized)")+
  xlab("Movement")+
  ggtitle("") + theme_classic()

# What is the relationship between movement and degree, by situation? (We do have a significant interaction term between movement and situation)
d_eff_movement_situ <- as.data.frame(ggeffect(degree_mod, terms = c("movement", "situ")))
movement_situ_eff <- ggplot(d_eff_movement_situ, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = movement, y = normDegree, col = situ), alpha = 0.5)+
  geom_line(aes(col = group), linewidth = 1)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  scale_fill_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Degree (normalized)")+
  xlab("Movement")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
ggsave(movement_situ_eff, file = "fig/movement_situ_eff.png", width = 9, height = 6)

d_eff_roost_div_situ <- as.data.frame(ggeffect(degree_mod, terms = c("roost_div", "situ")))
roost_div_situ_eff <- ggplot(d_eff_roost_div_situ, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = roost_div, y = normDegree, col = situ), alpha = 0.5)+
  geom_line(aes(col = group), linewidth = 1)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  scale_fill_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Degree (normalized)")+
  xlab("Roost diversification")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
ggsave(roost_div_situ_eff, file = "fig/roost_div_situ_eff.png", width = 9, height = 6)

d_eff_space_use <- as.data.frame(ggeffect(degree_mod, "space_use_log"))
space_use_situ <- ggplot(d_eff_space_use, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = space_use_log, y = normDegree, col = situ), alpha = 0.5)+
  geom_line(linewidth = 1)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Degree (normalized)")+
  xlab("Space use (log-transformed)")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
ggsave(space_use_situ, file = "fig/space_use_situ.png", width = 9, height = 6)

movement_situ_emt <- emmeans::emtrends(degree_mod, "situ", var = "movement") %>%
  as.data.frame() %>%
  mutate(situ = case_when(situ == "Ro" ~ "Roosting",
                          situ == "Fl" ~ "Flight",
                          situ == "Fe" ~ "Feeding"))
movement_situ_emt # All three situation-specific relationships are significant
# Let's use this to get the forest plot
movement_situ_forest <- movement_situ_emt %>%
  as.data.frame() %>%
  ggplot(aes(x = situ, y = movement.trend, col = situ))+
  geom_point(size = 6)+
  geom_errorbar(aes(x = situ, ymin = lower.CL, ymax = upper.CL), 
                width = 0, linewidth = 2)+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  theme(text = element_text(size = 20), legend.position = "none")+
  ylab("Movement effect")+
  xlab("Situation")+
  coord_flip()
ggsave(movement_situ_forest, file = "fig/movement_situ_forest.png", width = 10, height = 6)

roost_div_situ_emt <- emmeans::emtrends(degree_mod, "situ", var = "roost_div") %>%
  as.data.frame() %>%
  mutate(situ = case_when(situ == "Ro" ~ "Roosting",
                          situ == "Fl" ~ "Flight",
                          situ == "Fe" ~ "Feeding"))
roost_div_situ_emt 
roost_div_situ_forest <- roost_div_situ_emt %>%
  as.data.frame() %>%
  ggplot(aes(x = situ, y = roost_div.trend, col = situ))+
  geom_point(size = 6)+
  geom_errorbar(aes(x = situ, ymin = lower.CL, ymax = upper.CL), 
                width = 0, linewidth = 2)+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  theme(text = element_text(size = 20), legend.position = "none")+
  ylab("Roost diversification effect")+
  xlab("Situation")+
  coord_flip()
ggsave(roost_div_situ_forest, file = "fig/roost_div_situ_forest.png", width = 10, height = 6)
  
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
-------------------------------------------------
# We now have 3 models. Let's compile and tidy their outputs.
mods <- list("d" = degree_mod, "s" = strength_mod, "e" = evenness_mod)
save(mods, file = "data/derived/mods.Rda")

# here's info on how to interpret that log-transformed regression: https://data.library.virginia.edu/interpreting-log-transformations-in-a-linear-model/