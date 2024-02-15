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
library(ggeffects)
library(gtsummary)

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

save(linked, file = "data/mixedModels/linked.Rda")


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

# Degree ------------------------------------------------------------------
degree_base <- glmmTMB(normDegree ~ situ + movement + roost_div + space_use + age_group + season + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
check_model(degree_base) # the good news is that the variance inflation factor is quite low, so these predictors aren't prohibitively highly correlated (yay!!!!). The bad news is that the posterior predictive check and the residuals vs. fitted values plots look... bad. 
check_predictions(degree_base) # yeah this model is not a good fit for the data. No wonder, given the really squinched up values in the summers. I would have thought that including season and situation as predictors would fix this, but apparently not.
simulationOutput <- DHARMa::simulateResiduals(degree_base)
plot(simulationOutput, pch=".")
plotQQunif(simulationOutput)
plotResiduals(simulationOutput)

# What if there are different relationships between movement and degree in different seasons and situations?
degree_full <- glmmTMB(normDegree ~ situ*movement + situ*roost_div + situ*space_use + season*movement + season*roost_div + season*space_use + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian()) # this will almost certainly have very high VIFs
check_model(degree_full) # now we have some high VIFs
check_collinearity(degree_full) # the highest collinearity interaction is roost_div:season. Let's remove it, making sure that both roost_div and season stay in the model in some other capacity.

degree_1 <- glmmTMB(normDegree ~ situ*movement + situ*roost_div + situ*space_use + season*movement + season*space_use + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
check_collinearity(degree_1) # a couple more with moderate correlations that we should get rid of: situ:roost_div and situ:space_use. Starting with getting rid of situ:roost_div, adding roost_div back in as a main effect

degree_2 <- glmmTMB(normDegree ~ situ*movement + situ*space_use + season*movement + roost_div + season*space_use + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
check_collinearity(degree_2) # this allows them all to drop below 5, which I'm happy with.
check_model(degree_2)
summary(degree_2) # looks like neither space_use:season nor movement:season are significant, so let's remove those.

degree_3 <- glmmTMB(normDegree ~ situ*movement + situ*space_use + season*movement + roost_div  + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
summary(degree_3) # after removing season*space_use, we still don't have a significant interaction between movement and season

degree_4 <- glmmTMB(normDegree ~ situ*movement + situ*space_use + season + roost_div  + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
summary(degree_4) # made sure to add season bask as a main effect. 
check_collinearity(degree_4) # as a bonus, now none of our VIFs are above 3. Yay!

degree_mod <- degree_4


# Degree plots ------------------------------------------------------------
# Interaction plots for degree model

## situ:space_use (effect plot) -----------------------------------------------------
d_eff_situ_space <- as.data.frame(ggeffect(degree_mod, terms = c("space_use", "situ")))
plot_d_eff_situ_space <- ggplot(d_eff_situ_space, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = space_use, y = normDegree, col = situ), alpha = 0.5)+
  geom_line(aes(col = group), linewidth = 1)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  scale_fill_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Degree (normalized)")+
  xlab("Space use (log-transformed)")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_d_eff_situ_space
ggsave(plot_d_eff_situ_space, file = "fig/plot_d_eff_situ_space.png", width = 9, height = 6)

## situ:space_use (forest plot) --------------------------------------------
d_emt_situ_space <- emmeans::emtrends(degree_mod, "situ", var = "space_use") %>%
  as.data.frame() %>%
  mutate(situ = case_when(situ == "Ro" ~ "Roosting",
                          situ == "Fl" ~ "Flight",
                          situ == "Fe" ~ "Feeding"))
d_emt_situ_space 
plot_d_emt_situ_space <- d_emt_situ_space %>%
  as.data.frame() %>%
  ggplot(aes(x = situ, y = space_use.trend, col = situ))+
  geom_point(size = 6)+
  geom_errorbar(aes(x = situ, ymin = lower.CL, ymax = upper.CL), 
                width = 0, linewidth = 2)+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  theme(text = element_text(size = 20), legend.position = "none")+
  ylab("Space use effect")+
  xlab("Situation")+
  coord_flip()
ggsave(plot_d_emt_situ_space, file = "fig/plot_d_emt_situ_space.png", width = 10, height = 6)


## situ:movement (effect plot) ---------------------------------------------
d_eff_situ_movement <- as.data.frame(ggeffect(degree_mod, terms = c("movement", "situ")))
plot_d_eff_situ_movement <- ggplot(d_eff_situ_movement, aes(x, predicted))+
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
plot_d_eff_situ_movement
ggsave(plot_d_eff_situ_movement, file = "fig/plot_d_eff_situ_movement.png", width = 9, height = 6)


## situ:movement (forest plot) ---------------------------------------------
d_emt_situ_movement <- emmeans::emtrends(degree_mod, "situ", var = "movement") %>%
  as.data.frame() %>%
  mutate(situ = case_when(situ == "Ro" ~ "Roosting",
                          situ == "Fl" ~ "Flight",
                          situ == "Fe" ~ "Feeding"))
d_emt_situ_movement 
plot_d_emt_situ_movement <- d_emt_situ_movement %>%
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
ggsave(plot_d_emt_situ_movement, file = "fig/plot_d_emt_situ_movement.png", width = 10, height = 6)

## roost_div (effect plot) -------------------------------------------------
d_eff_roost <- as.data.frame(ggeffect(degree_mod, terms = "roost_div"))
plot_d_eff_roost <- ggplot(d_eff_roost, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = roost_div, y = normDegree, col = situ), alpha = 0.5)+
  geom_line(linewidth = 1)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Degree (normalized)")+
  xlab("Roost diversification")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_d_eff_roost
ggsave(plot_d_eff_roost, file = "fig/plot_d_eff_roost.png", width = 9, height = 6)

## season (effect plot) ----------------------------------------------------
d_eff_season <- as.data.frame(ggeffect(degree_mod, terms = "season"))
plot_d_eff_season <- ggplot(d_eff_season, aes(x, predicted))+
  geom_point(aes(x, y = predicted, col = x), size = 6)+
  geom_errorbar(aes(x, ymin = conf.low, ymax = conf.high, col = x), width = 0, linewidth = 2)+
  scale_color_manual(values = c(cc$breedingColor, cc$summerColor, cc$fallColor))+
  ylab("Degree (normalized)")+
  xlab("Season")+ 
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16),
        legend.position = "none")
plot_d_eff_season
ggsave(plot_d_eff_season, file = "fig/plot_d_eff_season.png", width = 9, height = 6)

## age group (effect plot) -------------------------------------------------
d_eff_age <- as.data.frame(ggeffect(degree_mod, terms = "age_group")) %>%
  mutate(x = factor(x, levels = c("juv", "sub", "adult"))) %>%
  mutate(x = fct_recode(x, 
             "Juvenile" = "juv",
             "Sub-adult" = "sub",
             "Adult" = "adult"))
plot_d_eff_age <- ggplot(d_eff_age, aes(x, predicted))+
  geom_point(aes(x, y = predicted, col = x), size = 6)+
  geom_errorbar(aes(x, ymin = conf.low, ymax = conf.high, col = x), width = 0, linewidth = 2)+
  ylab("Degree (normalized)")+
  xlab("Age group")+ 
  scale_color_manual(values = c("firebrick1", "firebrick3", "firebrick4"))+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16),
        legend.position = "none")
plot_d_eff_age
ggsave(plot_d_eff_age, file = "fig/plot_d_eff_age.png", width = 9, height = 6)

# Strength ----------------------------------------------------------------
strength_base <- glmmTMB(normStrength ~ situ + movement + roost_div + space_use + age_group + season + (1|seasonUnique)+(1|Nili_id), data = linked, family = beta_family())
check_model(strength_base) # this actually looks pretty good, shockingly! And I only had to exclude one individual for having a 0 for strength.
simulationOutput <- DHARMa::simulateResiduals(strength_base)
plot(simulationOutput, pch=".") # this is kinda bad but still better than the degree model!

# Let's see if we want any interactions
strength_full <- glmmTMB(normStrength ~ situ*movement + situ*roost_div + situ*space_use + season*movement + season*roost_div + season*space_use + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = beta_family())
check_model(strength_full) # now we have some high VIFs
check_collinearity(strength_full) # highest vif is roost_div:season

strength_1 <- glmmTMB(normStrength ~ situ*movement + situ*roost_div + situ*space_use + season*movement + season*space_use + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = beta_family())
check_collinearity(strength_1) #wow, that lowered the other VIFs a lot too! Let's now remove situ*roost_div. Have to remember to introduce a main effect term for roost_div so it doesn't go away.

strength_2 <- glmmTMB(normStrength ~ situ*movement + situ*space_use + season*movement + season*space_use + roost_div + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = beta_family())
check_collinearity(strength_2) # now these are all under 5, and actually they're all under 4.
summary(strength_2)# buuuut we see that a lot of our remaining interactions are not significant. Start by removing space_use*season

strength_3 <- glmmTMB(normStrength ~ situ*movement + situ*space_use + season*movement +  roost_div + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = beta_family())
summary(strength_3) # now movement*season is not significant

strength_4 <- glmmTMB(normStrength ~ situ*movement + situ*space_use + season + roost_div + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = beta_family())
summary(strength_4) # aand we can remove situ*movement too.

strength_5 <- glmmTMB(normStrength ~ situ*space_use + season + roost_div + movement + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = beta_family())
summary(strength_5) 
check_model(strength_5) # we have non-significant trends for movement, season, and space use, but I'm not removing those because they're main effects.

strength_mod <- strength_5

# Strength plots ----------------------------------------------------------

## situ:space_use (effect plot) --------------------------------------------
s_eff_situ_space <- as.data.frame(ggeffect(strength_mod, terms = c("space_use", "situ")))
plot_s_eff_situ_space <- ggplot(s_eff_situ_space, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = space_use, y = normStrength, col = situ), alpha = 0.5)+
  geom_line(aes(col = group), linewidth = 1)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  scale_fill_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Strength (normalized)")+
  xlab("Space use (log-transformed)")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_s_eff_situ_space
ggsave(plot_s_eff_situ_space, file = "fig/plot_s_eff_situ_space.png", width = 9, height = 6)

## situ:space_use (forest plot) --------------------------------------------
s_emt_situ_space <- emmeans::emtrends(strength_mod, "situ", var = "space_use") %>%
  as.data.frame() %>%
  mutate(situ = case_when(situ == "Ro" ~ "Roosting",
                          situ == "Fl" ~ "Flight",
                          situ == "Fe" ~ "Feeding"))
s_emt_situ_space 
plot_s_emt_situ_space <- s_emt_situ_space %>%
  as.data.frame() %>%
  ggplot(aes(x = situ, y = space_use.trend, col = situ))+
  geom_point(size = 6)+
  geom_errorbar(aes(x = situ, ymin = asymp.LCL, ymax = asymp.UCL), 
                width = 0, linewidth = 2)+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  theme(text = element_text(size = 20), legend.position = "none")+
  ylab("Space use effect")+
  xlab("Situation")+
  coord_flip()
plot_s_emt_situ_space
ggsave(plot_s_emt_situ_space, file = "fig/plot_s_emt_situ_space.png", width = 10, height = 6)

## age group (effect plot) -------------------------------------------------
s_eff_age <- as.data.frame(ggeffect(strength_mod, terms = "age_group")) %>%
  mutate(x = factor(x, levels = c("juv", "sub", "adult"))) %>%
  mutate(x = fct_recode(x, 
                        "Juvenile" = "juv",
                        "Sub-adult" = "sub",
                        "Adult" = "adult"))
plot_s_eff_age <- ggplot(s_eff_age, aes(x, predicted))+
  geom_point(aes(x, y = predicted, col = x), size = 6)+
  geom_errorbar(aes(x, ymin = conf.low, ymax = conf.high, col = x), width = 0, linewidth = 2)+
  ylab("Strength (normalized)")+
  xlab("Age group")+ 
  scale_color_manual(values = c("firebrick1", "firebrick3", "firebrick4"))+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16),
        legend.position = "none")
plot_s_eff_age
ggsave(plot_s_eff_age, file = "fig/plot_s_eff_age.png", width = 9, height = 6)


## movement (effect plot) --------------------------------------------------
s_eff_movement <- as.data.frame(ggeffect(strength_mod, terms = "movement"))
plot_s_eff_movement <- ggplot(s_eff_movement, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = movement, y = normStrength, col = situ), alpha = 0.5)+
  geom_line(linewidth = 1, linetype = 2)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Strength (normalized)")+
  xlab("Movement")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_s_eff_movement
ggsave(plot_s_eff_movement, file = "fig/plot_s_eff_movement.png", width = 9, height = 6)

## roost_div (effect plot) --------------------------------------------------
s_eff_roost <- as.data.frame(ggeffect(strength_mod, terms = "roost_div"))
plot_s_eff_roost <- ggplot(s_eff_roost, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = roost_div, y = normStrength, col = situ), alpha = 0.5)+
  geom_line(linewidth = 1)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Strength (normalized)")+
  xlab("Roost diversification")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_s_eff_roost
ggsave(plot_s_eff_roost, file = "fig/plot_s_eff_roost.png", width = 9, height = 6)

## season (effect plot) ----------------------------------------------------
s_eff_season <- as.data.frame(ggeffect(strength_mod, terms = "season"))
plot_s_eff_season <- ggplot(s_eff_season, aes(x, predicted))+
  geom_point(aes(x, y = predicted, col = x), size = 6)+
  geom_errorbar(aes(x, ymin = conf.low, ymax = conf.high, col = x), width = 0, linewidth = 2)+
  scale_color_manual(values = c(cc$breedingColor, cc$summerColor, cc$fallColor))+
  ylab("Strength (normalized)")+
  xlab("Season")+ 
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16),
        legend.position = "none")
plot_s_eff_season
ggsave(plot_s_eff_season, file = "fig/plot_s_eff_season.png", width = 9, height = 6)
