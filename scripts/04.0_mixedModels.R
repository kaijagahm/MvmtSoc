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

# Set ggplot theme to classic
theme_set(theme_classic())

# Load the data from the targets pipeline (wow this is so much easier!!!)
tar_load(linked)

# Examine response variable distributions ---------------------------------
normDegree_years_seasons_flight <- linked %>%
  filter(type == "flight") %>%
  mutate(season = factor(season, levels = c("fall", "breeding", "summer"))) %>%
  ggplot(aes(x = z_deg, col = season, group = seasonUnique))+
  geom_density(linewidth = 1.5)+
  theme_classic()+
  scale_color_manual(name = "Season", values = c(cc$fallColor, cc$breedingColor, cc$summerColor))+
  ylab("Frequency")+
  xlab("Degree (normalized)--z-scores")+
  facet_wrap(~season)+
  theme(legend.position = "none", text = element_text(size = 20))
normDegree_years_seasons_flight
ggsave(normDegree_years_seasons_flight, filename = "fig/normDegree_years_seasons_flight.png", width = 9, height = 7)

normDegree_years_seasons_feeding <- linked %>%
  filter(type == "feeding") %>%
  mutate(season = factor(season, levels = c("fall", "breeding", "summer"))) %>%
  ggplot(aes(x = z_deg, col = season, group = seasonUnique))+
  geom_density(linewidth = 1.5)+
  theme_classic()+
  scale_color_manual(name = "Season", values = c(cc$fallColor, cc$breedingColor, cc$summerColor))+
  ylab("Frequency")+
  xlab("Degree (normalized)--z-score")+
  facet_wrap(~season)+
  theme(legend.position = "none", text = element_text(size = 20))
ggsave(normDegree_years_seasons_feeding, filename = "fig/normDegree_years_seasons_feeding.png", width = 9, height = 7)

normDegree_years_seasons_roosting <- linked %>%
  filter(type == "roosting") %>%
  mutate(season = factor(season, levels = c("fall", "breeding", "summer"))) %>%
  ggplot(aes(x = z_deg, col = season, group = seasonUnique))+
  geom_density(linewidth = 1.5)+
  theme_classic()+
  scale_color_manual(name = "Season", values = c(cc$fallColor, cc$breedingColor, cc$summerColor))+
  ylab("Frequency")+
  xlab("Degree (normalized)--z-score")+
  facet_wrap(~season)+
  theme(legend.position = "none", text = element_text(size = 20))
ggsave(normDegree_years_seasons_roosting, filename = here("fig/normDegree_years_seasons_roosting.png"), width = 9, height = 7)

# Let's examine zeroes for the social network measures. I know that when we calculate the social networks, we had a lot of zeroes for both degree and strength. But most of the individuals that didn't have network connections probably aren't our focal individuals for the movement measures.
networkMetrics %>% filter(degree == 0 | strength == 0) # lots of rows
linked %>% filter(normDegree == 0 | normStrength == 0) # just one individual in one season!

# # XXX It is possible this will pose a problem for modeling. I wonder if it's better to remove the zero or do some kind of other transformation?
# I think I can just remove it for now
linked <- linked %>%
  filter(normDegree > 0, normStrength > 0)

# Modeling ----------------------------------------------------------------
# Degree ------------------------------------------------------------------
dat <- linked %>% filter(!is.na(z_deg), !is.infinite(z_deg))
degree_base <- glmmTMB(z_deg ~ situ + movement + roost_div + space_use + age_group + season + centr + (1|seasonUnique)+(1|Nili_id), data = dat, family = gaussian())
#check_model(degree_base) # 
check_predictions(degree_base) # this is beautiful!
simulationOutput <- DHARMa::simulateResiduals(degree_base)
plot(simulationOutput, pch=".") # oh wow, also beautiful. Waaaaay better than anything previous.
plotQQunif(simulationOutput)
plotResiduals(simulationOutput)

# What if there are different relationships between movement and degree in different seasons and situations?
degree_full <- glmmTMB(z_deg ~ situ*movement + situ*roost_div + situ*space_use + season*movement + season*roost_div + season*space_use + age_group + centr*situ + (1|seasonUnique)+(1|Nili_id), data = dat, family = gaussian()) # this will almost certainly have very high VIFs
#check_model(degree_full) # now we have some high VIFs
check_collinearity(degree_full) # the highest collinearity interaction is roost_div:season. Let's remove it, making sure that both roost_div and season stay in the model in some other capacity.
simulationOutput <- DHARMa::simulateResiduals(degree_full)
plot(simulationOutput, pch=".") # kinda gross but not terrible, especially compared to the old models.

degree_1 <- glmmTMB(z_deg ~ situ*movement + situ*roost_div + situ*space_use + season*movement + season*space_use + age_group + centr*situ + (1|seasonUnique)+(1|Nili_id), data = dat, family = gaussian())
check_collinearity(degree_1) # a couple more with moderate correlations that we should get rid of: situ:roost_div and situ:space_use. Starting with getting rid of situ:roost_div, adding roost_div back in as a main effect

degree_2 <- glmmTMB(z_deg ~ situ*movement + situ*space_use + season*movement + roost_div + season*space_use + age_group + centr*situ + (1|seasonUnique)+(1|Nili_id), data = dat, family = gaussian())
check_collinearity(degree_2) # some of these now still have high vifs... let's see about significance of effects before removing main effects.
summary(degree_2) # so neither space_use*season nor movement*season are significant. Let's get rid of both of them.

degree_3 <- glmmTMB(z_deg ~ situ*movement + situ*space_use + roost_div + season + age_group + centr*situ + (1|seasonUnique)+(1|Nili_id), data = dat, family = gaussian())
check_collinearity(degree_3) # these are all below 5 now, which is good, but I'm still skeptical of situ*space_use.
summary(degree_3) # Hmm, though, looks like situ*space_use is highly significant, while situ*movement isn't doing much for us. Let's remove that.

degree_4 <- glmmTMB(z_deg ~ movement + situ*space_use + roost_div + season + age_group + centr*situ + (1|seasonUnique)+(1|Nili_id), data = dat, family = gaussian())
check_collinearity(degree_4) # all below 4, nice.
summary(degree_4) # Remaining interactions are significant.

degree_mod <- degree_4

# Degree plots ------------------------------------------------------------
# Interaction plots for degree model
## centr (main) -----------------------------------------------------------
d_eff_centr <- as.data.frame(ggeffect(degree_mod, terms = c("centr")))
plot_d_eff_centr <- ggplot(d_eff_centr, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = centr, y = z_deg, col = situ), alpha = 0.5)+
  geom_line(linewidth = 1, col = "black")+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Degree (normalized)--z-score")+
  xlab("Spatial centrality")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_d_eff_centr
ggsave(plot_d_eff_centr, file = here("fig/mmPlots/plot_d_eff_centr.png"), width = 7, height = 6)

## centr:situ -------------------------------------------------------------
d_eff_situ_centr <- as.data.frame(ggeffect(degree_mod, terms = c("centr", "situ")))
plot_d_eff_situ_centr <- ggplot(d_eff_situ_centr, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = centr, y = z_deg, col = situ), alpha = 0.5)+
  geom_line(aes(col = group), linewidth = 1)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  scale_fill_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Degree (normalized)--z-score")+
  xlab("Spatial centrality")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_d_eff_situ_centr
ggsave(plot_d_eff_situ_centr, file = here("fig/mmPlots/plot_d_eff_situ_centr.png"), width = 7, height = 6)
d_emt_situ_centr <- emmeans::emtrends(degree_mod, "situ", var = "centr") %>%
  as.data.frame() %>%
  mutate(situ = case_when(situ == "Ro" ~ "Roosting",
                          situ == "Fl" ~ "Flight",
                          situ == "Fe" ~ "Feeding"))
d_emt_situ_centr 
plot_d_emt_situ_centr <- d_emt_situ_centr %>%
  as.data.frame() %>%
  ggplot(aes(x = situ, y = centr.trend, col = situ))+
  geom_point(size = 6)+
  geom_errorbar(aes(x = situ, ymin = lower.CL, ymax = upper.CL), 
                width = 0, linewidth = 2)+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  theme(text = element_text(size = 20), legend.position = "none")+
  ylab("centr effect")+
  xlab("Situation")+
  coord_flip()
plot_d_emt_situ_centr
ggsave(plot_d_emt_situ_centr, file = here("fig/mmPlots/plot_d_emt_situ_centr.png"), width = 5, height = 6)
 #XXX start here

## movement (main) ---------------------------------------------------------
d_eff_movement <- as.data.frame(ggeffect(degree_mod, terms = c("movement")))
plot_d_eff_movement <- ggplot(d_eff_movement, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = movement, y = z_deg, col = situ), alpha = 0.5)+
  geom_line(linewidth = 1, col = "black")+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Degree (normalized)--z-score")+
  xlab("Movement")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_d_eff_movement
ggsave(plot_d_eff_movement, file = here("fig/mmPlots/plot_d_eff_movement.png"), width = 7, height = 6)

## movement:situ -----------------------------------------------------------
d_eff_situ_movement <- as.data.frame(ggeffect(degree_mod, terms = c("movement", "situ")))
plot_d_eff_situ_movement <- ggplot(d_eff_situ_movement, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = movement, y = z_deg, col = situ), alpha = 0.5)+
  geom_line(aes(col = group), linewidth = 1)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  scale_fill_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Degree (normalized)--z-score")+
  xlab("Movement")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_d_eff_situ_movement
ggsave(plot_d_eff_situ_movement, file = here("fig/mmPlots/plot_d_eff_situ_movement.png"), width = 7, height = 6)
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
plot_d_emt_situ_movement
ggsave(plot_d_emt_situ_movement, file = here("fig/mmPlots/plot_d_emt_situ_movement.png"), width = 5, height = 6)

## movement:season ---------------------------------------------------------
d_eff_season_movement <- as.data.frame(ggeffect(degree_mod, terms = c("movement", "season")))
plot_d_eff_season_movement <- ggplot(d_eff_season_movement, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = movement, y = normDegree, col = season), alpha = 0.5)+
  geom_line(aes(col = group), linewidth = 1)+
  scale_color_manual(name = "Season", values = c(cc$breedingColor, cc$summerColor, cc$fallColor))+
  scale_fill_manual(name = "Season", values = c(cc$breedingColor, cc$summerColor, cc$fallColor))+
  ylab("Degree (normalized)")+
  xlab("Movement")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_d_eff_season_movement
ggsave(plot_d_eff_season_movement, file = here("fig/mmPlots/plot_d_eff_season_movement.png"), width = 7, height = 6)

d_emt_season_movement <- emmeans::emtrends(degree_mod, "season", var = "movement") %>%
  as.data.frame()
d_emt_season_movement 
plot_d_emt_season_movement <- d_emt_season_movement %>%
  as.data.frame() %>%
  ggplot(aes(x = season, y = movement.trend, col = season))+
  geom_point(size = 6)+
  geom_errorbar(aes(x = season, ymin = lower.CL, ymax = upper.CL), 
                width = 0, linewidth = 2)+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  scale_color_manual(name = "Season", values = c(cc$breedingColor, cc$summerColor, cc$fallColor))+
  theme(text = element_text(size = 20), legend.position = "none")+
  ylab("Movement effect")+
  xlab("Season")+
  coord_flip()
plot_d_emt_season_movement
ggsave(plot_d_emt_season_movement, file = here("fig/mmPlots/plot_d_emt_season_movement.png"), width = 5, height = 6)

d_emt_situ_movement_season <- emmeans::emtrends(degree_mod, c("situ", "season"), var = "movement") %>%
  as.data.frame()
plot_d_emt_situ_movement_season <- d_emt_situ_movement_season %>%
  as.data.frame() %>%
  ggplot(aes(x = season, y = movement.trend, col = situ, group = interaction(situ, season)))+
  geom_point(size = 6, position = position_dodge(width = 0.3))+
  geom_errorbar(aes(x = season, ymin = lower.CL, ymax = upper.CL), 
                width = 0, linewidth = 2, position = position_dodge(width = 0.3))+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  theme(text = element_text(size = 20))+
  ylab("Movement effect")+
  xlab("Season")+
  coord_flip()
ggsave(plot_d_emt_situ_movement_season, file = here("fig/mmPlots/plot_d_emt_situ_movement_season.png"), width = 5, height = 6)

## space_use (main) --------------------------------------------------------
d_eff_space <- as.data.frame(ggeffect(degree_mod, terms = c("space_use")))
plot_d_eff_space <- ggplot(d_eff_space, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = space_use, y = normDegree, col = situ), alpha = 0.5)+
  geom_line(linewidth = 1, col = "black", linetype = 2)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Degree (normalized)")+
  xlab("Space use (log-transformed)")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_d_eff_space
ggsave(plot_d_eff_space, file = here("fig/mmPlots/plot_d_eff_space.png"), width = 7, height = 6)

## space_use:situ ----------------------------------------------------------
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
ggsave(plot_d_eff_situ_space, file = here("fig/mmPlots/plot_d_eff_situ_space.png"), width = 7, height = 6)

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
plot_d_emt_situ_space
ggsave(plot_d_emt_situ_space, file = here("fig/mmPlots/plot_d_emt_situ_space.png"), width = 5, height = 6)

## roost_div (main) --------------------------------------------------------
d_eff_roost <- as.data.frame(ggeffect(degree_mod, terms = "roost_div"))
plot_d_eff_roost <- ggplot(d_eff_roost, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = roost_div, y = normDegree, col = situ), alpha = 0.5)+
  geom_line(linewidth = 1, linetype = 2)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Degree (normalized)")+
  xlab("Roost diversification")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_d_eff_roost
ggsave(plot_d_eff_roost, file = here("fig/mmPlots/plot_d_eff_roost.png"), width = 7, height = 6)

#@@@@@@@@@@@@@@@@@@@@@

# Strength ----------------------------------------------------------------
# strength_base <- glmmTMB(normStrength ~ situ + movement + roost_div + space_use + age_group + season + centr + (1|seasonUnique)+(1|Nili_id), data = linked, family = beta_family())
# check_model(strength_base) # this actually looks pretty good, shockingly! And I only had to exclude one individual for having a 0 for strength.
# simulationOutput <- DHARMa::simulateResiduals(strength_base)
# plot(simulationOutput, pch=".") # this is kinda bad but still better than the degree model!
# 
# # Let's see if we want any interactions
# strength_full <- glmmTMB(normStrength ~ situ*movement + situ*roost_div + situ*space_use + season*movement + season*roost_div + season*space_use + age_group + centr*situ + (1|seasonUnique)+(1|Nili_id), data = linked, family = beta_family())
# check_model(strength_full) # now we have some high VIFs
# check_collinearity(strength_full) # highest vif is roost_div:season
# 
# strength_1 <- glmmTMB(normStrength ~ situ*movement + situ*roost_div + situ*space_use + season*movement + season*space_use + age_group + centr*situ + (1|seasonUnique)+(1|Nili_id), data = linked, family = beta_family())
# check_collinearity(strength_1) #wow, that lowered the other VIFs a lot too! Let's now remove situ*roost_div. Have to remember to introduce a main effect term for roost_div so it doesn't go away.
# 
# strength_2 <- glmmTMB(normStrength ~ situ*movement + situ*space_use + season*movement + season*space_use + roost_div + age_group + centr*situ + (1|seasonUnique)+(1|Nili_id), data = linked, family = beta_family())
# check_collinearity(strength_2) # now these are all under 5 and most are under 4
# summary(strength_2)# buuuut we see that a lot of our remaining interactions are not significant. Start by removing situ*centr
# 
# strength_3 <- glmmTMB(normStrength ~ situ*movement + situ*space_use + season*movement + season*space_use + roost_div + age_group + centr + (1|seasonUnique)+(1|Nili_id), data = linked, family = beta_family())
# summary(strength_3) # next remove space use*season
# 
# strength_4 <- glmmTMB(normStrength ~ situ*movement + situ*space_use + season*movement +  roost_div + age_group + centr + (1|seasonUnique)+(1|Nili_id), data = linked, family = beta_family())
# summary(strength_4) # now movement*season is not significant
# 
# strength_5 <- glmmTMB(normStrength ~ situ*movement + situ*space_use + season + roost_div + age_group + centr + (1|seasonUnique)+(1|Nili_id), data = linked, family = beta_family())
# summary(strength_5) # can remove situ*movement too

strength_6 <- glmmTMB(normStrength ~ situ*space_use + season + roost_div + movement + age_group + centr + (1|seasonUnique)+(1|Nili_id), data = linked, family = beta_family())
summary(strength_6) # we have non-significant trends for movement, season, and space use, but I'm not removing those because they're main effects. 
check_model(strength_6) 

strength_mod <- strength_6

# Strength plots ----------------------------------------------------------
## space_use (main) --------------------------------------------------------
s_eff_space <- as.data.frame(ggeffect(strength_mod, terms = c("space_use")))
plot_s_eff_space <- ggplot(s_eff_space, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = space_use, y = normStrength, col = situ), alpha = 0.5)+
  geom_line(linewidth = 1, col = "black", linetype = 2)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Strength (normalized)")+
  xlab("Space use (log-transformed)")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_s_eff_space
ggsave(plot_s_eff_space, file = here("fig/mmPlots/plot_s_eff_space.png"), width = 7, height = 6)
## space_use:situ ----------------------------------------------------------
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
ggsave(plot_s_eff_situ_space, file = here("fig/mmPlots/plot_s_eff_situ_space.png"), width = 7, height = 6)

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
ggsave(plot_s_emt_situ_space, file = here("fig/mmPlots/plot_s_emt_situ_space.png"), width = 5, height = 6)

## centr (main) -----------------------------------------------------------
s_eff_centr <- as.data.frame(ggeffect(strength_mod, terms = "centr"))
plot_s_eff_centr <- ggplot(s_eff_centr, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = centr, y = normStrength, col = situ), alpha = 0.5)+
  geom_line(linewidth = 1)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Strength (normalized)")+
  xlab("Spatial centrality")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_s_eff_centr
ggsave(plot_s_eff_centr, file = here("fig/mmPlots/plot_s_eff_centr.png"), width = 7, height = 6)

## movement (main) ---------------------------------------------------------
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
ggsave(plot_s_eff_movement, file = here("fig/mmPlots/plot_s_eff_movement.png"), width = 7, height = 6)

## roost_div (main) --------------------------------------------------------
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
ggsave(plot_s_eff_roost, file = here("fig/mmPlots/plot_s_eff_roost.png"), width = 7, height = 6)

# Correlations between predictors -----------------------------------------

centr_movement <- linked %>%
  ggplot(aes(x = centr, y = movement, col = seasonUnique))+
  geom_point()+
  geom_smooth(method = "lm")+
  ylab("Movement")+
  xlab("Spatial centrality")+
  theme(axis.title = element_text(size = 16))+
  labs(color = "Season")

centr_space_use <- linked %>%
  ggplot(aes(x = centr, y = space_use, col = seasonUnique))+
  geom_point()+
  geom_smooth(method = "lm")+
  ylab("Space use")+
  xlab("Spatial centrality")+
  theme(axis.title = element_text(size = 16))+
  labs(color = "Season")

centr_roost_div <- linked %>%
  ggplot(aes(x = centr, y = roost_div, col = seasonUnique))+
  geom_point()+
  geom_smooth(method = "lm")+
  ylab("Roost diversification")+
  xlab("Spatial centrality")+
  theme(axis.title = element_text(size = 16))+
  labs(color = "Season")

ggsave(centr_movement, file = here("fig/centr_movement.png"), width = 7, height = 4)
ggsave(centr_space_use, file = here("fig/centr_space_use.png"), width = 7, height = 4)
ggsave(centr_roost_div, file = here("fig/centr_roost_div.png"), width = 7, height = 4)
