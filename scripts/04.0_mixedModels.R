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

# Set ggplot theme to classic
theme_set(theme_classic())

# Load the data from the targets pipeline (wow this is so much easier!!!)
tar_load(linked)
tar_load(cc)

# Examine response variable distributions ---------------------------------
linked %>%
  ggplot(aes(x = z_deg, col = season))+
  geom_density()+
  facet_wrap(~type, scales = "free") # these are weird distributions. I wonder what model family we should use here...

linked %>%
  ggplot(aes(x = z_str, col = season))+
  geom_density()+
  facet_wrap(~type, scales = "free")

# Let's examine zeroes for the social network measures. I know that when we calculate the social networks, we had a lot of zeroes for both degree and strength. But most of the individuals that didn't have network connections probably aren't our focal individuals for the movement measures.
linked %>% filter(z_deg == 0 | z_str == 0) # nobody
linked %>% filter(is.na(z_deg) | is.na(z_str)) # just one individual in one season

# Let's remove that individual in case she poses a problem for modeling
linked <- linked %>%
  filter(!is.na(z_deg), !is.na(z_str))
nrow(linked)

linked %>% filter(is.infinite(z_deg) | is.infinite(z_str))
linked <- linked %>%
  filter(!is.infinite(z_deg), !is.infinite(z_str))

# Examine the z-score distributions ---------------------------------------
linked %>%
  select(seasonUnique, situ, z_deg, z_str) %>%
  ungroup() %>%
  pivot_longer(cols = contains("z_"), names_to = "measure", values_to = "z") %>%
  ggplot(aes(x = z, col = situ, group = interaction(seasonUnique, measure, situ)))+
  geom_density(aes(linetype = measure))+
  facet_wrap(~seasonUnique, scales = "free")+
  geom_vline(aes(xintercept = 0)) # notably, we see that the z_scores for degree are almost entirely positive, while the ones for strength are basically centered around zero, with very few exceptions.

# Modeling ----------------------------------------------------------------
# Degree ------------------------------------------------------------------
tar_load(allMetrics)
vultures <- sample(unique(allMetrics$Nili_id), 10)
allMetrics %>% filter(Nili_id %in% vultures) %>% ggplot(aes(x = Nili_id, y = wrapped_degree, group = interaction(situ, Nili_id), col = situ))+geom_violin()+facet_wrap(~season, scales = "free")+geom_point(aes(x = Nili_id, y = degree)) # individuals generally *do* have degrees higher than expected by chance.

degree_base <- glmmTMB(z_deg ~ situ + movement + roost_div + space_use + age_group + season + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian()) 
check_predictions(degree_base)
simulationOutput <- DHARMa::simulateResiduals(degree_base)
plot(simulationOutput, pch=".")
plotQQunif(simulationOutput)
plotResiduals(simulationOutput)

# What if there are different relationships between movement and degree in different seasons and situations?
degree_full <- glmmTMB(z_deg ~ situ*movement*season + situ*roost_div*season + situ*space_use*season + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian()) # this will almost certainly have very high VIFs
check_collinearity(degree_full) # YIKES. Okay, that was predictable. Let's get rid of situ:season:roost_div, situ:movement:season, and situ:season:space_use for starters.
simulationOutput <- DHARMa::simulateResiduals(degree_full)
plot(simulationOutput, pch=".") # kinda gross but not terrible, especially compared to the old models

degree_1 <- glmmTMB(z_deg ~ situ*movement + situ*season + movement*season + situ*roost_div + roost_div*season + situ*space_use + space_use*season + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
summary(degree_1)
check_collinearity(degree_1) # getting better. Next we can remove situ*season.

degree_2 <- glmmTMB(z_deg ~ situ*movement + movement*season + situ*roost_div + roost_div*season + situ*space_use + space_use*season + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
summary(degree_2)
check_collinearity(degree_2) # and now we can remove season*roost_div

degree_3 <- glmmTMB(z_deg ~ situ*movement + movement*season + situ*roost_div + situ*space_use + space_use*season + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
summary(degree_3) # season*space, movement*season, and situ*movement are all non-significant.
check_collinearity(degree_3) # the ones with the highest collinearity have some significant effects. Maybe we can reduce the collinearity by removing non-significant ones instead? Let's start by removing season*space

degree_4 <- glmmTMB(z_deg ~ situ*movement + movement*season + situ*roost_div + situ*space_use + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
summary(degree_4) #
check_collinearity(degree_4) # situ*space has high collinearity but also a significant effect. Maybe we could remove situ*movement instead...

degree_5 <- glmmTMB(z_deg ~ movement*season + situ*roost_div + situ*space_use + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
summary(degree_5) #
check_collinearity(degree_5) # it's not going to help, but we can remove movement*season last...

degree_6 <- glmmTMB(z_deg ~ movement + season + situ*roost_div + situ*space_use + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
summary(degree_6) #
check_collinearity(degree_6) # these VIFs of 6.52 and 6.65 for situ*roost and situ*space are kind of borderline. One thing we could do is make two models and see if they agree, each removing one of the terms?

degree_7a <- glmmTMB(z_deg ~ movement + season + situ*roost_div + space_use + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
summary(degree_7a) # lo and behold, the other is now not significant at all...
check_collinearity(degree_7a) # now all vifs are low

degree_7b <- glmmTMB(z_deg ~ movement + season + situ*space_use + roost_div + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
summary(degree_7b) # ooh, situ*space is still highly significant when we remove situ*roost
check_collinearity(degree_7b) # and all the vifs are low.

# I think based on this we should go with 7b!

degree_mod <- degree_7b

# Degree plots ------------------------------------------------------------

## movement (main) ---------------------------------------------------------
d_eff_movement <- as.data.frame(ggeffect(degree_mod, terms = c("movement")))
plot_d_eff_movement <- ggplot(d_eff_movement, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = movement, y = z_deg, col = situ), alpha = 0.5)+
  geom_line(linewidth = 1, col = "black", linetype = 2)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Degree deviation")+
  xlab("Movement")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_d_eff_movement
#ggsave(plot_d_eff_movement, file = here("fig/mmPlots/plot_d_eff_movement.png"), width = 7, height = 6)

## space_use (main) --------------------------------------------------------
d_eff_space <- as.data.frame(ggeffect(degree_mod, terms = c("space_use")))
plot_d_eff_space <- ggplot(d_eff_space, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = space_use, y = z_deg, col = situ), alpha = 0.5)+
  geom_line(linewidth = 1, col = "black", linetype = 2)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Degree deviation")+
  xlab("Space use (log-transformed)")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_d_eff_space
#ggsave(plot_d_eff_space, file = here("fig/mmPlots/plot_d_eff_space.png"), width = 7, height = 6)

## space_use:situ ----------------------------------------------------------
d_eff_situ_space <- as.data.frame(ggeffect(degree_mod, terms = c("space_use", "situ")))
plot_d_eff_situ_space <- ggplot(d_eff_situ_space, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = space_use, y = z_deg, col = situ), alpha = 0.5)+
  geom_line(aes(col = group), linewidth = 1)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  scale_fill_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Degree deviation")+
  xlab("Space use (log-transformed)")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_d_eff_situ_space
#ggsave(plot_d_eff_situ_space, file = here("fig/mmPlots/plot_d_eff_situ_space.png"), width = 7, height = 6)

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
  ylab("Space use effect (degree deviation)")+
  xlab("Situation")+
  coord_flip()
plot_d_emt_situ_space
#ggsave(plot_d_emt_situ_space, file = here("fig/mmPlots/plot_d_emt_situ_space.png"), width = 5, height = 6)

## roost_div (main) --------------------------------------------------------
d_eff_roost <- as.data.frame(ggeffect(degree_mod, terms = "roost_div"))
plot_d_eff_roost <- ggplot(d_eff_roost, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = roost_div, y = z_deg, col = situ), alpha = 0.5)+
  geom_line(linewidth = 1)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Degree deviation")+
  xlab("Roost diversification")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_d_eff_roost
#ggsave(plot_d_eff_roost, file = here("fig/mmPlots/plot_d_eff_roost.png"), width = 7, height = 6)

#@@@@@@@@@@@@@@@@@@@@@

# Strength ----------------------------------------------------------------
tar_load(allMetrics)
vultures <- sample(unique(allMetrics$Nili_id), 10)
allMetrics %>% filter(Nili_id %in% vultures) %>% ggplot(aes(x = Nili_id, y = wrapped_strength, group = interaction(situ, Nili_id), col = situ))+geom_violin()+facet_wrap(~season, scales = "free")+geom_point(aes(x = Nili_id, y = strength)) # individuals don't really seem to have strengths higher than expected by chance!

strength_base <- glmmTMB(z_str ~ situ + movement + roost_div + space_use + age_group + season + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
check_predictions(strength_base) # oof, that looks bad...
simulationOutput <- DHARMa::simulateResiduals(strength_base)
plot(simulationOutput, pch=".")
plotQQunif(simulationOutput)
plotResiduals(simulationOutput) # these look very bad, honestly...

# What if there are different relationships between movement and degree in different seasons and situations?
strength_full <- glmmTMB(z_str ~ situ*movement*season + situ*roost_div*season + situ*space_use*season + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian()) # this will almost certainly have very high VIFs
check_collinearity(strength_full) # YIKES. Let's get rid of situ*movement*season and situ*season*roost_div for starters.

strength_1 <- glmmTMB(z_str ~ situ*movement + movement*season + situ*roost_div + situ*season + roost_div*season + situ*space_use*season + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
check_collinearity(strength_1) # remove situ*season*space now

strength_2 <- glmmTMB(z_str ~ situ*movement + movement*season + situ*roost_div + roost_div*season + situ*space_use + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
check_collinearity(strength_2) # now we can remove movement*season, assuming it's not highly significant
summary(strength_2) # lol nope def n.s.

strength_3 <- glmmTMB(z_str ~ situ*movement + situ*roost_div + roost_div*season + situ*space_use + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
check_collinearity(strength_3) # highest collinearity with situ*roost
summary(strength_3) # but it's significant. Let's remove situ*movement instead, and/or roost*season

strength_4 <- glmmTMB(z_str ~ movement + situ*roost_div + roost_div*season + situ*space_use + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
check_collinearity(strength_4)
summary(strength_4) # can remove roost*season

strength_5 <- glmmTMB(z_str ~ movement + situ*roost_div + season + situ*space_use + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
check_collinearity(strength_5) # once again, we're down to situ*roost and situ*space_use. Let's do the split thing again...
summary(strength_5)

strength_6a <- glmmTMB(z_str ~ movement + situ*roost_div + season + space_use + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian()) 
check_collinearity(strength_6a) # this is all better
summary(strength_6a) # highly significant interaction between situ and roost_div

# strength_6b <- glmmTMB(z_str ~ movement + situ*space_use + season + roost_div + age_group + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian()) 
# check_collinearity(strength_6b) # likewise, much better
# summary(strength_6b) # highly significant interaction between situ and roost_div
# 
# compare_performance(strength_6a, strength_6b) # these are basically the same in terms of fit, too.

check_predictions(strength_6a) # this still looks really bad
simulationOutput <- DHARMa::simulateResiduals(strength_6a)
plot(simulationOutput, pch=".")
plotQQunif(simulationOutput)
plotResiduals(simulationOutput) # these look very bad, honestly...
# I really don't like the fit of either of these models. Maybe the distribution is too tight for a gaussian?

strength_mod <- strength_6a # choosing this arbitrarily; I'm not really happy with either one.

# Strength plots ----------------------------------------------------------
## space_use (main) --------------------------------------------------------
s_eff_space <- as.data.frame(ggeffect(strength_mod, terms = c("space_use")))
plot_s_eff_space <- ggplot(s_eff_space, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = space_use, y = z_str, col = situ), alpha = 0.5)+
  geom_line(linewidth = 1, col = "black", linetype = 2)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Strength (normalized)")+
  xlab("Space use (log-transformed)")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_s_eff_space
#ggsave(plot_s_eff_space, file = here("fig/mmPlots/plot_s_eff_space.png"), width = 7, height = 6)
## space_use:situ ----------------------------------------------------------
s_eff_situ_space <- as.data.frame(ggeffect(strength_mod, terms = c("space_use", "situ")))
plot_s_eff_situ_space <- ggplot(s_eff_situ_space, aes(x, predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_point(data = linked, aes(x = space_use, y = z_str, col = situ), alpha = 0.5)+
  geom_line(aes(col = group), linewidth = 1)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  scale_fill_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  ylab("Strength (normalized)")+
  xlab("Space use (log-transformed)")+
  ggtitle("")+theme_classic()+
  theme(text = element_text(size = 16))
plot_s_eff_situ_space
#ggsave(plot_s_eff_situ_space, file = here("fig/mmPlots/plot_s_eff_situ_space.png"), width = 7, height = 6)

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
  geom_errorbar(aes(x = situ, ymin = lower.CL, ymax = upper.CL), 
                width = 0, linewidth = 2)+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  theme(text = element_text(size = 20), legend.position = "none")+
  ylab("Space use effect")+
  xlab("Situation")+
  coord_flip()
plot_s_emt_situ_space
#ggsave(plot_s_emt_situ_space, file = here("fig/mmPlots/plot_s_emt_situ_space.png"), width = 5, height = 6)