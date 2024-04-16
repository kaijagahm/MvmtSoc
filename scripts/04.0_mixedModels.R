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
theme_set(theme_classic(base_size = 6))

# Load the data from the targets pipeline (wow this is so much easier!!!)
tar_load(linked)
tar_load(cc)

# Examine response variable distributions ---------------------------------

# Normalized degree and strength ------------------------------------------
linked %>%
  ggplot(aes(x = degree, col = season, group = seasonUnique))+
  geom_density()+
  facet_wrap(~type, scales = "free") # very distinct distributions, but since we will have a random effect for seasonUnique, that should cover it... 

linked %>%
  ggplot(aes(x = strength, col = season, group = seasonUnique))+
  geom_density()+
  facet_wrap(~type, scales = "free") 

# Degree and strength z-scores (non-normalized) ---------------------------
linked %>%
  ggplot(aes(x = z_deg, col = season, group = seasonUnique))+
  geom_density()+
  facet_wrap(~type, scales = "free") # these are weird distributions. I wonder what model family we should use here...

linked %>%
  ggplot(aes(x = z_str, col = season, group = seasonUnique))+
  geom_density()+
  facet_wrap(~type, scales = "free")

# Let's examine zeroes for the social network measures. I know that when we calculate the social networks, we had a lot of zeroes for both degree and strength. But most of the individuals that didn't have network connections probably aren't our focal individuals for the movement measures.
linked %>% filter(z_deg == 0 | z_str == 0) # nobody
linked %>% filter(degree == 0, strength == 0) # just one individual in one season
linked %>% filter(is.na(z_deg) | is.na(z_str)) # just one individual in one season
linked %>% filter(is.na(z_deg) | is.na(z_str)) # just one individual in one season

# Let's remove that individual in case she poses a problem for modeling
linked <- linked %>%
  filter(!is.na(z_deg), !is.na(z_str))
nrow(linked)

linked %>% filter(is.infinite(z_deg) | is.infinite(z_str)) # likewise, removing the infinite individual
linked <- linked %>%
  filter(!is.infinite(z_deg), !is.infinite(z_str))

View(linked)
# Modeling: not corrected for space use -----------------------------------
tar_load(allMetrics)
## Degree -----------------------------------------------------------------
deg_base <- glmmTMB(normDegree ~ situ + movement + roost_div + space_use + age_group + season + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
check_predictions(deg_base) # bleh
simulationOutput <- DHARMa::simulateResiduals(deg_base)
plot(simulationOutput, pch=".") # not great but not terrible.

deg_full <- glmmTMB(normDegree ~ situ*season*movement + situ*season*roost_div + situ*season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian()) # not converging, hmmm...
simulationOutput <- DHARMa::simulateResiduals(deg_full)
plot(simulationOutput, pch=".") # not great but not terrible.
check_collinearity(deg_full) # remove situ*season*roost_div first

deg_1 <- glmmTMB(normDegree ~ situ*season*movement + situ*season + situ*roost_div + season*roost_div + situ*season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian()) # still not converging. hmm.. let's keep going, I guess?
check_collinearity(deg_1) # now remove situ*season*movement

deg_2 <- glmmTMB(normDegree ~ situ*movement + season*movement + situ*season + situ*roost_div + season*roost_div + situ*season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian()) # converges now!
check_collinearity(deg_2) # now remove situ*season*space. Phew, no 3-way interactions!

deg_3 <- glmmTMB(normDegree ~ situ*movement + season*movement + situ*season + situ*roost_div + season*roost_div + situ*space_use + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_3) # now we can remove situ*season

deg_4 <- glmmTMB(normDegree ~ situ*movement + season*movement + situ*roost_div + season*roost_div + situ*space_use + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_4) # season*roost_div still has high collinearity

deg_5 <- glmmTMB(normDegree ~ situ*movement + season*movement + situ*roost_div + situ*space_use + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_5) # now situ*roost_div

deg_6 <- glmmTMB(normDegree ~ situ*movement + season*movement + roost_div + situ*space_use + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_6) # all collinearities are small enough now. So let's take a look at the summary
summary(deg_6) # season*space_use is clearly non-significant, so let's remove that.

deg_7 <- glmmTMB(normDegree ~ situ*movement + season*movement + roost_div + situ*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
summary(deg_7) # all other interactions are at least marginally significant.
simulationOutput <- DHARMa::simulateResiduals(deg_7)
plot(simulationOutput, pch=".") # bleh

deg_mod <- deg_7

## Strength ---------------------------------------------------------------
str_base <- glmmTMB(normStrength ~ situ + movement + roost_div + space_use + age_group + season + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
check_predictions(str_base) # not bad, actually!
simulationOutput <- DHARMa::simulateResiduals(str_base)
plot(simulationOutput, pch=".") # nice, pretty good!

str_full <- glmmTMB(normStrength ~ situ*season*movement + situ*season*roost_div + situ*season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
simulationOutput <- DHARMa::simulateResiduals(str_full)
dev.off()
plot(simulationOutput, pch=".") # oh GOD
check_collinearity(str_full) # can start by removing situ*season*roost_div

str_1 <- glmmTMB(normStrength ~ situ*season*movement + situ*season + situ*roost_div + season*roost_div + situ*season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
check_collinearity(str_1) # now we can remove situ*season*movement

str_2 <- glmmTMB(normStrength ~ situ*movement + season*movement + situ*season + situ*roost_div + season*roost_div + situ*season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
check_collinearity(str_2) # now we can remove situ*season*space_use

str_3 <- glmmTMB(normStrength ~ situ*movement + season*movement + situ*season + situ*roost_div + season*roost_div + situ*space_use + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
check_collinearity(str_3) # situ*season now has the highest collinearity

str_4 <- glmmTMB(normStrength ~ situ*movement + season*movement + situ*roost_div + season*roost_div + situ*space_use + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
check_collinearity(str_4) # situ*season now has the highest collinearity

str_5 <- glmmTMB(normStrength ~ situ*movement + season*movement + situ*roost_div + season*roost_div + situ*space_use + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
check_collinearity(str_5) # and now we remove season*roost_div

str_6 <- glmmTMB(normStrength ~ situ*movement + season*movement + situ*roost_div + situ*space_use + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
summary(str_6) # situ*roost_div is the only interaction that's significant
check_collinearity(str_6) # but/and it's also the one that's the most collinear. Hmm. Maybe we can remove situ*space first?

str_7 <- glmmTMB(normStrength ~ situ*movement + season*movement + situ*roost_div + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
check_collinearity(str_7) # nice, now everything has dropped down to low collinearity
summary(str_7) # a bunch of these are non-significant. Let's start by removing situ*movement

str_8 <- glmmTMB(normStrength ~ season*movement + situ*roost_div + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
summary(str_8) # season*space and season*movement are non-significant. Remove both.

str_9 <- glmmTMB(normStrength ~ movement + situ*roost_div + space_use + season + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
summary(str_9) # all remaining interactions are statistically significant

str_mod <- str_9

# Modeling: z-scores (corrected for space use) ----------------------------
# This time we're using non-normalized degree and strength values, since using z-scores ends up normalizing each individual against itself.

# Visualize some of the z score deviations
tar_load(allMetrics)
vultures <- sample(unique(allMetrics$Nili_id), 10)
allMetrics %>% filter(Nili_id %in% vultures) %>% ggplot(aes(x = Nili_id, y = wrapped_degree, group = interaction(situ, Nili_id), col = situ))+geom_violin()+facet_wrap(~season, scales = "free")+geom_point(aes(x = Nili_id, y = degree)) # individuals generally *do* have degrees higher than expected by chance.

## Degree ---------------------------------------------------------------
deg_z_base <- glmmTMB(z_deg ~ situ + movement + roost_div + space_use + age_group + season + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian()) 
check_predictions(deg_z_base) # ick

deg_z_full <- glmmTMB(z_deg ~ situ*season*movement + situ*season*roost_div + situ*season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian()) 
check_predictions(deg_z_full) # not any better
simulationOutput <- DHARMa::simulateResiduals(deg_z_full)
dev.off()
plot(simulationOutput, pch=".") # actually not as terrible as I'd feared
check_collinearity(deg_z_full) # first get rid of situ*season*roost_div

deg_z_1 <- glmmTMB(z_deg ~ situ*season*movement + situ*season + situ*roost_div + season*roost_div + situ*season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian()) 
check_collinearity(deg_z_1) # now remove situ*season*movement

deg_z_2 <- glmmTMB(z_deg ~ situ*movement + season*movement + situ*season + situ*roost_div + season*roost_div + situ*season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian()) 
check_collinearity(deg_z_2) # now remove situ*season*space

deg_z_3 <- glmmTMB(z_deg ~ situ*movement + season*movement + situ*season + situ*roost_div + season*roost_div + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian()) 
check_collinearity(deg_z_3) # now remove situ*season

deg_z_4 <- glmmTMB(z_deg ~ situ*movement + season*movement + situ*roost_div + season*roost_div + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian()) 
check_collinearity(deg_z_4) # now remove season*roost_div

deg_z_5 <- glmmTMB(z_deg ~ situ*movement + season*movement + situ*roost_div + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian()) 
check_collinearity(deg_z_5) # now that we're down to moderate correlations, let's see what the summary looks like
summary(deg_z_5) # situ*space and situ*roost are significant
check_collinearity(deg_z_5) # situ*roost and situ*space are the highest. Let's try removing movement*season first, I guess?

deg_z_6 <- glmmTMB(z_deg ~ situ*movement + situ*roost_div + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_z_6)
summary(deg_z_6) # situ*movement is n.s.

deg_z_7 <- glmmTMB(z_deg ~ movement + situ*roost_div + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_z_7)
summary(deg_z_7) # space*season is n.s.

deg_z_8 <- glmmTMB(z_deg ~ movement + situ*roost_div + situ*space_use + season + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_z_8) # now we have two alternatives for removal: situ*roost_div or situ*space
summary(deg_z_8) # both show up here as significant

# first removing situ*roost_div
deg_z_9a <- glmmTMB(z_deg ~ movement + roost_div + situ*space_use + season + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_z_9a) # this is fine now
summary(deg_z_9a) # situ*space is retained as a significant effect

# now removing situ*space
deg_z_9b <- glmmTMB(z_deg ~ movement + space_use + situ*roost_div + season + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_z_9b) # this is fine now
summary(deg_z_9b) # situ*roost_div is now non-significant.

# so let's go with deg_z_9a
deg_z_mod <- deg_z_9a

## Strength ---------------------------------------------------------------
str_z_base <- glmmTMB(z_str ~ situ + movement + roost_div + space_use + age_group + season + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian()) 
check_predictions(str_z_base) # oh no...
linked %>% ggplot(aes(x = z_str, col = situ, group = interaction(situ, seasonUnique)))+geom_density()

str_z_full <- glmmTMB(z_str ~ situ*season*movement + situ*season*roost_div + situ*season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian()) 
check_predictions(str_z_full) # not any better
simulationOutput <- DHARMa::simulateResiduals(str_z_full)
dev.off()
plot(simulationOutput, pch=".") # also really bad. I wonder what I should do about this...
check_collinearity(str_z_full) # first remove situ*season*roost_div

str_z_1 <- glmmTMB(z_str ~ situ*season*movement + situ*season + situ*roost_div + season*roost_div + situ*season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian()) 
check_collinearity(str_z_1) # now remove situ*season*movement

str_z_2 <- glmmTMB(z_str ~ situ*movement + season*movement + situ*season + situ*roost_div + season*roost_div + situ*season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian()) 
check_collinearity(str_z_2) # now remove situ*season*space_use

str_z_3 <- glmmTMB(z_str ~ situ*movement + season*movement + situ*season + situ*roost_div + season*roost_div + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian()) 
check_collinearity(str_z_3) # now remove situ*season

str_z_4 <- glmmTMB(z_str ~ situ*movement + season*movement + situ*roost_div + season*roost_div + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian()) 
check_collinearity(str_z_4) # now remove season*roost_div

str_z_5 <- glmmTMB(z_str ~ situ*movement + season*movement + situ*roost_div + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian()) 
check_collinearity(str_z_5) # situ*roost_div would be the next to go, but let's check the summary
summary(str_z_5) # sure, that one is totally non-significant, so let's remove it

str_z_6 <- glmmTMB(z_str ~ situ*movement + season*movement + roost_div + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian()) 
check_collinearity(str_z_6) # hmm, space_use is highly collinear, but maybe we can remove some interactions involving it instead of removing the main effect term itself.
summary(str_z_6) # season*space_use is non-significant, while situ*space_use is significant.

str_z_7 <- glmmTMB(z_str ~ situ*movement + season*movement + roost_div + situ*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian()) 
check_collinearity(str_z_7) #  all collinearities are now low. Let's remove any remaining significant effects.
summary(str_z_7) # oh man, almost nothing shows up. Get rid of situ*movement and movement*season

str_z_8 <- glmmTMB(z_str ~ movement + roost_div + situ*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian()) 
summary(str_z_8) # nice! 

str_z_mod <- str_z_8

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