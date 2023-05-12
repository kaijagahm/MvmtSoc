library(car) # for p-values in mixed models?
library(tidyverse)
library(sf)
library(factoextra)
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

# Here's an interesting reference article for some modeling stuff: https://biol607.github.io/lab/12_gzlm.html

load("data/linked.Rda")
load("data/mnPPD.Rda")
mnPPD <- mnPPD %>%
  mutate(year = factor(as.numeric(str_extract(seasonUnique, "[0-9]+"))),
         season = factor(as.character(str_extract(seasonUnique, "[a-z]+")), levels = c("breeding", "summer", "fall")))
linked <- linked %>%
  left_join(mnPPD)

linked <- linked %>%
  mutate(season = factor(season, levels = c("breeding", "summer", "fall")))

# Remove individuals that don't have movement information
colSums(is.na(linked)) # lots of NA's
linked <- linked %>%
  filter(!is.na(PC1)) # note that because I joined the sex/age information along with the movement information (instead of along with the social information), if I ever want to *not* filter out those individuals, I'll have to go back and add sex/age info for the additional birds. Don't need this for now, so I'm skipping it. 
colSums(is.na(linked)) # okay now we have no more NA's except in sex. Yay!

# Examine covariates ------------------------------------------------------
# SEX
## differences in PC values by sex
long <- linked %>%
  filter(sex %in% c("f", "m")) %>%
  dplyr::select(Nili_id, season, age, age_group, sex, PC1, PC2, PC3) %>%
  pivot_longer(cols = c("PC1", "PC2", "PC3"), values_to = "value", 
               names_to = "PC")
stat.test <- long %>%
  group_by(PC) %>%
  t_test(value ~ sex) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance() %>% 
  add_xy_position(x = "sex")

bxp <- ggboxplot(
  long, x = "sex", y = "value", 
  fill = "sex", 
  palette = c("orange", "skyblue3"),
  facet.by = "PC"
) + stat_pvalue_manual(stat.test)
bxp

linked %>%
  dplyr::select(sex, type, Nili_id, degreeRelative, strengthRelative, sbd) %>%
  pivot_longer(cols = c("degreeRelative", "strengthRelative", "sbd"), 
               names_to = "socialPositionMeasure", values_to = "value") %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = sex, y = value, fill = sex))+
  geom_boxplot()+
  theme_minimal()+
  scale_fill_manual(values = c("orange", "skyblue3"))+
  theme(legend.position = "none")+
  facet_wrap(facets = vars(type, socialPositionMeasure), 
             nrow = 3, ncol = 3, scales= "free")+
  ylab("Value")+
  xlab("Sex")

# AGE
## differences in PC values by age
long %>%
  ggplot(aes(x = age, y = value, col = PC))+
  geom_point(size = 1.5, alpha = 0.7)+
  geom_smooth(method = "lm")+
  theme_classic()+
  scale_color_viridis_d(option = "D")+
  theme(text = element_text(size = 18))+
  ylab("Value")+
  xlab("Age")

stat.test_age <- long %>%
  group_by(PC) %>%
  t_test(value ~ age_group) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance() %>% 
  add_xy_position(x = "age_group")

bxp_age <- ggboxplot(
  long, x = "age_group", y = "value", 
  fill = "age_group", 
  palette = c("red", "red4"),
  facet.by = "PC"
) + stat_pvalue_manual(stat.test_age)
bxp_age

## differences in degree/strength by age 
head(linked)
linked %>%
  dplyr::select(age_group, type, Nili_id, degreeRelative, strengthRelative, sbd) %>%
  pivot_longer(cols = c("degreeRelative", "strengthRelative", "sbd"), names_to = "socialPositionMeasure", values_to = "value") %>%
  filter(!is.na(age_group)) %>%
  ggplot(aes(x = age_group, y = value, fill = age_group))+
  geom_boxplot()+
  theme_minimal()+
  scale_fill_manual(values = c("red", "red4"))+
  theme(legend.position = "none")+
  facet_wrap(facets = vars(type, socialPositionMeasure), 
             nrow = 3, ncol = 3, scales= "free")+
  ylab("Value")+
  xlab("Age group")

# SEASON
linked %>%
  dplyr::select(type, Nili_id, degreeRelative, strengthRelative, sbd, season) %>%
  pivot_longer(cols = c("degreeRelative", "strengthRelative", "sbd"), names_to = "socialPositionMeasure", values_to = "value") %>%
  filter(!is.na(season)) %>%
  ggplot(aes(x = season, y = value, fill = season))+
  geom_boxplot()+
  theme_minimal()+
  scale_fill_manual(values = c("blue", "red", "darkorange"))+
  theme(legend.position = "none")+
  facet_wrap(facets = vars(type, socialPositionMeasure), 
             nrow = 3, ncol = 3, scales= "free")+
  ylab("Value")+
  xlab("Season") 

## differences in PC values by season
long <- linked %>%
  dplyr::select(Nili_id, season, age, age_group, sex, PC1, PC2, PC3) %>%
  pivot_longer(cols = c("PC1", "PC2", "PC3"), values_to = "value", 
               names_to = "PC")
stat.test <- long %>%
  group_by(PC) %>%
  t_test(value ~ season) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance() %>% 
  add_xy_position(x = "season")

bxp <- ggboxplot(
  long, x = "season", y = "value", 
  fill = "season", 
  palette = c("blue", "red", "darkorange"),
  facet.by = "PC"
) + stat_pvalue_manual(stat.test)
bxp # we definitely have seasonal differences in movement! As expected.

# Mixed models -------------------------------------------------------------------
# Starting with CO-FLIGHT

# Response variables:
# Relative degree
# Relative strength
# Strength by degree

# Predictors:
# PC1 (continuous)
# PC2 (continuous)
# age (cat: adult/juv_sub)
# sex (cat: m/f)
# season (cat: b/nb)
# year (cat)
# Nili_id (random effect) (1|Nili_id)
flight <- linked %>%
  filter(type == "flight")
feeding <- linked %>%
  filter(type == "feeding")
roosting <- linked %>%
  filter(type == "roosting")

# FLIGHT: RELATIVE DEGREE
# I thought we couldn't include random effects if they had fewer than 5 levels, but maybe you can? https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8784019/
# First, fit a model that includes all the predictors but no interaction effects, to check the VIF, which will tell us whether the predictors are correlated within the model.
fd_noint <- glmmTMB(degreeRelative ~ PC1 + PC2 + age_group + season + year + (1|Nili_id), 
                    data = flight, family = beta_family()) # link function transforms your variable
#vif(fd_noint) # can't calculate vif on beta-distribution models i guess?
summary(fd_noint) # age group comes out the least significant.
# use DHARMa to check the model
so_fd_noint <- simulateResiduals(fittedModel = fd_noint, plot = F)
plot(so_fd_noint) # okay this looks pretty bad, actually. What do we do about that?
testDispersion(so_fd_noint)
plotResiduals(so_fd_noint, form = flight$season)
plotResiduals(so_fd_noint, form = flight$PC1)
plotResiduals(so_fd_noint, form = flight$PC2)
plotResiduals(so_fd_noint, form = flight$age_group)
plotResiduals(so_fd_noint, form = flight$year)
# okay literally *ALL* of these are bad. What do I do??
# Seems like the model is underdispersed, based on comparing it to the DHARMa examples. This would suggest that the model is too complicated... Let's begin removing things.
testDispersion(so_fd_noint) # looks like it's underdispersed?

fd_max <- glmmTMB(degreeRelative ~ PC1*age_group*season + PC2*age_group*season + year + (1|Nili_id), family = beta_family(), data = flight)
so_fd_max <- simulateResiduals(fittedModel = fd_max, plot = F)
plot(so_fd_max)
summary(fd_max) # almost nothing here is significant. Can start by removing the three-way interaction with PC1.

plotResiduals(so_fd_max, form = flight$PC1)
plotResiduals(so_fd_max, form = flight$PC2)
plotResiduals(so_fd_max, form = flight$age_group)
plotResiduals(so_fd_max, form = flight$year)

fd_2 <- glmmTMB(degreeRelative ~ PC1*age_group + PC1*season + PC2*age_group*season + year + (1|Nili_id), family = beta_family(), data = flight)
so_fd_2 <- simulateResiduals(fittedModel = fd_2, plot = F)
plot(so_fd_2)
summary(fd_2) # interactions between PC1 and age and season are not significant

fd_3 <- glmmTMB(degreeRelative ~ PC1 + PC1*season + PC2*age_group*season + year + (1|Nili_id), family = beta_family(), data = flight)
so_fd_3 <- simulateResiduals(fittedModel = fd_3, plot = F)
plot(so_fd_3) # yuck
summary(fd_3) # yep, can still remove PC1*season

fd_4 <- glmmTMB(degreeRelative ~ PC1 + PC2*age_group*season + year + (1|Nili_id), family = beta_family(), data = flight)
so_fd_4 <- simulateResiduals(fittedModel = fd_4, plot = F)
plot(so_fd_4)
summary(fd_4) # okay, not loving the age group *season effect

fd_5 <- glmmTMB(degreeRelative ~ PC1 + PC2*age_group + PC2*season + year + (1|Nili_id), family = beta_family(), data = flight)
so_fd_5 <- simulateResiduals(fittedModel = fd_5, plot = F)
plot(so_fd_5) # this fit is still really really bad!
summary(fd_5) # now none of the interactions with PC2 are any good. Let's remove one at at time.

fd_6 <- glmmTMB(degreeRelative ~ PC1 + PC2*age_group + season + year + (1|Nili_id), family = beta_family(), data = flight)
so_fd_6 <- simulateResiduals(fittedModel = fd_6, plot = F)
plot(so_fd_6) 
summary(fd_6)

fd_7 <- glmmTMB(degreeRelative ~ PC1 + PC2*season + age_group + year + (1|Nili_id), family = beta_family(), data = flight)
so_fd_7 <- simulateResiduals(fittedModel = fd_7, plot = F)
plot(so_fd_7) 
summary(fd_7)

# the next step would be to remove both interactions, and that would get us back to the fd_noint model. For good measure, going to include two models with the PC1 interactions but without PC2 interactions. Expect these both to be bad.

fd_8 <- glmmTMB(degreeRelative ~ PC2 + PC1*age_group + season + year + (1|Nili_id), family = beta_family(), data = flight)
so_fd_8 <- simulateResiduals(fittedModel = fd_8, plot = F)
plot(so_fd_8) 
summary(fd_8) # yeah, non-signif and it didn't improve the fit.

fd_9 <- glmmTMB(degreeRelative ~ PC2 + PC1*season + age_group + year + (1|Nili_id), family = beta_family(), data = flight)
so_fd_9 <- simulateResiduals(fittedModel = fd_9, plot = F)
plot(so_fd_9) 
summary(fd_9) # likewise, non-signif, and didn't improve the fit. Ah well.

# Now we move on to removing main effects.
summary(fd_noint) # interestingly, age group is the first to go.

fd_10 <- glmmTMB(degreeRelative ~ PC1 + PC2 + season + year + (1 | Nili_id), family = beta_family(), data = flight)
so_fd_10 <- simulateResiduals(fittedModel = fd_10, plot = F)
plot(so_fd_10) # the fit still looks bad. What is going on??
summary(fd_10) # everything is significant now; I don't really want to remove anything else.

# It's not a good thing that year is significant... really would like that to be a random effect. Arghh... 

# Which one of these is the best so far?
compare_performance(fd_noint, fd_max, fd_2, fd_3, fd_4, fd_5, fd_6, fd_7, fd_8, fd_9, fd_10, rank = T)
# ok, fd_10 is the best so far (age group removed, no interactions). But I don't trust this; everything's still underdispersed
testDispersion(fd_noint)

# Don't want to do year as a random effect--Marta argues that this would be like trying to make a boxplot with 3 data points. Doesn't make sense. See also https://stats.stackexchange.com/questions/464157/year-as-a-fixed-or-random-effect-in-glm-with-only-two-levels.

# Maybe try all of this again with a gaussian instead of a beta? Or look into the link function--log instead of logit?

fd_noint_g <- lmer(degreeRelative ~ PC1 + PC2 + age_group + season + year + (1|Nili_id), data = flight)
summary(fd_noint_g) # age group comes out the least significant.
# use DHARMa to check the model
so_fd_noint_g <- simulateResiduals(fittedModel = fd_noint_g, plot = F)
plot(so_fd_noint_g) # still really bad.
check_model(fd_noint_g) # No evidence of multicollinearity in the model without interaction terms. So the inflated VIF's shown in the other models are the result of the interactions, I guess.
testDispersion(fd_noint_g)
plotResiduals(fd_noint_g, form = flight$season)
plotResiduals(fd_noint_g, form = flight$PC1)
plotResiduals(fd_noint_g, form = flight$PC2)
plotResiduals(fd_noint_g, form = flight$age_group)
plotResiduals(fd_noint_g, form = flight$year)
plot_model(fd_noint_g, type = "pred", term = "PC2", show.data = TRUE)

fd_max_g <- glmmTMB(degreeRelative ~ PC1*age_group*season + PC2*age_group*season + year + (1|Nili_id), family = gaussian(), data = flight)
check_model(fd_max_g) # this is kind of a disaster with the inflated VIF due to multiple interaction terms.
summary(fd_max_g)

fd_2_g <- glmmTMB(degreeRelative ~ PC1*age_group + PC1*season + PC2*age_group*season + year + (1|Nili_id), family = gaussian(), data = flight)
check_model(fd_2_g) # better, but still inflated vif.
summary(fd_2)

fd_3_g <- glmmTMB(degreeRelative ~ PC1 + PC1*season + PC2*age_group*season + year + (1|Nili_id), family = gaussian(), data = flight)
check_model(fd_3_g) # still bad
summary(fd_3_g)

fd_4_g <- glmmTMB(degreeRelative ~ PC1 + PC2*age_group*season + year + (1|Nili_id), family = gaussian(), data = flight)
check_model(fd_4_g) # VIF's still bad, and the rest of the diagnostics are also bad.
summary(fd_4) 

fd_5_g <- glmmTMB(degreeRelative ~ PC1 + PC2*age_group + PC2*season + year + (1|Nili_id), family = gaussian(), data = flight)
check_model(fd_5_g) # oh hey this is getting better! Some of the diagnostics are still kind of rough, but at least the VIF is back in a normal range.
summary(fd_5)

fd_6_g <- glmmTMB(degreeRelative ~ PC1 + PC2*age_group + season + year + (1|Nili_id), family = gaussian(), data = flight)
check_model(fd_6_g) # all VIF's are good now. The other diagnostics aren't wonderful, but not terrible either.
summary(fd_6)

fd_7_g <- glmmTMB(degreeRelative ~ PC1 + PC2*season + age_group + year + (1|Nili_id), family = gaussian(), data = flight)
check_model(fd_7_g) # VIF is worse again here.
summary(fd_7)

# the next step would be to remove both interactions, and that would get us back to the fd_noint model. For good measure, going to include two models with the PC1 interactions but without PC2 interactions. Expect these both to be bad.

fd_8_g <- glmmTMB(degreeRelative ~ PC2 + PC1*age_group + season + year + (1|Nili_id), family = gaussian(), data = flight)
check_model(fd_8_g) # back to being happier
summary(fd_8)

fd_9_g <- glmmTMB(degreeRelative ~ PC2 + PC1*season + age_group + year + (1|Nili_id), family = gaussian(), data = flight)
check_model(fd_9_g)
summary(fd_9) 

# Now we move on to removing main effects.
summary(fd_noint_g) # interestingly, age group is the first to go.

fd_10_g <- glmmTMB(degreeRelative ~ PC1 + PC2 + season + year + (1 | Nili_id), family = gaussian(), data = flight)
check_model(fd_10_g) # looks about the same?
summary(fd_10) # wow okay!

compare_performance(fd_noint_g, fd_max_g, fd_2_g, fd_3_g, fd_4_g, fd_5_g, fd_6_g, fd_7_g, fd_8_g, fd_9_g, fd_10_g, rank = T)
# 3, 4, then 10. 
# ? Is it even reasonable to do this check including models that have a bad fit according to the diagnostic plots?
