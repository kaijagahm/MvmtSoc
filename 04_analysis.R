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
# Don't want to do year as a random effect--Marta argues that this would be like trying to make a boxplot with 3 data points. Doesn't make sense. See also https://stats.stackexchange.com/questions/464157/year-as-a-fixed-or-random-effect-in-glm-with-only-two-levels.
fd_noint <- lmer(degreeRelative ~ PC1 + PC2 + age_group + season + year + 
                   mnPPD + (1|Nili_id), data = flight)
summary(fd_noint) # age group comes out the least significant.
# use DHARMa to check the model
so_fd_noint <- simulateResiduals(fittedModel = fd_noint, plot = F)
plot(so_fd_noint) # this looks bad
check_model(fd_noint) # this looks fine, actually! Candidate
# why does DHARMa look terrible when check_model looks fine?
# Anyway, no evidence of multicollinearity, judging from the VIFs. So we should be okay continuing to use these params in the model.
testDispersion(fd_noint) # this is actually pretty good!
plotResiduals(fd_noint, form = flight$season)
plotResiduals(fd_noint, form = flight$PC1)
plotResiduals(fd_noint, form = flight$PC2)
plotResiduals(fd_noint, form = flight$age_group)
plotResiduals(fd_noint, form = flight$year)
plot_model(fd_noint, type = "pred", term = "PC2", show.data = TRUE) # this is a pretty bad fit. Marta suggests a nonlinear term here.

fd_max <- lmer(degreeRelative ~ PC1*age_group*season + PC2*age_group*season + mnPPD*season + year + (1|Nili_id), data = flight)
check_model(fd_max) # oof those VIFs are baaad. Not candidate
summary(fd_max) # let's start by taking out season*mnPPD
vif(fd_max)

fd_2 <- lmer(degreeRelative ~ PC1*age_group*season + PC2*age_group*season + mnPPD + year + (1|Nili_id), data = flight)
check_model(fd_2) # VIFs slightly improved but still really bad. Not candidate.
vif(fd_2) #PC1*season is the worst, so let's remove that.
summary(fd_2)

fd_3 <- lmer(degreeRelative ~ PC1*age_group + PC2*age_group*season + mnPPD + year + (1|Nili_id), data = flight)
check_model(fd_3) # okay, getting better. Not candidate.
vif(fd_3)

fd_4 <- lmer(degreeRelative ~ PC1*age_group + PC2 + season + mnPPD + year + (1|Nili_id), data = flight)
check_model(fd_4) # waaay better on the VIF front. Candidate
vif(fd_4)

# I don't love that I had to take out the interactions of the PCs with season, though. What about putting back PC2*season?
fd_5 <- lmer(degreeRelative ~ PC1*age_group + PC2*season + mnPPD + year + (1|Nili_id), data = flight)
vif(fd_5)
check_model(fd_5) # not candidate

# Going back to fd_4, the vif values all look totally fine. Let's look at the summary to decide what to remove next.
summary(fd_4) # okay, no effect of either PC1*age_group or mnPPD. Let's get rid of mnPPD first, because it's less interpretable.
fd_6 <- lmer(degreeRelative ~ PC1*age_group + PC2 + season + year + (1|Nili_id), data = flight)
check_model(fd_6) # ok, reasonable. Candidate.
summary(fd_6)

# Now we can get rid of the PC1*age_group effect. This is the same as fd_noint except without mnPPD.
fd_7 <- lmer(degreeRelative ~ PC1 + PC2 + season + age_group + year + (1|Nili_id), data = flight)
check_model(fd_7) # candidate
summary(fd_7) # next to go is age

# remove age_group
fd_8 <- lmer(degreeRelative ~ PC1 + PC2 + season + year + (1|Nili_id), data = flight)
check_model(fd_8) # candidate
summary(fd_8)

# remove PC1
fd_9 <- lmer(degreeRelative ~ PC2 + season + year + (1|Nili_id), data = flight)
check_model(fd_9) # this has some bad influential observations, plus I don't really want to get rid of PC1 conceptually, so I think I won't include it. Not candidate.
summary(fd_9)

# Compare performance of only the models with a reasonable fit
compare_performance(fd_noint, fd_4, fd_6, fd_7, fd_8, rank = T) # fd_8 is best, score 87.5%, r2 conditional = 0.64
# Compare performance of everything regardless of reasonable fit
compare_performance(fd_noint, fd_max, fd_2, fd_3, fd_4, fd_5, fd_6, fd_7, fd_8, fd_9, rank = T) # model 9 (without PC1) does better, followed by model 8. On principle I don't really want to remove PC1, so maybe we'll keep model 8.

# FLIGHT: RELATIVE STRENGTH
# Theoretically we might want to do the same model selection for strength as for degree (same models in same order), but just to see, I'm going to do the process as if it were its own thing.
# I think gaussian is still reasonable here:
flight %>% ggplot(aes(x = strengthRelative))+geom_histogram() # yeah that's decently normal-shaped.

fs_noint <- lmer(strengthRelative ~ PC1 + PC2 + age_group + season + year + 
                   mnPPD + (1|Nili_id), data = flight)
summary(fs_noint) # oh weird, no season effect here! Also no PC2, though PC1 is more significant. Age group does show up this time, and ppd remains not that significant.
check_model(fs_noint) # oh wow this actually looks perfect!!! Candidate.
# What does DHARMa think?
so_fs_noint <- simulateResiduals(fittedModel = fs_noint, plot = F)
plot(so_fs_noint) # holy moly that's so nice! Yesssss
# No evidence of multicollinearity.
testDispersion(fs_noint) # beautiful
plotResiduals(fs_noint, form = flight$season) # beautiful
plotResiduals(fs_noint, form = flight$PC1) # meh but not terrible
plotResiduals(fs_noint, form = flight$PC2) # meh but not quite as bad as the other one
plotResiduals(fs_noint, form = flight$age_group) # not terrible
plotResiduals(fs_noint, form = flight$year) # yay!
plot_model(fs_noint, type = "pred", term = "PC2", show.data = TRUE) # Again, a pretty bad fit; might want a nonlinear term.
plot_model(fs_noint, type = "pred", term = "PC1", show.data = TRUE) # also kind of bad

fs_max <- lmer(strengthRelative ~ PC1*age_group*season + PC2*age_group*season + mnPPD*season + year + (1|Nili_id), data = flight)
check_model(fs_max) # really bad VIFs. Not candidate.
summary(fs_max) # let's start by taking out season*mnPPD (so far, this is the same as the degree model.)
vif(fs_max)

fs_2 <- lmer(strengthRelative ~ PC1*age_group*season + PC2*age_group*season + mnPPD + year + (1|Nili_id), data = flight)
check_model(fs_2) # VIFs slightly improved but still really bad. Not candidate.
vif(fs_2) #PC1*season is the worst, so let's remove that. (still same as degree model)
summary(fs_2)

fs_3 <- lmer(strengthRelative ~ PC1*age_group + PC2*age_group*season + mnPPD + year + (1|Nili_id), data = flight)
check_model(fs_3) # okay, getting better, but not candidate (same as degree)
vif(fs_3)

# remove PC2*age_group*season interaction (same as degree)
fs_4 <- lmer(strengthRelative ~ PC1*age_group + PC2 + season + mnPPD + year + (1|Nili_id), data = flight)
check_model(fs_4) # waaay better on the VIF front. Candidate. Looks great. Same as degree
vif(fs_4) # these look fine.

# I don't love that I had to take out the interactions of the PCs with season, though. What about putting back PC2*season? (same as degree)
fs_5 <- lmer(strengthRelative ~ PC1*age_group + PC2*season + mnPPD + year + (1|Nili_id), data = flight)
vif(fs_5) # not great
check_model(fs_5) # not candidate--vif too high. (same as degree).

# Going back to fs_4, the vif values all look totally fine. Let's look at the summary to decide what to remove next.
summary(fs_4) # Here we diverge from the degree model. No effect of season at all. Marginal effects of mnPPD and PC1*age_group. Also no PC2 effect! Strange. Get rid of season...
fs_6 <- lmer(strengthRelative ~ PC1*age_group + PC2 + year + mnPPD + (1|Nili_id), data = flight)
check_model(fs_6) # looks great! candidate.
summary(fs_6)

# Same as for degree, the PC1*age_group effect is only marginally significant (it was n.s. for degree). Let's remove it. (I'm avoiding removing PC2 until the end.)
fs_7 <- lmer(strengthRelative ~ PC1 + PC2 + age_group + year + (1|Nili_id), data = flight)
check_model(fs_7) # looks good. Candidate.
summary(fs_7) # age is now n.s. Remove it...

# remove age_group
fs_8 <- lmer(strengthRelative ~ PC1 + PC2 + year + (1|Nili_id), data = flight)
check_model(fs_8)  # candidate
summary(fs_8) # Now PC2 is n.s.

# remove PC2
fs_9 <- lmer(strengthRelative ~ PC1 + year + (1|Nili_id), data = flight)
check_model(fs_9) # has some bad influential observations, plus I don't really want to get rid of the PCs conceptually, so not candidate.
summary(fs_9)

# Compare performance of only the models with a reasonable fit
compare_performance(fs_noint, fs_4, fs_6, fs_7, fs_8, rank = T) # fs_6 is the best: strengthRelative ~ PC1*age_group + PC2 + year + mnPPD + (1|Nili_id)
# Compare performance of everything regardless of reasonable fit
compare_performance(fs_noint, fs_max, fs_2, fs_3, fs_4, fs_5, fs_6, fs_7, fs_8, fs_9, rank = T) # interesting--fs_3 wins and fs_6 is nowhere near the best.
