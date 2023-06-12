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
library(broom.mixed)
library(jtools) # similar to sjplot, for forest plots

# Set ggplot theme to classic
theme_set(theme_classic())

# Set season colors
seasonColors <- c("#2FF8CA", "#CA2FF8", "#F8CA2F")
situationColors <- c("dodgerblue2", "olivedrab3", "gold")

# Here's an interesting reference article for some modeling stuff: https://biol607.github.io/lab/12_gzlm.html

# arbitrary changes to test for slowness

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
  dplyr::select(sex, type, Nili_id, degreeRelative, strengthRelative, evenness) %>%
  pivot_longer(cols = c("degreeRelative", "strengthRelative", "evenness"), 
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
  dplyr::select(age_group, type, Nili_id, degreeRelative, strengthRelative, evenness) %>%
  pivot_longer(cols = c("degreeRelative", "strengthRelative", "evenness"), names_to = "socialPositionMeasure", values_to = "value") %>%
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
  dplyr::select(type, Nili_id, degreeRelative, strengthRelative, evenness, season) %>%
  pivot_longer(cols = c("degreeRelative", "strengthRelative", "evenness"), names_to = "socialPositionMeasure", values_to = "value") %>%
  filter(!is.na(season)) %>%
  ggplot(aes(x = season, y = value, fill = season))+
  geom_boxplot()+
  theme_minimal()+
  scale_fill_manual(values = seasonColors)+
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
  palette = seasonColors,
  facet.by = "PC"
) + stat_pvalue_manual(stat.test)
bxp # we definitely have seasonal differences in movement! As expected.

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
flight <- linked %>%
  filter(type == "flight")
feeding <- linked %>%
  filter(type == "feeding")
roosting <- linked %>%
  filter(type == "roosting")

# FLIGHT ------------------------------------------------------------------
## Degree ------------------------------------------------------------------
# I thought we couldn't include random effects if they had fewer than 5 levels, but maybe you can? https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8784019/
# First, fit a model that includes all the predictors but no interaction effects, to check the VIF, which will tell us whether the predictors are correlated within the model.
# Don't want to do year as a random effect--Marta argues that this would be like trying to make a boxplot with 3 data points. Doesn't make sense. See also https://stats.stackexchange.com/questions/464157/year-as-a-fixed-or-random-effect-in-glm-with-only-two-levels.
# Not including mean ppd because Orr doesn't think the relationship is linear--better to just correct for it in data prep. I'm inclined to agree.
fd_noint <- lmer(degreeRelative ~ PC1 + PC2 + age_group + season + year + (1|Nili_id), data = flight)
summary(fd_noint) # age group comes out the least significant.
# use DHARMa to check the model
so_fd_noint <- simulateResiduals(fittedModel = fd_noint, plot = F)
plot(so_fd_noint) # this actually isn't too bad.
check_model(fd_noint) # looks good! Candidate.

# No evidence of multicollinearity, judging from the VIFs. So we should be okay continuing to use these params in the model.
testDispersion(fd_noint) # this is actually pretty good!
plotResiduals(fd_noint, form = flight$season)
plotResiduals(fd_noint, form = flight$PC1) # bad but not terrible
plotResiduals(fd_noint, form = flight$PC2) # bad but not terrible
plotResiduals(fd_noint, form = flight$age_group)
plotResiduals(fd_noint, form = flight$year)
plot_model(fd_noint, type = "pred", term = "PC2", show.data = TRUE) # I don't like this linear fit, but I don't think the quadratic term is very interpretable, and I can't justify it (at least not now)

fd_max <- lmer(degreeRelative ~ PC1*age_group*season + PC2*age_group*season + season*year + (1|Nili_id), data = flight)
check_model(fd_max) # oof those VIFs are baaad. Not candidate
summary(fd_max)
vif(fd_max) #PC1*season is the worst, so let's remove that.

fd_2 <- lmer(degreeRelative ~ PC1*age_group + PC2*age_group*season + year*season + (1|Nili_id), data = flight)
check_model(fd_2) # VIFs slightly improved but still really bad. Not candidate.
vif(fd_2) #PC2*season is the worst, so we'll remove that next

fd_3 <- lmer(degreeRelative ~ PC1*age_group + PC2*age_group + PC2*season + year*season + (1|Nili_id), data = flight)
check_model(fd_3) # Not candidate
vif(fd_3) #season*year is the worst

fd_4 <- lmer(degreeRelative ~ PC1*age_group + PC2*age_group + PC2*season + year + (1|Nili_id), data = flight)
check_model(fd_4) # Slightly inflated VIF but better. Candidate.
vif(fd_4) # Next remove PC2*season.

fd_5 <- lmer(degreeRelative ~ PC1*age_group + PC2*age_group + season + year + (1|Nili_id), data = flight)
check_model(fd_5) # Candidate.
vif(fd_5) # This looks fine.
summary(fd_5) # not seeing significant effects of either PC1 or PC2 interacted with age. 

fd_6 <- lmer(degreeRelative ~ PC1*age_group + PC2 + season + year + (1|Nili_id), data = flight)
check_model(fd_6) # Candidate.
summary(fd_6) # removing the last interaction would just bring us back to the noint model.

summary(fd_noint) # the only non-significant thing to remove now would be PC1, but I don't want to do that because PC1 being non-significant is a result in and of itself. So I think I'll stop.

compare_performance(fd_noint, fd_4, fd_5, fd_6, rank = T) # of these options, fd_noint comes up as the best. 
fd_mod <- fd_noint # save as the model to use going forward.

## Strength ------------------------------------------------------------------
fs_noint <- lmer(strengthRelative ~ PC1 + PC2 + age_group + season + year + (1|Nili_id), data = flight)
so_fs_noint <- simulateResiduals(fittedModel = fs_noint, plot = F)
plot(so_fs_noint) # Looks good!!! Incredible.
check_model(fs_noint) # looks good! Candidate.

fs_max <- lmer(strengthRelative ~ PC1*age_group*season + PC2*age_group*season + season*year + (1|Nili_id), data = flight)
check_model(fs_max) # Not Candidate.
vif(fs_max) # I'm worried because this looks identical to vif(fd_max). Why? Maybe because this evaluates the variables as a whole, not with respect to the response variable? XXX investigate.

fs_2 <- lmer(strengthRelative ~ PC1*age_group + PC2*age_group*season + season*year + (1|Nili_id), data = flight)
check_model(fs_2) # Not Candidate.
vif(fs_2)

fs_3 <- lmer(strengthRelative ~ PC1*age_group + PC2*age_group + season*year + (1|Nili_id), data = flight)
check_model(fs_3) # Candidate.
vif(fs_3)

fs_4 <- lmer(strengthRelative ~ PC1*age_group + PC2*age_group + season + year + (1|Nili_id), data = flight)
check_model(fs_4) # Candidate.
summary(fs_4)

fs_5 <- lmer(strengthRelative ~ PC1*age_group + PC2 + season + year + (1|Nili_id), data = flight)
check_model(fs_5) # Candidate.
summary(fs_5) # neither of the PCs are significant here, but I don't want to remove them, so I'll stop here.

compare_performance(fs_noint, fs_3, fs_4, fs_5, rank = T)
fs_mod <- fs_4

## Evenness ----------------------------------------------------------------
fe <- flight %>%
  filter(!is.na(evenness)) %>%
  mutate(evenness = case_when(evenness == 1 ~ 0.999999, # transform the 1's to 0.999999 so the model can handle them.
                              TRUE ~ evenness))
fe_noint <- lmer(evenness ~ PC1 + PC2 + age_group + season + year + (1|Nili_id), data = fe)
so_fe_noint <- simulateResiduals(fittedModel = fe_noint, plot = F)
plot(so_fe_noint) # Yuck...
check_model(fe_noint) # distribution looks really off here. Maybe it's not reasonable to model this one with a gaussian.

fe_noint <- glmmTMB(evenness ~ PC1 + PC2 + age_group + season + year + (1|Nili_id), data = fe, family = beta_family())
so_fe_noint <- simulateResiduals(fittedModel = fe_noint, plot = F)
plot(so_fe_noint) # Looks a bit better but still not good.
check_model(fe_noint) # oof, that distribution is still a really awful fit. But it's a bit better, and apparently Conner has seen worse? Let's go with it for now. Candidate.

fe_max <- glmmTMB(evenness ~ PC1*age_group*season + PC2*age_group*season + season*year + (1|Nili_id), data = fe, family = beta_family()) # does not converge

fe_2 <- glmmTMB(evenness ~ PC1*age_group*season + PC2*age_group + season*year + (1|Nili_id), data = fe, family = beta_family()) # does not converge

fe_3 <- glmmTMB(evenness ~ PC1*age_group + PC2*age_group + season*year + (1|Nili_id), data = fe, family = beta_family()) # does not converge

# Let's remove season*year next...
fe_4 <- glmmTMB(evenness ~ PC1*age_group + PC2*age_group + season + year + (1|Nili_id), data = fe, family = beta_family()) # okay now it converges
check_model(fe_4) # this looks fine, actually. Candidate.
summary(fe_4) # Let's remove the age_group interactions.

fe_5 <- glmmTMB(evenness ~ PC1 + PC2*age_group + season + year + (1|Nili_id), data = fe, family = beta_family())
check_model(fe_5) # fine, candidate.
summary(fe_5) # Remove the other age_group interaction

# Removing the next interaction would just bring us back to fe_noint
summary(fe_noint) # Can't go any farther here without removing the PC1 and PC2 values.

compare_performance(fe_noint, fe_4, fe_5, rank = T)
fe_mod <- fe_noint

# FEEDING ------------------------------------------------------------------
## Degree ------------------------------------------------------------------
ed_noint <- lmer(degreeRelative ~ PC1 + PC2 + age_group + season + year + (1|Nili_id), data = feeding)
summary(ed_noint) # age group comes out the least significant.

# use DHARMa to check the model
so_ed_noint <- simulateResiduals(fittedModel = ed_noint, plot = F)
plot(so_ed_noint) # Not great but not terrible.
check_model(ed_noint) # looks good! Candidate.

ed_max <- lmer(degreeRelative ~ PC1*age_group*season + PC2*age_group*season + season*year + (1|Nili_id), data = feeding)
check_model(ed_max) # Not candidate
vif(ed_max) #PC1*season is the worst

ed_2 <- lmer(degreeRelative ~ PC1*age_group + PC2*age_group*season + season*year + (1|Nili_id), data = feeding)
check_model(ed_2) # Not candidate
vif(ed_2) #next to remove: PC2*season

ed_3 <- lmer(degreeRelative ~ PC1*age_group + PC2*age_group + season*year + (1|Nili_id), data = feeding)
check_model(ed_3) # Candidate, marginally.
vif(ed_3) #next to remove: season*year. So far this is following a similar pattern as before.

ed_4 <- lmer(degreeRelative ~ PC1*age_group + PC2*age_group + season + year + (1|Nili_id), data = feeding)
check_model(ed_4) # Candidate 
summary(ed_4) # interesting! Let's get rid of those interaction terms.

ed_5 <- lmer(degreeRelative ~ PC1*age_group + PC2 + season + year + (1|Nili_id), data = feeding)
check_model(ed_5) # Candidate 
summary(ed_5) # removing the next interaction term would just get us back to ed_noint

summary(ed_noint) # I suppose we can remove age...

ed_6 <- lmer(degreeRelative ~ PC1 + PC2 + season + year + (1|Nili_id), data = feeding)
check_model(ed_6) # Candidate
summary(ed_6) # that's the best we can do without removing PCs.

compare_performance(ed_noint, ed_3, ed_4, ed_5, ed_6, rank = T)
ed_mod <- ed_3

## Strength ------------------------------------------------------------------
es_noint <- lmer(strengthRelative ~ PC1 + PC2 + age_group + season + year + (1|Nili_id), data = feeding)
summary(es_noint) # age group comes out the least significant.

# use DHARMa to check the model
so_es_noint <- simulateResiduals(fittedModel = es_noint, plot = F)
plot(so_es_noint) # Not great but not terrible.
check_model(es_noint) # Candidate

es_max <- lmer(strengthRelative ~ PC1*age_group*season + PC2*age_group*season + season*year + (1|Nili_id), data = feeding)
check_model(es_max) # Not candidate
vif(es_max) #PC1*season is the worst

es_2 <- lmer(strengthRelative ~ PC1*age_group + PC2*age_group*season + season*year + (1|Nili_id), data = feeding)
check_model(es_2) # Not candidate
vif(es_2) # Next remove PC2*season

es_3 <- lmer(strengthRelative ~ PC1*age_group + PC2*age_group + season*year + (1|Nili_id), data = feeding)
check_model(es_3) # Candidate
vif(es_3) # Next remove season*year

es_4 <- lmer(strengthRelative ~ PC1*age_group + PC2*age_group + season + year + (1|Nili_id), data = feeding)
check_model(es_4) # Candidate
summary(es_4) # Next remove the interactions with age_group

es_5 <- lmer(strengthRelative ~ PC1*age_group + PC2 + season + year + (1|Nili_id), data = feeding)
check_model(es_5) # Candidate
summary(es_5) # Next thing to remove would be the interaction with PC1*age_group, but that would just take us back to es_noint

summary(es_noint) # I guess the next thing is to remove age_group entirely.

es_6 <- lmer(strengthRelative ~ PC1 + PC2 + season + year + (1|Nili_id), data = feeding)
check_model(es_6) # Candidate
summary(es_6) # Looks like this is as far as we can go.

compare_performance(es_noint, es_3, es_4, es_5, es_6, rank = T)
es_mod <- es_3

## Evenness ----------------------------------------------------------------
ee <- feeding %>%
  filter(!is.na(evenness)) %>%
  mutate(evenness = case_when(evenness == 1 ~ 0.999999, # transform the 1's to 0.999999 so the model can handle them.
                              TRUE ~ evenness))

ee_noint <- glmmTMB(evenness ~ PC1 + PC2 + age_group + season + year + (1|Nili_id), data = ee, family = beta_family())
so_ee_noint <- simulateResiduals(fittedModel = ee_noint, plot = F)
plot(so_ee_noint) # Bleh....
check_model(ee_noint) # This actually looks really good! Candidate.

ee_max <- glmmTMB(degreeRelative ~ PC1*age_group*season + PC2*age_group*season + season*year + (1|Nili_id), data = ee, family = beta_family()) # fails to converge

ee_2 <- glmmTMB(degreeRelative ~ PC1*age_group*season + PC2*age_group + season*year + (1|Nili_id), data = ee, family = beta_family()) # fails to converge

ee_3 <- glmmTMB(degreeRelative ~ PC1*age_group + PC2*age_group + season*year + (1|Nili_id), data = ee, family = beta_family()) # fails to converge

ee_4 <- glmmTMB(degreeRelative ~ PC1*age_group + PC2*age_group + season + year + (1|Nili_id), data = ee, family = beta_family()) # Converges!
check_model(ee_4) # Looks pretty good!
summary(ee_4) # Remove PC1*age_group

ee_5 <- glmmTMB(degreeRelative ~ PC1 + PC2*age_group + season + year + (1|Nili_id), data = ee, family = beta_family()) # Converges!
check_model(ee_5) # Candidate
summary(ee_5) # This looks like the best we can do.

compare_performance(ee_noint, ee_4, ee_5, rank = T)
ee_mod <- ee_noint

# ROOSTING ------------------------------------------------------------------
## Degree ------------------------------------------------------------------
rd_noint <- lmer(degreeRelative ~ PC1 + PC2 + age_group + season + year + (1|Nili_id), data = roosting) # singular fit. I highly doubt we need to make the model *more* complex. I wonder if we need to remove the random effect?

rd_noint_norandom <- lm(degreeRelative ~ PC1 + PC2 + age_group + season + year, data = roosting) # well, this converges, at least.
check_model(rd_noint_norandom) #Hmm, this is a fairly bad fit. A beta distribution might be better? Still, candidate.

rd_noint_norandom_beta <- glmmTMB(degreeRelative ~ PC1 + PC2 + age_group + season + year, data = roosting, family = beta_family()) # converges
check_model(rd_noint_norandom_beta) # this is no better than the linear one, and it's less interpretable.

rd_max_norandom <- lm(degreeRelative ~ PC1*age_group*season + PC2*age_group*season + season*year, data = roosting)
check_model(rd_max_norandom) # much better fit, but bad vifs. Not candidate
check_collinearity(rd_max_norandom) # worst one is PC1*season, as usual.

rd_2_norandom <- lm(degreeRelative ~ PC1*age_group + PC2*age_group*season + season*year, data = roosting)
check_model(rd_2_norandom) # Not candidate
check_collinearity(rd_2_norandom) # Worst is PC2*season*age_group. Let's remove PC2*season.

rd_3_norandom <- lm(degreeRelative ~ PC1*age_group + PC2*age_group + season*year, data = roosting)
check_model(rd_3_norandom) # Candidate (borderline)
check_collinearity(rd_3_norandom) # worst one is season*year

rd_4_norandom <- lm(degreeRelative ~ PC1*age_group + PC2*age_group + season + year, data = roosting)
check_model(rd_4_norandom) # Candidate, but now again the distribution looks kind of bad.
summary(rd_4_norandom) # next to get rid of: PC1*age group

rd_5_norandom <- lm(degreeRelative ~ PC1 + PC2*age_group + season + year, data = roosting)
check_model(rd_5_norandom) # Candidate
summary(rd_5_norandom) # Age group is still bad here, but we can't remove it because the interaction with PC2 is significant.

# What about adding back season*year? Will this fix the distribution problem?
rd_6_norandom <- lm(degreeRelative ~ PC1 + PC2*age_group + season*year, data = roosting)
check_model(rd_6_norandom) # Yes, it fixes the distribution, but it raises the vifs.
check_collinearity(rd_6_norandom)
summary(rd_6_norandom) # We get NA values for season*year coefs, so let's not use those.

summary(rd_5_norandom) # this is as far as I can go.

compare_performance(rd_noint_norandom, rd_3_norandom, rd_4_norandom, rd_5_norandom, rank = T)
rd_mod <- rd_3_norandom

## Strength ------------------------------------------------------------------
rs_noint <- lmer(strengthRelative ~ PC1 + PC2 + age_group + season + year + (1|Nili_id), data = roosting)
check_model(rs_noint) # this is pretty good actually! Just one influential observation, but everything else looks good.

rs_max <- lmer(strengthRelative ~ PC1*age_group*season + PC2*age_group*season + season*year + (1|Nili_id), data = roosting)
check_model(rs_max) # Not candidate. Too much VIF
vif(rs_max) # once again, PC1*season is the culprit

rs_2 <- lmer(strengthRelative ~ PC1*age_group + PC2*age_group*season + season*year + (1|Nili_id), data = roosting)
check_model(rs_2) # Not candidate.
vif(rs_2) # PC2*season

rs_3 <- lmer(strengthRelative ~ PC1*age_group + PC2*age_group + season*year + (1|Nili_id), data = roosting)
check_model(rs_3) # Candidate
vif(rs_3) # season*year

rs_4 <- lmer(strengthRelative ~ PC1*age_group + PC2*age_group + season + year + (1|Nili_id), data = roosting)
check_model(rs_4) # Candidate. Looks really nice actually!
summary(rs_4) # no effect of PC1*age_group

rs_5 <- lmer(strengthRelative ~ PC1 + PC2*age_group + season + year + (1|Nili_id), data = roosting)
check_model(rs_5) # Candidate
summary(rs_5) # time to stop.

compare_performance(rs_noint, rs_3, rs_4, rs_5, rank = T)
rs_mod <- rs_5

## Evenness ----------------------------------------------------------------
re <- roosting %>%
  filter(!is.na(evenness)) %>%
  mutate(evenness = case_when(evenness == 1 ~ 0.999999, # transform the 1's to 0.999999 so the model can handle them.
                              TRUE ~ evenness))

re_noint <- glmmTMB(evenness ~ PC1 + PC2 + age_group + season + year + (1|Nili_id), data = re, family = beta_family())
so_re_noint <- simulateResiduals(fittedModel = re_noint, plot = F)
plot(so_re_noint) # Yuck...
check_model(re_noint) # Candidate! Actually not bad at all.

re_max <- glmmTMB(evenness ~ PC1*age_group*season + PC2*age_group*season + season*year + (1|Nili_id), data = re, family = beta_family()) # fails to converge

re_2 <- glmmTMB(evenness ~ PC1*age_group*season + PC2*age_group + season*year + (1|Nili_id), data = re, family = beta_family()) # fails to converge

re_3 <- glmmTMB(evenness ~ PC1*age_group + PC2*age_group + season*year + (1|Nili_id), data = re, family = beta_family()) # fails to converge

re_4 <- glmmTMB(evenness ~ PC1*age_group + PC2*age_group + season + year + (1|Nili_id), data = re, family = beta_family()) # converges!
check_model(re_4) # Candidate
summary(re_4) # can remove PC1*age_group

re_5 <- glmmTMB(evenness ~ PC1 + PC2*age_group + season + year + (1|Nili_id), data = re, family = beta_family())
check_model(re_5) # Candidate
summary(re_5) # Can't go any farther without removing PC2.

compare_performance(re_noint, re_4, re_5, rank = T)
re_mod <- re_5


# Effect plots ------------------------------------------------------------
# We now have 9 models. Let's compile and tidy their outputs.
mods <- list("fd" = fd_mod, "fs" = fs_mod, "fe" = fe_mod, "ed" = ed_mod, "es" = es_mod, "ee" = ee_mod, "rd" = rd_mod, "rs" = rs_mod, "re" = re_mod)
type <- substr(names(mods), 1, 1)
response <- substr(names(mods), 2, 2)
outputs <- map(mods, broom.mixed::tidy)

effects <- outputs %>%
  map2(., type, ~.x %>% mutate(type = .y) %>%
         relocate(type)) %>%
  map2(., response, ~.x %>% mutate(response = .y) %>%
         relocate(response, .after = "type")) %>%
  purrr::list_rbind() %>%
  mutate(type = case_when(type == "f" ~ "flight",
                          type == "e" ~ "feeding",
                          type == "r" ~ "roosting"),
         response = case_when(response == "d" ~ "degreeRelative",
                              response == "s" ~ "strengthRelative",
                              response == "e" ~ "evenness"))

save(mods, file = "data/mods.Rda")
save(effects, file = "data/effects.Rda")

# Now make effect size plots
effects %>%
  mutate(sig.05 = case_when(p.value < 0.05 ~ T,
                            TRUE ~F),
         sig.01 = case_when(p.value < 0.01 ~ T,
                            TRUE ~ F)) %>%
  filter(term %in% c("PC1", "PC2")) %>%
  ggplot(aes(x = response, y = estimate, col = type))+
  geom_point(size = 3, aes(shape = sig.05))+
  scale_color_manual(name = "Situation", values = situationColors)+
  ylab("Effect size")+
  facet_wrap(~term) # okay, this shows everything but it is definitely not the right way to visualize.

plot_model(fd_mod, type = "eff")

# Degree
plot_summs(fd_mod, ed_mod, rd_mod, inner_ci_level = .9,
           model.names = c("Flight", "Feeding", "Roosting"), coefs = c("PC1", "PC2"))+
  scale_color_manual(values = situationColors)+
  ggtitle("Degree (normalized)")

# Degree
plot_summs(fs_mod, es_mod, rs_mod, inner_ci_level = .9,
           model.names = c("Flight", "Feeding", "Roosting"), coefs = c("PC1", "PC2"))+
  scale_color_manual(values = situationColors)+
  ggtitle("Strength (normalized)")

# Evenness
plot_summs(fe_mod, ee_mod, re_mod, inner_ci_level = .9,
           model.names = c("Flight", "Feeding", "Roosting"), coefs = c("PC1", "PC2"))+
  scale_color_manual(values = situationColors)+
  ggtitle("Evenness")


# plots -------------------------------------------------------------------


plot_model(fb_mod, type = "pred", terms = "PC1 [all]", col = "white")+
  theme(panel.background = element_rect(fill = "#595959"),
        plot.background = element_rect(fill = "#595959"),
        text = element_text(color = "white", size = 15),
        axis.text = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        axis.line = element_line(color = "white"))+
  ylab("Average strength")+
  ggtitle("")+
  theme(text = element_text(size = 18)) # sig but small change
plot_model(fb_mod, type = "pred", terms = "PC2 [all]", col = "white")+
  theme(panel.background = element_rect(fill = "#595959"),
        plot.background = element_rect(fill = "#595959"),
        text = element_text(color = "white", size = 15),
        axis.text = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        axis.line = element_line(color = "white"))+
  ylab("Average strength")+
  ggtitle("")+
  theme(text = element_text(size = 18))# sig but small change
plot_model(fb_mod, type = "pred", terms = "age_group") # sig but small change
plot_model(fb_mod, type = "pred", terms = "season") # sig but small change
plot_model(fb_mod, type = "pred", terms = "season [all]", col = "white")+
  theme(panel.background = element_rect(fill = "#595959"),
        plot.background = element_rect(fill = "#595959"),
        text = element_text(color = "white", size = 15),
        axis.text = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        axis.line = element_line(color = "white"))+
  ylab("Average strength")+
  ggtitle("")+
  theme(text = element_text(size = 18))
plot_model(fb_mod, type = "pred", terms = "year") # sig but small change
plot_model(fb_mod, type = "pred", terms = "mnPPD") # sig but small change
plot_model(fb_mod, type = "pred", terms = c("PC2 [all]", "age_group"))+
  ggtitle("")+
  ylab("Average strength")+
  theme(text = element_text(size = 18))# sig but small change. Kind of mirrors the effect found in strength, but less strong.

# Let's look at the PC contributions
contrib <- contrib %>%
  mutate(var = row.names(.),
         varName = case_when(var == "meanDMD" ~ "Mn. daily max displacement",
                             var == "propSwitch" ~"Prop. nights roost-switching",
                             var == "shannon" ~"Roost diversity",
                             var == "coreArea" ~"50% KDE",
                             var == "coreAreaFidelity" ~"50%/95% KDE",
                             var == "homeRange"~"95% KDE",
                             var == "meanDDT" ~ "Mn. daily distance traveled",
                             var == "uniqueRoosts"~"# unique roosts",
                             var == "meanDFD" ~"Mn. daily flight time",
                             var == "mnDailyMaxAlt" ~"Mn. daily max. altitude",
                             var == "mnDailyMedAlt" ~"Mn. daily median altitude",
                             var == "mnTort" ~ "Mn. daily tortuosity"))

# PC1
PC1contrib <- contrib %>%
  ggplot(aes(x = reorder(varName, PC1), y = PC1))+
  geom_col(fill = "white")+
  coord_flip()+
  ylab("PC1 % contribution")+
  xlab("")+
  theme(panel.background = element_rect(fill = "#666666"),
        plot.background = element_rect(fill = "#666666"),
        text = element_text(color = "white", size = 15),
        axis.text = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        axis.line = element_line(color = "white"))+
  theme(text = element_text(size = 18))
PC1contrib

PC2contrib <- contrib %>%
  ggplot(aes(x = reorder(varName, PC2), y = PC2))+
  geom_col(fill = "white")+
  coord_flip()+
  ylab("PC2 % contribution")+
  xlab("")+
  theme(panel.background = element_rect(fill = "#666666"),
        plot.background = element_rect(fill = "#666666"),
        text = element_text(color = "white", size = 15),
        axis.text = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        axis.line = element_line(color = "white"))+
  theme(text = element_text(size = 18))
PC2contrib

