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
fd_noint <- lmer(degreeRelative ~ PC1 + poly(PC2, 2) + age_group + season + year + 
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
plot_model(fd_noint, type = "pred", term = "PC2", show.data = TRUE) # better fit than with just the linear term, but I worry that this will become the entire story because it has an easily interpretable shape...

fd_max <- lmer(degreeRelative ~ PC1*age_group*season + poly(PC2, 2)*age_group*season + mnPPD*season + year + (1|Nili_id), data = flight)
check_model(fd_max) # oof those VIFs are baaad. Not candidate
summary(fd_max) # let's start by taking out season*mnPPD
vif(fd_max)

fd_2 <- lmer(degreeRelative ~ PC1*age_group*season + poly(PC2, 2)*age_group*season + mnPPD + year + (1|Nili_id), data = flight)
check_model(fd_2) # VIFs slightly improved but still really bad. Not candidate.
vif(fd_2) #PC2*season*age_group is the worst, so let's remove that.
summary(fd_2)

fd_3 <- lmer(degreeRelative ~ PC1*age_group*season + poly(PC2, 2) + mnPPD + year + (1|Nili_id), data = flight)
check_model(fd_3) # still really high VIF
vif(fd_3) # highest is PC1*season, so we'll remove that next. Not candidate.
summary(fd_3)

fd_4 <- lmer(degreeRelative ~ PC1*age_group + season + poly(PC2, 2) + mnPPD + year + (1|Nili_id), data = flight)
check_model(fd_4) # all VIF's look good, and the rest of the fit is fine. Candidate.
vif(fd_4) # brilliant
summary(fd_4) # next we can get rid of PC1*age group

fd_5 <- lmer(degreeRelative ~ PC1 + age_group + season + poly(PC2, 2) + mnPPD + year + (1|Nili_id), data = flight)
check_model(fd_5) # Looks good! candidate.
summary(fd_5) # looking nice. mnPPD and age group are still non-significant.

# remove mnPPD
fd_6 <- lmer(degreeRelative ~ PC1 + age_group + season + poly(PC2, 2) + year + (1|Nili_id), data = flight)
check_model(fd_6) # Looks good! candidate.
summary(fd_6) # remove age group

# remove age group
fd_7 <- lmer(degreeRelative ~ PC1 + season + poly(PC2, 2) + year + (1|Nili_id), data = flight)
check_model(fd_7) # Looks good! candidate.
summary(fd_7) # nice!

# Compare performance of only the models with a reasonable fit
compare_performance(fd_noint, fd_4, fd_5, fd_6, fd_7, rank = T) # fd_7 wins here: degreeRelative ~ PC1 + season + poly(PC2, 2) + year + (1|Nili_id)

## Strength ------------------------------------------------------------------
# Theoretically we might want to do the same model selection for strength as for degree (same models in same order), but just to see, I'm going to do the process as if it were its own thing.
# I think gaussian is still reasonable here:
flight %>% ggplot(aes(x = strengthRelative))+geom_histogram() # yeah that's decently normal-shaped.

fs_noint <- lmer(strengthRelative ~ PC1 + poly(PC2, 2) + age_group + season + year +  mnPPD + (1|Nili_id), data = flight)
summary(fs_noint) # oh weird, no season effect here! 
check_model(fs_noint) # oh wow this actually looks perfect!!! Candidate.
# What does DHARMa think?
so_fs_noint <- simulateResiduals(fittedModel = fs_noint, plot = F)
plot(so_fs_noint) # holy moly that's so nice! Yesssss
# No evidence of multicollinearity.
testDispersion(fs_noint) # beautiful
plotResiduals(fs_noint, form = flight$season) # beautiful
plotResiduals(fs_noint, form = flight$PC1) # meh but not terrible
plotResiduals(fs_noint, form = flight$PC2) # wait this one is actually good now that we included the quadratic term for PC2!
plotResiduals(fs_noint, form = flight$age_group) # not terrible
plotResiduals(fs_noint, form = flight$year) # yay!
plot_model(fs_noint, type = "pred", term = "PC2 [all]", show.data = TRUE) # Much better fit than the linear.
plot_model(fs_noint, type = "pred", term = "PC1", show.data = TRUE) # Yuck.

fs_max <- lmer(strengthRelative ~ PC1*age_group*season + poly(PC2, 2)*age_group*season + mnPPD*season + year + (1|Nili_id), data = flight)
check_model(fs_max) # really bad VIFs. Not candidate.
vif(fs_max) # start by removing age*season*PC2

fs_2 <- lmer(strengthRelative ~ PC1*age_group*season + poly(PC2, 2) + mnPPD + year + (1|Nili_id), data = flight)
check_model(fs_2) # VIFs slightly improved but still really bad. Not candidate.
vif(fs_2) # PC1*season is the worst
summary(fs_2)

fs_3 <- lmer(strengthRelative ~ PC1*age_group + season + poly(PC2, 2) + mnPPD + year + (1|Nili_id), data = flight)
check_model(fs_3) # this looks better! Candidate.
vif(fs_3) # golden
summary(fs_3) # can remove mnPPD

fs_4 <- lmer(strengthRelative ~ PC1*age_group + season + poly(PC2, 2) + year + (1|Nili_id), data = flight)
check_model(fs_4) # candidate.
summary(fs_4) # can remove mnPPD

# Remove season
fs_5 <- lmer(strengthRelative ~ PC1 + age_group + poly(PC2, 2) + year + (1|Nili_id), data = flight)
check_model(fs_5) # candidate.
summary(fs_5)

# Compare performance of only the models with a reasonable fit
compare_performance(fs_noint, fs_3, fs_4, fs_5, rank = T) # fs_3 wins: strengthRelative ~ PC1*age_group + season + poly(PC2, 2) + mnPPD + year + (1|Nili_id)

## Strength by degree ------------------------------------------------------
fb_noint <- lmer(sbd ~ PC1 + PC2 + age_group + season + year +  mnPPD + (1|Nili_id), data = flight)
check_model(fb_noint) # Looks okay! Not ideal, but fine. Candidate.
# What does DHARMa think?
so_fb_noint <- simulateResiduals(fittedModel = fb_noint, plot = F)
plot(so_fb_noint) # weird. Less bad than degree, but worse than strength.
testDispersion(fb_noint) # beautiful
plotResiduals(fb_noint, form = flight$season) # ok
plotResiduals(fb_noint, form = flight$PC1) # not bad!
plotResiduals(fb_noint, form = flight$PC2) # nice!
plotResiduals(fb_noint, form = flight$age_group) # ok
plotResiduals(fb_noint, form = flight$year) # ok
plot_model(fb_noint, type = "pred", term = "PC2 [all]", show.data = TRUE) # looks fine
plot_model(fb_noint, type = "pred", term = "PC1 [all]", show.data = TRUE) # this one looks fine actually.

fb_max <- lmer(sbd ~ PC1*age_group*season + PC2*age_group*season + mnPPD*season + year + (1|Nili_id), data = flight)
check_model(fb_max) # aaaaah. Not candidate.
vif(fb_max) # worst is season*mnPPD

fb_2 <- lmer(sbd ~ PC1*age_group*season + PC2*age_group*season + mnPPD + year + (1|Nili_id), data = flight)
check_model(fb_2) # still bad
vif(fb_2) # now the worst is PC1*season

fb_3 <- lmer(sbd ~ PC1*age_group + PC2*age_group*season + mnPPD + year + (1|Nili_id), data = flight)
check_model(fb_3) # still bad but getting better
vif(fb_3) # age*PC2*season is bad

fb_4 <- lmer(sbd ~ PC1*age_group + PC2*age_group + PC2*season + mnPPD + year + (1|Nili_id), data = flight)
check_model(fb_4) # moderately elevated VIF but not terrible. candidate.
vif(fb_4) # the worst is PC2*season

fb_5 <- lmer(sbd ~ PC1*age_group + PC2*age_group + season + mnPPD + year + (1|Nili_id), data = flight)
check_model(fb_5) # candidate
vif(fb_5) # golden!
summary(fb_5) # next thing to remove is PC1*age group

fb_6 <- lmer(sbd ~ PC1+ PC2*age_group + season + mnPPD + year + (1|Nili_id), data = flight)
check_model(fb_6) # fine
summary(fb_6) # next we would remove PC2 but I don't want to do that. I think we can stop.

compare_performance(fb_noint, fb_4, fb_5, fb_6, rank = T) # fb_6 wins: sbd ~ PC1+ PC2*age_group + season + mnPPD + year + (1|Nili_id)


# Lingering questions
# 0. When I do performance::compare_performance, should I only include models that have already been deemed to fit the data fairly well, or should I include all of them? I can see arguments either way. Unclear to me whether compare_performance also takes into account the model diagnostics when evaluating performance, or if it only computes AIC and assumes you've already decided the diagnostics are met. I think it's the latter, but I'm not sure.
# - Correct, but if it's borderline then you can include it.
# 0.5. Why does DHARMa sometimes show bad diagnostic plots even when check_model shows that things are fine?
# -Conner doesn't know, but thinks that the DHARMa thing isn't a core assumption
# 1. If I see that there are relationships between predictor variables in the pre-modeling plots, such as the age*PC boxplots I made at the top, does that mean I *should* or *shouldn't* include an interaction term? I thought it meant I should, but those all ended up needing to get removed, and now it's occurring to me that maybe that just means that they were correlated at first. Case in point: mean ppd * season always ends up having a super high VIF.
# 2. Should I use the same model selection process or an independent one for each of the 9 models I need to make?
# - can use a different process each time
# 3. Should I refrain from removing the PC's from the model if they are the variables of interest, or is an absence of a significant effect a result in itself?
# 4. If I use different model selection processes (or the same one), what is the best way to compare effect sizes and which model is chosen between different models?
# - part r2 allows you to partition the marginal and conditional r2 for each of the models--can use that to compare
# 5. These effect sizes seem really small... but maybe that's just because the relative degree and relative strength values are so small? Would multiply by the number of vultures for each season to think about the difference in real terms.
# 6. Just generally, a little confused as to which things constitute results that I can talk about: 1) which models fit the data decently well as per the model checks, 2) which variables are retained in the best-fitting model of the candidates, and 3) the actual effect sizes and directions and p values of the best-fitting model. 
# 7. If I decide that adding a quadratic term is good, does that then mean that I need to re-fit all of the models with the quadratic term included?
# - just decide to add it

# -google the predicted vs. residuals plot and figure out what it actually means. 