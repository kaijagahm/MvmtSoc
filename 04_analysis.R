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

# Set ggplot theme to classic
theme_set(theme_classic())

# Set season colors
seasonColors <- c("#2FF8CA", "#CA2FF8", "#F8CA2F")

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
plot_model(fd_noint, type = "pred", term = "PC2", show.data = TRUE) # I don't like this linear fit, but I don't think the quadratic term is very interpretable, and I can't justify it (at least not now)

fd_max <- lmer(degreeRelative ~ PC1*age_group*season + PC2*age_group*season + mnPPD*season + year + (1|Nili_id), data = flight)
check_model(fd_max) # oof those VIFs are baaad. Not candidate
summary(fd_max) # let's start by taking out season*mnPPD
vif(fd_max)

fd_2 <- lmer(degreeRelative ~ PC1*age_group*season + PC2*age_group*season + mnPPD + year + (1|Nili_id), data = flight)
check_model(fd_2) # VIFs slightly improved but still really bad. Not candidate.
vif(fd_2) #PC1*season is the worst, so let's remove that.

fd_3 <- lmer(degreeRelative ~ PC1*age_group + PC2*age_group*season + mnPPD + year + (1|Nili_id), data = flight)
check_model(fd_3) # VIFs still too high. Not candidate.
vif(fd_3) #age*PC2*season is the worst, so we'll remove that next

fd_4 <- lmer(degreeRelative ~ PC1*age_group + PC2*season + PC2*age_group + mnPPD + year + (1|Nili_id), data = flight)
check_model(fd_4) # Borderline, but candidate.
vif(fd_4) # Highest now is PC2*season

fd_4.5 <- lmer(degreeRelative ~ PC1*age_group + PC2*age_group + season + mnPPD + year + (1|Nili_id), data = flight)
check_model(fd_4.5) # nice, candidate.
vif(fd_4.5) #these all look good now.
summary(fd_4.5) # next to go will be the interactions between age group and the PCs, but also mnPPD isn't significant and I don't really like it, so let's get rid of that first.

fd_5 <- lmer(degreeRelative ~ PC1*age_group + PC2*age_group + season + year + (1|Nili_id), data = flight)
check_model(fd_5) # candidate
summary(fd_5)

# Still not loving those interactions... remove the PC2 one first because reasons.
fd_6 <- lmer(degreeRelative ~ PC1*age_group + PC2 + season + year + (1|Nili_id), data = flight)
check_model(fd_6) # candidate
summary(fd_6) # PC1*age_group is still bad. 

fd_7 <- lmer(degreeRelative ~ PC1 + age_group + PC2 + season + year + (1|Nili_id), data = flight)
check_model(fd_7) # candidate
summary(fd_7) # I guess we can remove age_group...

fd_8 <- lmer(degreeRelative ~ PC1 + PC2 + season + year + (1|Nili_id), data = flight)
check_model(fd_8) # candidate
summary(fd_8) # okay, going to stop there because I don't want to remove the PCs.

# what about... do we want a season*year effect??
fd_9 <- lmer(degreeRelative ~ PC1 + PC2 + season*year + (1|Nili_id), data = flight)
check_model(fd_9) # ah, this becomes "rank-deficient", which I think is because we don't have all three seasons for all of the years. So let's not do this. Not candidate.

# Compare performance of only the models with a reasonable fit
compare_performance(fd_noint, fd_4, fd_4.5, fd_5, fd_6, fd_7, fd_8, rank = T) # fd_8: degreeRelative ~ PC1 + PC2 + season + year + (1|Nili_id)
fd_mod <- fd_8

## Strength ------------------------------------------------------------------
# I think gaussian is still reasonable here:
flight %>% ggplot(aes(x = strengthRelative))+geom_histogram() # yeah that's decently normal-shaped. Even better than degree, I think. And according to Marta, if I want to compare between models, I should use the same type of model for each of them.

fs_noint <- lmer(strengthRelative ~ PC1 + PC2 + age_group + season + year +  mnPPD + (1|Nili_id), data = flight)
summary(fs_noint) # oh weird, no season effect here! 
check_model(fs_noint) # oh wow this actually looks perfect!!! Candidate.
# What does DHARMa think?
so_fs_noint <- simulateResiduals(fittedModel = fs_noint, plot = F)
plot(so_fs_noint) # holy moly that's so nice! Yesssss
# No evidence of multicollinearity.
testDispersion(fs_noint) # beautiful
plotResiduals(fs_noint, form = flight$season) # beautiful
plotResiduals(fs_noint, form = flight$PC1) # meh but not terrible
plotResiduals(fs_noint, form = flight$PC2) # kind of bad but could just be low sample size?
plotResiduals(fs_noint, form = flight$age_group) # not terrible
plotResiduals(fs_noint, form = flight$year) # yay!
plot_model(fs_noint, type = "pred", term = "PC2 [all]", show.data = TRUE) # this is gross but I've decided that a quadratic term will be too hard to deal with
plot_model(fs_noint, type = "pred", term = "PC1", show.data = TRUE) # Yuck.

fs_max <- lmer(strengthRelative ~ PC1*age_group*season + PC2*age_group*season + mnPPD*season + year + (1|Nili_id), data = flight)
check_model(fs_max) # really bad VIFs. Not candidate.
vif(fs_max) # start by removing season*mnPPD

fs_2 <- lmer(strengthRelative ~ PC1*age_group*season + PC2*age_group*season + mnPPD + year + (1|Nili_id), data = flight)
check_model(fs_2) # still really high VIFs. Not candidate.
vif(fs_2) # now PC1*season is bad

fs_3 <- lmer(strengthRelative ~ PC1*age_group + PC2*age_group*season + mnPPD + year + (1|Nili_id), data = flight)
check_model(fs_3) # still too high but getting better
vif(fs_3) # next problem is the three-way interaction with PC2

fs_4 <- lmer(strengthRelative ~ PC1*age_group + PC2*age_group + PC2*season + mnPPD + year + (1|Nili_id), data = flight)
check_model(fs_4) # much better but borderline... candidate, I guess.
vif(fs_4) # get rid of PC2*season next

fs_5 <- lmer(strengthRelative ~ PC1*age_group + PC2*age_group + mnPPD + season + year + (1|Nili_id), data = flight)
check_model(fs_5) # ok! candidate.
vif(fs_5) # these all look nice now.
summary(fs_5) # no effect of season, mnPPD, and also no effect of PC1*age group. Let's remove mnPPD first because it's the least interpretable.

fs_6 <- lmer(strengthRelative ~ PC1*age_group + PC2*age_group + season + year + (1|Nili_id), data = flight)
check_model(fs_6) # candidate
summary(fs_6) # now we can remove PC1*age group

fs_7 <- lmer(strengthRelative ~ PC1 + PC2*age_group + season + year + (1|Nili_id), data = flight)
check_model(fs_7) # looks lovely. candidate.
summary(fs_7) # still really no effect of season! Surprising.

fs_8 <- lmer(strengthRelative ~ PC1 + PC2*age_group + year + (1|Nili_id), data = flight)
check_model(fs_8) # candidate
summary(fs_8) 

# Compare performance of only the models with a reasonable fit
compare_performance(fs_noint, fs_4, fs_5, fs_6, fs_7, fs_8, rank = T) # fs_8 wins: strengthRelative ~ PC1 + PC2*age_group + year + (1|Nili_id). Second place is fs_7, strengthRelative ~ PC1 + PC2*age_group + season + year + (1|Nili_id) (includes season), so we should use that one if we don't want to remove season, which I think I don't.
fs_mod <- fs_7

## Evenness ------------------------------------------------------
fb_noint <- lmer(evenness ~ PC1 + PC2 + age_group + season + year +  mnPPD + (1|Nili_id), data = flight)

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

# Marginal effect plots ---------------------------------------------------
(formula(fd_mod))
(formula(fs_mod))
(formula(fb_mod))

# to aid in the interpretation, let's look at the variable contributions again
load("data/contrib.Rda")
min(flight$n)
max(flight$n)
# population size ranges from 95 to 262.

## Degree ------------------------------------------------------------------
plot_model(fd_mod) # Estimates are quite small. Largest effect is year (negative effect). Largest positive effect is season (summer).
summary(fd_mod)
# PC1: non-signif
# PC2: significant but tiny tiny tiny.
# season: marginally significant, small effect.
# year: significant, small effects.
# No interactions retained in the model.
plot_model(fd_mod, type = "pred", terms = "PC2 [all]", col = "white")+
  theme(panel.background = element_rect(fill = "#595959"),
        plot.background = element_rect(fill = "#595959"),
        text = element_text(color = "white", size = 15),
        axis.text = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        axis.line = element_line(color = "white"))+
  ylab("Degree (normalized)")+
  ggtitle("")+
  theme(text = element_text(size = 18))
plot_model(fd_mod, type = "pred", terms = "season [all]", col = "white")+
  theme(panel.background = element_rect(fill = "#595959"),
        plot.background = element_rect(fill = "#595959"),
        text = element_text(color = "white", size = 15),
        axis.text = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        axis.line = element_line(color = "white"))+
  ylab("Degree (normalized)")+
  ggtitle("")+
  theme(text = element_text(size = 18))
# to translate this into population size--Looks like the most different seasons differ by about 0.03 in degreeRelative. Multiply by 95 (lowest bound for pop size), this is a difference of 2.85 vultures. Multiply by 90 (highest bound for pop size), this is a difference of 7.86 vultures. So, this is actually pretty biologically significant!
# Interpretability makes me want to just include population size in the model...
plot_model(fd_mod, type = "pred", terms = "year [all]") # 2021 significantly lower than the others, but only by a small amount.
plot_model(fd_mod, type = "pred", terms = c("year [all]", "season"), 
           colors = seasonColors) # there's no season*year effect included in the model, so the fact that the differences between the different points are the same in each year is not an important takeaway from this plot. Basically just visualizing what absolute values these differences would translate to.

## Strength ------------------------------------------------------------------
summary(fs_mod)
# PC1: significant but tiny tiny.
# PC2: significant but tiny tiny.
# age group: significant but tiny tiny.
# season: not significant, and also very small.
# year: significant but tiny tiny.
# PC2*age: significant but tiny tiny.

# Before we despair of biological meaning, this is relative strength. Strength values were already small, and we divided them by n. Let's take the example of the PC1 effect: 3.764e-04. Multiply that by the population sizes: You get a lower bound of 0.036 and an upper bound of  0.099. That's the effect size, so the amount of change in strength due to those effects. Those are strength values (rather than relative strength values), and strength is an SRI calculation. So I don't love how uninterpretable these are...

# PC1 ranges from -6 to 6, so a range of 12 units. At max, that would translate into 0.099*12 = 1.188, which is not a large strength difference, but it's something...

# I still wish I could remove at least one of these layers of abstraction in order to make this actually mean something.

plot_model(fs_mod) # tiny effects, basically negligible
plot_model(fs_mod, type = "pred", terms = "PC1 [all]", col = "white")+
  theme(panel.background = element_rect(fill = "#595959"),
        plot.background = element_rect(fill = "#595959"),
        text = element_text(color = "white", size = 15),
        axis.text = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        axis.line = element_line(color = "white"))+
  ylab("Strength (normalized)")+
  ggtitle("")+
  theme(text = element_text(size = 18)) # significant, but really tiny if you look at the y value.
plot_model(fs_mod, type = "pred", terms = "PC2 [all]", col = "white")+
  theme(panel.background = element_rect(fill = "#595959"),
        plot.background = element_rect(fill = "#595959"),
        text = element_text(color = "white", size = 15),
        axis.text = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        axis.line = element_line(color = "white"))+
  ylab("Strength (normalized)")+
  ggtitle("")+
  theme(text = element_text(size = 18)) # same--significant increase, but really really small values.

plot_model(fs_mod, type = "pred", terms = "season [all]", col = "white")+
  theme(panel.background = element_rect(fill = "#595959"),
        plot.background = element_rect(fill = "#595959"),
        text = element_text(color = "white", size = 15),
        axis.text = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        axis.line = element_line(color = "white"))+
  ylab("Strength (normalized)")+
  ggtitle("")+
  theme(text = element_text(size = 18)) # no significant season differences
plot_model(fs_mod, type = "pred", terms = c("PC2 [all]", "age_group")) # oh this is actually cool! But still really small effects.
plot_model(fs_mod, type = "pred", terms = "age_group") # sig but small differences
plot_model(fs_mod, type = "pred", terms = "year") # once again, 2021 is lower (but the differences here are really small.)

## Strength by degree ------------------------------------------------------
summary(fb_mod)
# Unlike the others, strength by degree is directly interpretable as the average edge weight (SRI value).
hist(flight$sbd) # ranges from 0.02 to 0.14 ish.
plot_model(fb_mod) # also tiny tiny effects.

# PC1 significant but really small
# PC2 non-signif
# Age group significant but small
# season significant but small
# ppd significant but small
# year significant but small
# pc2*age effect significant but small.

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

