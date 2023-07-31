library(car) # for p-values in mixed models?
# In lieu of tidyverse
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)
library(dplyr)
library(tibble)
library(purrr)
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

load("data/linked.Rda")

# Set ggplot theme to classic
theme_set(theme_classic())

# Set season colors
seasonColors <- c("#2FF8CA", "#CA2FF8", "#F8CA2F")
situationColors <- c("dodgerblue2", "olivedrab3", "gold")

# Check that the response variable distributions look ok--nothing too weird here.
linked %>%
  mutate(season = factor(season, levels = c("breeding", "summer", "fall"))) %>%
  ggplot(aes(x = evenness))+
  geom_density(aes(col = season), linewidth = 1.5)+
  facet_wrap(~type, scales = "free_y")+
  scale_color_manual(name = "Season", values = seasonColors)+
  xlab("Evenness")+
  ylab("")+
  theme(text = element_text(size = 16))

linked %>%
  mutate(season = factor(season, levels = c("breeding", "summer", "fall"))) %>%
  ggplot(aes(x = degreeRelative))+
  geom_density(aes(col = season), linewidth = 1.5)+
  facet_wrap(~type, scales = "free_y")+
  scale_color_manual(name = "Season", values = seasonColors)+
  xlab("Degree (normalized)")+
  ylab("")+
  theme(text = element_text(size = 16))

linked %>%
  mutate(season = factor(season, levels = c("breeding", "summer", "fall"))) %>%
  ggplot(aes(x = degree))+
  geom_density(aes(col = season), linewidth = 1.5)+
  facet_wrap(~type*year, scales = "free_y")+
  scale_color_manual(name = "Season", values = seasonColors)+
  xlab("Degree")+
  ylab("")+
  theme(text = element_text(size = 16))


linked %>%
  mutate(season = factor(season, levels = c("breeding", "summer", "fall"))) %>%
  ggplot(aes(x = strengthRelative))+
  geom_density(aes(col = season), linewidth = 1.5)+
  facet_wrap(~type, scales = "free_y")+
  scale_color_manual(name = "Season", values = seasonColors)+
  xlab("Strength (normalized)")+
  ylab("")+
  theme(text = element_text(size = 16))

# Here's an interesting reference article for some modeling stuff: https://biol607.github.io/lab/12_gzlm.html
linked <- linked %>%
  mutate(season = factor(season, levels = c("breeding", "summer", "fall")))

# Remove individuals that don't have movement information
colSums(is.na(linked)) # lots of NA's
linked <- linked %>%
  filter(!is.na(PC1)) # note that because I joined the sex/age information along with the movement information (instead of along with the social information), if I ever want to *not* filter out those individuals, I'll have to go back and add sex/age info for the additional birds. Don't need this for now, so I'm skipping it. 
colSums(is.na(linked)) # okay now we have no more NA's except in sex. Yay!

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

# Question from vulture meeting--should I care about high VIF values for interaction terms?
# No, they're fine: https://stats.stackexchange.com/questions/52856/vif-values-and-interactions-in-multiple-regression, https://stats.stackexchange.com/questions/274320/how-to-deal-with-interaction-terms-vif-score.
# So, I think I need to rethink the model selection here. Maybe do it just based on significance?

# Models with situation as a factor ---------------------------------------
linked <- linked %>%
  mutate(situ = case_when(type == "flight" ~ "Fl", 
                          type == "feeding" ~ "Fe",
                          type == "roosting" ~ "Ro")) %>%
  mutate(seasonUnique = paste(year, season, sep = "_"))
table(linked$situ) # as expected, we have more data for the roost network than for feeding and flight.

## Degree ------------------------------------------------------------------
# Trying a binomial model
# Instructions on how to specify the formula here: https://stats.stackexchange.com/questions/362911/lme4-problems-trying-to-run-negative-binomial-glmer-nb
#deg_noint <- glmer.nb(cbind(degree, n-degree) ~ situ + I(PC1^2) + I(PC2^2) + age_group + season + (1|year) + (1|Nili_id), data = linked, weights = n)
arm::binnedplot(fitted(deg_noint),residuals(deg_noint))
plot(DHARMa::simulateResiduals(deg_noint), pch=".") # this looks fairly bad, but I don't really know what to do about it. Candidate.
summary(deg_noint)
testDispersion(deg_noint)

# Kenji's residual plots ----------------------------------
pred <- predict(deg_noint, type = "response")
# Plot predicted probs against observed probs
plot(x = pred, y = linked$degree/linked$n, xlab = "expected", ylab = "observed")
abline(a = 0, b = 1, lty = 2)

# Plot residuals against predicted probs
plot(x = pred, y = pred - linked$degree/linked$n, xlab = "expected", ylab = "residuals")
abline(h = 0, lty = 2)

deg_max <- glmer(cbind(degree, n-degree) ~ situ*PC1*age_group + situ*PC1*season + situ*PC2*age_group + situ*PC2*season + PC1*age_group*season + PC2*age_group*season + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit"))
arm::binnedplot(fitted(deg_max),residuals(deg_max)) # this is actually a lot less terrible!
plot(DHARMa::simulateResiduals(deg_max), pch=".") # nearly unidentifiable, failed to converge. So not candidate. But actually getting better. Not candidate.
summary(deg_max) # can remove PC1*age_group*season

deg_2 <- glmer(cbind(degree, n-degree) ~ situ*PC1*age_group + situ*PC1*season + situ*PC2*age_group + situ*PC2*season + PC1*age_group + PC1*season + age_group*season + PC2*age_group*season + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit")) # failed to converge, not candidate
arm::binnedplot(fitted(deg_max),residuals(deg_2)) # not great but not awful
plot(DHARMa::simulateResiduals(deg_2), pch=".")
summary(deg_2) # seems like we can mostly remove situ*PC1*season

deg_3 <- glmer(cbind(degree, n-degree) ~ situ*PC1*age_group + situ*PC1 + situ*season + situ*PC2*age_group + situ*PC2*season + PC1*age_group + PC1*season + age_group*season + PC2*age_group*season + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit")) # failed to converge, not candidate
summary(deg_3) #let's remove situ*age*PC2 next

deg_4 <- glmer(cbind(degree, n-degree) ~ situ*PC1*age_group + situ*PC1 + situ*season + situ*PC2 + PC2*age_group + situ*age_group + situ*PC2*season + PC1*age_group + PC1*season + age_group*season + PC2*age_group*season + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit")) # failed to converge 
summary(deg_4) # I guess next I'll go ahead and remove situ*season*PC2?

deg_5 <- glmer(cbind(degree, n-degree) ~ situ*PC1*age_group + situ*PC1 + situ*season + situ*PC2 + PC2*age_group + situ*age_group + PC2*season + PC1*age_group + PC1*season + age_group*season + PC2*age_group*season + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit")) # failed to converge, again. I guess it's time to remove the rest of the three-way interaction terms.

deg_6 <- glmer(cbind(degree, n-degree) ~ situ*PC1 + PC1*age_group + situ*age_group + situ*season + situ*PC2 + PC2*age_group + PC2*season + PC1*season + age_group*season + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit")) # this one actually converges! hooray!
arm::binnedplot(fitted(deg_6),residuals(deg_6)) # not great but not awful. Candidate.
plot(DHARMa::simulateResiduals(deg_6), pch=".")
summary(deg_6) # pretty much everything else is significant. maybe we could try getting rid of PC1*season?

deg_7 <- glmer(cbind(degree, n-degree) ~ situ*PC1 + PC1*age_group + situ*age_group + situ*season + situ*PC2 + PC2*age_group + PC2*season + age_group*season + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit")) # back to not converging... Not candidate.
summary(deg_7) # just based on biology, let's next remove situation by age, I guess?

deg_8 <- glmer(cbind(degree, n-degree) ~ situ*PC1 + PC1*age_group + situ*season + situ*PC2 + PC2*age_group + PC2*season + age_group*season + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit")) # converges, candidate
summary(deg_8) # next could remove season*PC2 i guess?

deg_9 <- glmer(cbind(degree, n-degree) ~ situ*PC1 + PC1*age_group + situ*season + situ*PC2 + PC2*age_group + age_group*season + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit")) # does not converge, not candidate

# Maybe instead I should go up from the base model. Let's start with a season effect for each of PC1 and PC2. 
deg_10 <- glmer(cbind(degree, n-degree) ~ situ + season*PC1 + PC2 + season + age_group + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit"))
summary(deg_10) # this doesn't seem to have added anything. Not candidate. Instead, let's try season*PC2.

deg_11 <- glmer(cbind(degree, n-degree) ~ situ + season*PC2 + PC1 + season + age_group + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit"))
summary(deg_11) # this is potentially useful!

deg_12 <- glmer(cbind(degree, n-degree) ~ situ*season + season*PC2 + PC1 + age_group + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit"))
summary(deg_12) # makes the season*PC2 effect go away. Let's try removing it

deg_13 <- glmer(cbind(degree, n-degree) ~ situ*season + PC2 + PC1 + age_group + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit"))
summary(deg_13) # that looks better! What about age*season? We have reason to believe that would be important. 

deg_14 <- glmer(cbind(degree, n-degree) ~ situ*season + PC2 + PC1 + age_group*season + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit"))
summary(deg_14) # ahh yes this is looking good. What about adding an age effect with PC1 or PC2?

deg_15 <- glmer(cbind(degree, n-degree) ~ situ*season + PC2 + age_group*PC1 + age_group*season + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit"))
summary(deg_15) # still looking good.

deg_16 <- glmer(cbind(degree, n-degree) ~ situ*season + age_group*PC2 + age_group*PC1 + age_group*season + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit"))
summary(deg_16) # Ok, what about situation by PC1 and/or PC2?

deg_17 <- glmer(cbind(degree, n-degree) ~ situ*PC1 + situ*season + age_group*PC2 + age_group*PC1 + age_group*season + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit"))
summary(deg_17) # failed to converge. What about PC2 instead?

deg_18 <- glmer(cbind(degree, n-degree) ~ situ*PC2 + situ*season + age_group*PC2 + age_group*PC1 + age_group*season + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit"))
summary(deg_18) # okay, not too bad.

compare_performance(deg_18, deg_16, deg_15, deg_14, deg_13, deg_11, deg_8, deg_6, deg_noint, rank = T)
# we get deg_6, followed by deg_14. What do these have in common?
# deg_6: situ*PC1 + PC1*age_group + situ*age_group + situ*season + situ*PC2 + PC2*age_group + PC2*season + PC1*season + age_group*season + (1|year) + (1|Nili_id)
# deg_14: situ*season + PC2 + PC1 + age_group*season + (1|year) + (1|Nili_id)
# interesting! either a really complicated model with almost all possible interactions, or a fairly simple one with only season interactions. Third place is deg_18, which has an intermediate number of interactions.

deg_mod <- deg_6

# Now, how do I need to transform the coefficients in order to interpret them?
# Interpretation of coefficients: see here. https://stats.stackexchange.com/questions/361529/why-are-exponentiated-logistic-regression-coefficients-considered-odds-ratios
# "the original coefficients describe a difference in the log odds for two units who differ by 1 in the predictor. E.g., for a coefficient on 𝑋of 5, we can say that the difference in log odds between two units who differ on 𝑋 by 1 is 5."
# let's look at an example from our model
summary(deg_mod) # let's look at PC2, which has a coefficient of 0.165. Putting that into the same terms, "the difference in log odds between two units who differ on PC2 by 1 is 0.165".

#https://stats.stackexchange.com/questions/444797/interpreting-a-generalised-linear-mixed-model-with-binomial-data has a good example of interpreting the coefficients (cockroach example)
# also worth checking out next: https://stats.stackexchange.com/questions/365907/interpretation-of-fixed-effects-from-mixed-effect-logistic-regression and the linked webpage.


# 2023-06-05 going back to gaussian for now
degree_noint <- lmer(degree ~ situ + PC1 + PC2 + age_group + season + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(degree_noint)
check_model(degree_noint) # looks okay, candidate. No correlated predictors.
plot(DHARMa::simulateResiduals(degree_noint), pch=".")

degree_max <- lmer(degree ~ situ*PC1*age_group + situ*PC1*season + situ*PC2*age_group + situ*PC2*season + PC1*age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked)
check_model(degree_max) # still looks basically ok, except for the vif's, which we can ignore
summary(degree_max) # okay, we can remove most of these. Going to replace them with two-way interactions and then remove duplicates.

degree_2 <- lmer(degree ~ situ*PC1*age_group + situ*PC1 + situ*season + PC1*season + situ*PC2 + situ*age_group + PC2*age_group + situ*PC2 + PC2*season + PC1*age_group + age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(degree_2) #things that can be removed: situ*PC1*age

degree_3 <- lmer(degree ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC1*season + situ*PC2 + PC2*age_group + PC2*season + age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(degree_3) # now would like to remove some two-way interactions, but can't do that until we remove more three-ways. I don't feel that there's much justification in keeping situ*age_group*season with those marginal values, so let's start by removing those.

degree_4 <- lmer(degree ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC1*season + situ*PC2 + PC2*age_group + PC2*season + age_group*season + PC2*age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(degree_4) # next would like to remove age_group*PC2 or season*PC2, but can't do that without removing the 3-way interaction first.

degree_5 <- lmer(degree ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC1*season + situ*PC2 + PC2*age_group + PC2*season + age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(degree_5) # Ok, this free us up to remove some other things, starting with season*PC2.

degree_6 <- lmer(degree ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC1*season + situ*PC2 + PC2*age_group + age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(degree_6) # hmm not entirely clear what to remove next--let's remove PC1*season first I guess because the effect sizes are smaller?

degree_7 <- lmer(degree ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + situ*PC2 + PC2*age_group + age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(degree_7) # now I guess we could try getting rid of situ*PC2, but probably tbh we don't want to do either this or the previous one.

degree_8 <- lmer(degree ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC2*age_group + age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(degree_8) # I guess we could also remove situ*age...

degree_9 <- lmer(degree ~ situ*PC1 + PC1*age_group + situ*season + PC2*age_group + age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(degree_9) # I don't want to remove situ*season because that effect size on summer is HUGE. So let's stop here.

compare_performance(degree_noint, degree_max, degree_2, degree_3, degree_4, degree_5, degree_6, degree_7, degree_8, degree_9, rank = T)

degree_mod <- degree_3 # all righty, nice and consistent!
check_model(degree_mod) # that looks pretty glorious, except for the VIF's, which we can probably ignore anyway.

## Strength ------------------------------------------------------------------
# strength_noint <- lmer(strength ~ situ + PC1 + PC2 + age_group + season + (1|seasonUnique) + (1|Nili_id), data = linked)
# summary(strength_noint)
# check_model(strength_noint) # this looks pretty awful, especially below 0, which isn't really a region where we should be doing inference anyway. But we see a significant spike near 0 so maybe should use a gamma?
# 
# #I could try a gamma and add a tiny bit to the zeroes.
# linked %>% filter(strength < 1) %>% pull(strength) %>% hist() # okay, we have relatively few true zeroes.
# linked %>% filter(strength < 0.2) %>% pull(strength) %>% hist() # okay, we have relatively few true zeroes. How many are there?
# linked %>% filter(strength == 0) %>% nrow() # only 4! Okay, I definitely don't want to do a hurdle model just for 4 zeroes. Let's just make them very very small, like 0.000001.
# linked_s <- linked %>%
#   filter(strength != 0) %>%
#   mutate(logstrength = log(strength))
# 
# strength_noint <- lmer(logstrength ~ situ + PC1 + PC2 + age_group + season + (1|seasonUnique) + (1|Nili_id), data = linked_s)
# check_model(strength_noint) # this looks really good actually! surprisingly good. Damn.
# 
# strength_max <- lmer(logstrength ~ situ*PC1*age_group + situ*PC1*season + situ*PC2*age_group + situ*PC2*season + PC1*age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked_s)
# check_model(strength_max) # nice!
# summary(strength_max) # can get rid of PC1*age_group*season and PC2*situ*season
# 
# strength_2 <- lmer(logstrength ~ situ*PC1*age_group + situ*PC1*season + situ*PC2*age_group + situ*PC2 + situ*season + PC2*season + PC1*age_group + PC1*season + age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked_s)
# summary(strength_2) # looks like we can reasonably remove situ*PC1*season
# 
# strength_3 <- lmer(logstrength ~ situ*PC1*age_group + situ*PC1 + situ*season + PC1*season + situ*PC2*age_group + situ*PC2 + PC2*season + PC1*age_group + age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked_s)
# summary(strength_3) # ok now we can remove situ*PC1*age_group
# 
# strength_4 <- lmer(logstrength ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC1*season + situ*PC2*age_group + situ*PC2 + PC2*season + age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked_s)
# summary(strength_4) #can get rid of situ*age_group*PC2
# 
# strength_5 <- lmer(logstrength ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC1*season + situ*PC2 + PC2*age_group + PC2*season + age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked_s)
# summary(strength_5) # can probably get rid of situ*age_group*season
# 
# strength_6 <- lmer(logstrength ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC1*season + situ*PC2 + PC2*age_group + PC2*season + age_group*season + PC2*age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked_s)
# summary(strength_6) # now can remove situ*PC2
# 
# strength_7 <- lmer(logstrength ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC1*season + PC2*age_group + PC2*season + age_group*season + PC2*age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked_s)
# summary(strength_7) # next would be to remove some of the 2-way interactions, but they are contained in the last remaining 3-way interaction, so let's get rid of that.
# 
# strength_8 <- lmer(logstrength ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC1*season + PC2*age_group + PC2*season + age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked_s)
# summary(strength_8) # ok now we can remove season*PC2
# 
# strength_9 <- lmer(logstrength ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC1*season + PC2*age_group + age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked_s)
# summary(strength_9) # this is as far as we can go
# 
# compare_performance(strength_noint, strength_max, strength_2, strength_3, strength_4, strength_5, strength_6, strength_7, strength_8, strength_9, rank = T)
# 
# strength_mod <- strength_2 # really??? that seems like so many interactions! But the next ones are 3, 4, with pretty big differences in AIC/BIC, so I don't really feel like I can easily jump to a different, much simpler one.

# Ok, for the sake of this conference, though, I'm going to go back to a gaussian.
strength_noint <- lmer(strength ~ PC1 + PC2 + situ + season + age_group + (1|seasonUnique) + (1|Nili_id), data = linked)

strength_max <- lmer(strength ~ PC1*situ*season + PC1*season*age_group + PC1*situ*age_group + PC2*situ*season + PC2*season*age_group + PC2*situ*age_group + situ*season*age_group + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(strength_max) # huh, some of these are actually marginally significant! PC1*season*age_group looks safe to remove.

strength_2 <- lmer(strength ~ PC1*situ*season + PC1*season + PC1*age_group + season*age_group + PC1*situ*age_group + PC2*situ*season + PC2*season*age_group + PC2*situ*age_group + situ*season*age_group + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(strength_2) # now can remove season*age_group*PC2

strength_3 <- lmer(strength ~ PC1*situ*season + PC1*season + PC1*age_group + season*age_group + PC1*situ*age_group + PC2*situ*season + PC2*season + PC2*age_group + PC2*situ*age_group + situ*season*age_group + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(strength_3) # I guess we can keep going... situ*season*age_group

strength_4 <- lmer(strength ~ PC1*situ*season + PC1*season + PC1*age_group + season*age_group + PC1*situ*age_group + PC2*situ*season + PC2*season + PC2*age_group + PC2*situ*age_group + situ*season + situ*age_group + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(strength_4) # situ*season*PC2 next

strength_5 <- lmer(strength ~ PC1*situ*season + PC1*season + PC1*age_group + season*age_group + PC1*situ*age_group + PC2*situ + PC2*season + PC2*age_group + PC2*situ*age_group + situ*season + situ*age_group + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(strength_5) # PC1*situ*age_group

strength_6 <- lmer(strength ~ PC1*situ*season + PC1*season + PC1*age_group + season*age_group + PC1*situ + PC2*situ + PC2*season + PC2*age_group + PC2*situ*age_group + situ*season + situ*age_group + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(strength_6) # situ*age_group*PC2

strength_7 <- lmer(strength ~ PC1*situ*season + PC1*season + PC1*age_group + season*age_group + PC1*situ + PC2*situ + PC2*season + PC2*age_group + situ*season + situ*age_group + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(strength_7) # that's as far as we should go

compare_performance(strength_noint, strength_max, strength_2, strength_3, strength_4, strength_5, strength_6, strength_7, rank = T)

strength_mod <- strength_3

## Evenness ----------------------------------------------------------------
# any(linked$evenness == 1 & !is.na(linked$evenness)) # no values of 1. Good! I assume no zeroes either?
# any(linked$evenness == 0 & !is.na(linked$evenness)) # excellent. So we will be able to use a beta.
# linked_e <- linked %>%
#   filter(!is.na(evenness)) # remove NA's, otherwise the model won't converge
# 
# # Let's try a gaussian first just to see
# evenness_noint <- lmer(evenness ~ situ + PC1 + PC2 + age_group + season + (1|seasonUnique) + (1|Nili_id), data = linked_e)
# check_model(evenness_noint) # that's really awful, isn't it. Maybe a beta distribution?
# 
# evenness_noint <- glmmTMB(evenness ~ situ + PC1 + PC2 + age_group + season + (1|seasonUnique) + (1|Nili_id), data = linked_e, family = beta_family(link = "logit")) # huh, apparently this warning is fine? "This warning occurs when the optimizer visits a region of parameter space that is invalid. It is not a problem as long as the optimizer has left that region of parameter space upon convergence, which is indicated by an absence of the model convergence warnings described above." From https://cran.r-project.org/web/packages/glmmTMB/vignettes/troubleshooting.html.
# check_model(evenness_noint) # oh! That's actually kind of fine! Overdispersed, but hmm okay for no I guess? Not sure how else to move forward. Later I'll investigate negative binomials or something but for now let's just do it.
# summary(evenness_noint)
# 
# evenness_max <- glmmTMB(evenness ~ situ*PC1*age_group + situ*PC1*season + situ*PC2*age_group + situ*PC2*season + PC1*age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked_e, family = beta_family(link = "logit")) # again with that warning, but I think we can just ignore it
# check_model(evenness_max) # yuck but okay i guess
# summary(evenness_max) # nice, can remove a lot of this
# 
# evenness_2 <- glmmTMB(evenness ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC1*season + situ*PC2 + PC2*age_group + PC2*season + age_group*season + PC2*age_group*season + situ*age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked_e, family = beta_family(link = "logit")) # again with that warning, but I think we can just ignore it
# summary(evenness_2) # next would be removing a lot of the smaller effects, but first would have to get rid of the three-way effects. Let's remove situ*age_group*season first.
# 
# evenness_3 <- glmmTMB(evenness ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC1*season + situ*PC2 + PC2*age_group + PC2*season + age_group*season + PC2*age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked_e, family = beta_family(link = "logit")) # again with that warning, but I think we can just ignore it
# summary(evenness_3) # ok, now we can get rid of situ*PC2 and PC1*season
# 
# evenness_4 <- glmmTMB(evenness ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC2*age_group + PC2*season + age_group*season + PC2*age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked_e, family = beta_family(link = "logit")) # warning again, ignore
# summary(evenness_4) # i guess for good measure let's remove the age*season*PC2 three-way effect?
# 
# evenness_5 <- glmmTMB(evenness ~ situ*PC1 + situ*age_group + PC1*age_group + situ*season + PC2*age_group + PC2*season + age_group*season + (1|seasonUnique) + (1|Nili_id), data = linked_e, family = beta_family(link = "logit")) # warning again, ignore
# summary(evenness_5) # everything else looks good! let's stop.
# 
# compare_performance(evenness_noint, evenness_max, evenness_2, evenness_3, evenness_4, evenness_5, rank = T)
# 
# evenness_mod <- evenness_2 #wow, yuck, a lot of three-way interactions I don't want to interpret. Second best is evenness_5, which is a lot simpler.

### For the conference, just doing a gaussian I guess.
evenness_noint <- lmer(evenness ~ situ + PC1 + PC2 + age_group + season + (1|seasonUnique) + (1|Nili_id), data = linked)
check_model(evenness_noint) # meh.

evenness_max <- lmer(evenness ~ PC1*season*situ + PC1*season*age_group + PC1*situ*age_group + PC2*season*situ + PC2*season*age_group + PC2*situ*age_group + season*situ*age_group + (1|seasonUnique) + (1|Nili_id), data = linked)
check_model(evenness_max)
summary(evenness_max) # can remove situ*age*PC2, season*situ*PC2, PC1*situ*age_group, PC1*season*age_group, PC1*season*situ

evenness_2 <- lmer(evenness ~ PC1*season + PC1*situ + season*situ + PC1*age_group + season*age_group + situ*age_group + PC2*season + PC2*situ + PC2*season*age_group + PC2*age_group + situ*age_group + season*situ*age_group + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(evenness_2) # want to remove more 2-way interactions but have to get rid of the three-ways first. Let's do season*age_group*PC2 first I guess

evenness_3 <- lmer(evenness ~ PC1*season + PC1*situ + season*situ + PC1*age_group + season*age_group + situ*age_group + PC2*season + PC2*situ + PC2*age_group + situ*age_group + season*situ*age_group + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(evenness_3) # now the two-ways become significant. Just for kicks, let's get rid of the last 3-way interaction.

evenness_4 <- lmer(evenness ~ PC1*season + PC1*situ + season*situ + PC1*age_group + season*age_group + situ*age_group + PC2*season + PC2*situ + PC2*age_group + situ*age_group + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(evenness_4) # can remove situ*PC2, PC1*season

evenness_5 <- lmer(evenness ~ PC1*situ + season*situ + PC1*age_group + season*age_group + situ*age_group + PC2*season + PC2*age_group + situ*age_group + (1|seasonUnique) + (1|Nili_id), data = linked)
summary(evenness_5) # time to stop!

compare_performance(evenness_noint, evenness_max, evenness_2, evenness_3, evenness_4, evenness_5, rank = T)

evenness_mod <- evenness_2

# Get model effects ----------------------------------------------------------
# We now have 3 models. Let's compile and tidy their outputs.
mods <- list("d" = degree_mod, "s" = strength_mod, "e" = evenness_mod)
outputs <- map(mods, broom.mixed::tidy) %>%
  purrr::list_rbind()

save(mods, file = "data/mods.Rda")
