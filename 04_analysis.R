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
flight <- linked %>%
  filter(type == "flight")
feeding <- linked %>%
  filter(type == "feeding")
roosting <- linked %>%
  filter(type == "roosting")

# Question from vulture meeting--should I care about high VIF values for interaction terms?
# No, they're fine: https://stats.stackexchange.com/questions/52856/vif-values-and-interactions-in-multiple-regression, https://stats.stackexchange.com/questions/274320/how-to-deal-with-interaction-terms-vif-score.
# So, I think I need to rethink the model selection here. Maybe do it just based on significance?

# Models with situation as a factor ---------------------------------------
linked <- linked %>%
  mutate(situ = case_when(type == "flight" ~ "Fl", 
                          type == "feeding" ~ "Fe",
                          type == "roosting" ~ "Ro"))

## Degree ------------------------------------------------------------------
# Trying a binomial model
deg <- glmer(cbind(degree, n) ~ situ + PC1 + PC2 + age_group + season + (1|year) + (1|Nili_id), data = linked, family = binomial(link = "logit"))
check_model(deg) # oh my god this looks absolutely awful...

degree_noint <- lmer(degreeRelative ~ situ + PC1 + PC2 + age_group + season + year + (1|Nili_id), data = linked) 
summary(degree_noint)
performance::check_model(degree_noint) # This isn't great, but it's not terrible either. Candidate.
testDispersion(degree_noint) # not over- or under-dispersed
DHARMa::plotResiduals(degree_noint)

# Now, let's fit a "maximal" model including up to the 3-way interactions (not including PC1*PC2 because those are orthogonal)
degree_max <- lmer(degreeRelative ~ situ*PC1*age_group + situ*PC1*season + situ*PC2*age_group + situ*PC2*season + PC1*age*season + PC2*age*season + (1|year) + (1|Nili_id), data = linked) # amazingly, this isn't rank-deficient!
check_model(degree_max) # apart from high VIF's, which is to be expected, the rest of the model checks actually look fine. I don't love that distribution, but it's not awful. Candidate.
summary(degree_max) # oof almost none of the 3-way interactions are significant. I guess that's good!

# The only 3-way interaction that's significant is season*PC2*age_group (and borderline situation*season*PC2, but only for one season). Let's remove all the other 3-way interactions. Have to add in all the 2-way interactions (some of these will be redundant with the ones subsumed in the remaining 3-way interaction, but that's fine).
degree_2 <- lmer(degreeRelative ~ situ*PC1 + situ*PC2 + situ*age_group + situ*season + PC1*age_group + PC1*season + PC2*age_group + PC2*season + age_group*season + PC2*age_group*season + (1|year) + (1|Nili_id), data = linked) # also runs fine
check_model(degree_2) #fine, or at least no worse. Candidate.
summary(degree_2) # okay, we're down to basically just the 2-way interactions. What else can we remove?
# Things that are non-significant: PC2*age_group, PC1*season, PC1, and PC2. Not going to remove PC1 and PC2. Let's start with removing the PC2*age_group and PC1*season.

degree_3 <- lmer(degreeRelative ~ situ*PC1 + situ*PC2 + situ*age_group + situ*season + PC1*age_group + PC2*season + age_group*season + PC2*age_group*season + (1|year) + (1|Nili_id), data = linked) # also runs fine
check_model(degree_3) #fine, or at least no worse. Candidate.
summary(degree_3) # okay, we're down to basically just the 2-way interactions. What else can we remove? The three-way interaction between PC2, age, and season is still significant, but only for one season, and the PC2*age interaction underneath is not. What happens if we remove the 3-way?

degree_4 <- lmer(degreeRelative ~ situ*PC1 + situ*PC2 + situ*age_group + situ*season + PC1*age_group + PC2*season + age_group*season + PC2*age_group + (1|year) + (1|Nili_id), data = linked) # runs fine
check_model(degree_4) # Fine, candidate.
summary(degree_4) # This looks pretty good now; I'm not sure I should go farther. But just in case the model checks reveal something, let's take out a few more that are marginally significant and/or small effects. Will start with PC2*age_group.

degree_5 <- lmer(degreeRelative ~ situ*PC1 + situ*PC2 + situ*age_group + situ*season + PC1*age_group + PC2*season + age_group*season + (1|year) + (1|Nili_id), data = linked) # runs fine
check_model(degree_5) # Fine, candidate.
summary(degree_5) # This looks good, to the point where I'm not sure if I should remove more, but I feel like I should on principle... What about PC2*season?

degree_6 <- lmer(degreeRelative ~ situ*PC1 + situ*PC2 + situ*age_group + situ*season + PC1*age_group + age_group*season + (1|year) + (1|Nili_id), data = linked)  # runs fine
check_model(degree_6) # Fine, candidate.
summary(degree_6) # Maybe remove situ*PC2? I don't know, I'm grasping at straws here.

degree_7 <- lmer(degreeRelative ~ situ*PC1 + PC2 +situ*age_group + situ*season + PC1*age_group + age_group*season + (1|year) + (1|Nili_id), data = linked)  # runs fine
check_model(degree_7) # Fine, candidate.
summary(degree_7) # I guess we can remove situ*age_group?

degree_8 <- lmer(degreeRelative ~ situ*PC1 + PC2 + situ*season + PC1*age_group + age_group*season + (1|year) + (1|Nili_id), data = linked) # runs fine
check_model(degree_8) # Fine, candidate.
summary(degree_8) # Just for argument's sake, let's remove situ*season, just to cover all the bases.

degree_9 <- lmer(degreeRelative ~ situ*PC1 + PC2 + PC1*age_group + age_group*season + (1|year) + (1|Nili_id), data = linked) # runs fine
check_model(degree_9) # Fine, candidate.
summary(degree_9) # Can try removing situ*PC1, but it's significant so I don't really want to.

degree_10 <- lmer(degreeRelative ~ situ + PC2 +  PC1*age_group + age_group*season + (1|year) + (1|Nili_id), data = linked) # runs fine
check_model(degree_10) # Fine, candidate.
summary(degree_10) # Time to stop, the next one would just be down to degree_noint

# Best model before I started removing effects that were significant
compare_performance(degree_noint, degree_max, degree_2, degree_3, degree_4, rank = T) # max, 4, 3, 2, noint. Yuuck, I don't like the best model being the maximal one...

# Best model including removing effects that were significant
compare_performance(degree_noint, degree_max, degree_2, degree_3, degree_4, degree_5, degree_6, degree_7, degree_8, degree_9, degree_10, rank = T) # Still max, 5, 3, 2...

degree_mod <- # ??

## Strength ------------------------------------------------------------------
strength_noint <- lmer(strength ~ type + PC1 + PC2 + age_group + season + (1|seasonUnique) + (1|Nili_id), data = linked %>% mutate(seasonUnique = paste(season, year))) 
summary(strength_noint)
check_model(strength_noint)

linked %>%
  ggplot(aes(x = strengthRelative))+
  geom_density()+
  facet_wrap(~season) # maybe worth modeling the zeroes separately...

# Now, let's fit a "maximal" model including up to the 3-way interactions (not including PC1*PC2 because those are orthogonal)
strength_max <- lmer(strengthRelative ~ situ*PC1*age_group + situ*PC1*season + situ*PC2*age_group + situ*PC2*season + PC1*age*season + PC2*age*season + (1|year) + (1|Nili_id), data = linked) # Not rank-deficient!
check_model(strength_max) # Distribution is a little less wonky but still a bit gross... not much different than the degree one though. High VIFs are expected due to all the interactions. Candidate.
summary(strength_max) # a lot of the 3-way interactions are not significant, but some are! Let's see. First to go can be season*PC2*age and situ*season*PC2.

compare_performance(strength_noint, strength_max, strength_2, strength_3, strength_4, strength_5, strength_6, strength_7, strength_8, rank = T) # hmmmmmmm max is top, then 6, then 3... that's weird! I don't like max! It's practically uninterpretable! Let's go with 6 for now I guess???

strength_mod <- strength_6

## Evenness ----------------------------------------------------------------
# XXX Going to hold off with this one until I've incorporated the new data.
linked_e <-  linked %>%
  mutate(evenness = case_when(evenness == 1 ~ 0.999,
                              TRUE ~ evenness))
evenness_noint <- lmer(evenness ~ situ*PC1 + situ*PC2 + situ*age_group + situ*season + situ*year +(1|Nili_id), data = linked_e)
check_model(evenness_noint) # that's really awful, isn't it. Maybe a beta distribution?

evenness_noint <- glmmTMB(evenness ~ situ*PC1 + situ*PC2 + situ*age_group + situ*season + situ*year +(1|Nili_id), data = linked_e, family = beta_family())
check_model(evenness_noint) # that fits better, but it's still kinda bad.
summary(evenness_noint)

evenness_max <- glmmTMB(evenness ~ situ*PC1*age_group*season + situ*PC2*age_group*season + situ*season*year + (1|Nili_id), data = linked_e, family = beta_family()) # doesn't converge

#Down to 3-way interactions
evenness_2 <- glmmTMB(evenness ~ situ*PC1*age_group + situ*PC1*season + situ*PC2*age_group + situ*PC2*season + situ*age_group*season + situ*season*year + (1|Nili_id), data = linked_e, family = beta_family()) # Doesn't converge

# Based on what we did before, let's remove situ*age_group*PC1
evenness_3 <- glmmTMB(evenness ~ situ*PC1*season + situ*PC2*age_group + situ*PC2*season + situ*age_group*season + situ*season*year + PC1*age_group + (1|Nili_id), data = linked_e, family = beta_family()) # Doesn't converge, still. hmmm. How about removing situ*age_group*PC2

evenness_4 <- glmmTMB(evenness ~ situ*PC1*season + situ*PC2 + situ*age_group + PC2*age_group + situ*PC2*season + situ*age_group*season + situ*season*year + PC1*age_group + (1|Nili_id), data = linked_e, family = beta_family()) # still doesn't converge. What do I even do from here? I feel like I'm mucking about in the dark.

# Get model effects ----------------------------------------------------------
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
