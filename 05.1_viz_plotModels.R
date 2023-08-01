library(tidyverse)
library(jtools)
library(sjPlot)
library(ggplot2)
library(ggeffects)

#load("data/linked.Rda") # DO NOT LOAD THIS!!!
load("data/forModeling.Rda") # loading this instead so we can run ggpredict properly.
load("data/mods.Rda")

# Response variable distributions -----------------------------------------
linked %>%
  pivot_longer(cols = c("degree", "strength", "evenness"), names_to = "measure", values_to = "value") %>%
  ggplot(aes(x = value, col = season, group = interaction(season, year)))+
  geom_density()+
  facet_wrap(~measure, scales = "free")+
  theme_classic()+
  scale_color_manual(name = "Season", values = c(breedingColor, fallColor, summerColor))+
  ylab("")+xlab("")+theme(text = element_text(size = 16))

forModeling %>%
  pivot_longer(cols = c("degree_scl", "strength_scl", "evenness_scl"), names_to = "measure", values_to = "value") %>%
  ggplot(aes(x = value, col = season, group = interaction(season, year)))+
  geom_density()+
  facet_wrap(~measure, scales = "free")+
  theme_classic()+
  scale_color_manual(name = "Season", values = c(breedingColor, fallColor, summerColor))+
  ylab("")+xlab("")+theme(text = element_text(size = 16))


# Set season colors
seasonColors <- c("#2FF8CA", "#CA2FF8", "#F8CA2F")
breedingColor <- "#2FF8CA"
summerColor <- "#CA2FF8"
fallColor <- "#F8CA2F"
situationColors <- c("dodgerblue", "olivedrab4", "gold") # flight, roosting, feeding. These actually might be bad colors because two of them combine to make the other.
flightColor <- "dodgerblue"
roostingColor <- "olivedrab4"
feedingColor <- "gold"

# Degree ------------------------------------------------------------------
d <- mods[["d"]]
# What is the effect of PC1 on degree, by situation?
## with raw data
plot_model(d, type = "eff", terms = c("PC1", "situ"), show.data = T)+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Degree (scaled)")+
  xlab("Movement (PC1)")+
  ggtitle("")

## without data
plot_model(d, type = "eff", terms = c("PC1", "situ"))+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Degree (scaled)")+
  xlab("Movement (PC1)")+
  ggtitle("")

## Is this effect explained by age differences?
plot_model(d, type = "eff", terms = c("PC1", "situ", "age_group"))+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Degree (scaled)")+
  xlab("Movement (PC1)")+
  ggtitle("") # sort of! The negative relationship between movement and degree in the co-feeding networks is entirely due to adult birds, but both juveniles and adults show a positive relationship between movement and degree in co-roosting networks.

# What about season?
plot_model(d, type = "eff", terms = c("PC1", "situ", "season"))+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Degree (scaled)")+
  xlab("Movement (PC1)")+
  ggtitle("")# the only seasonal difference is that we see no effect on feeding in summer and a slight downward effect on flight in summer. Otherwise the patterns hold.

# What is the effect of PC2 on degree, by situation?
## without data
plot_model(d, type = "eff", terms = c("PC2", "situ"))+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Degree (scaled)")+
  xlab("Exploration (PC2")+
  ggtitle("")

# Is this driven by age differences?
plot_model(d, type = "eff", terms = c("PC2", "situ", "age_group"))+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Degree (scaled)")+
  xlab("Exploration (PC2")+
  ggtitle("") # nope! In this case, the pattern is the same across adults and juveniles. 

# What about across seasons?
plot_model(d, type = "eff", terms = c("PC2", "situ", "season"))+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Degree (scaled)")+
  xlab("Movement (PC1)")+
  ggtitle("") # ooh, interesting! We have some big differences here. In summer, the roost network looks different. In fall, the flight and feeding networks have a weaker relationship than in the breeding and summer seasons. Arrrgh too much happening!

# Takeaways about DEGREE:
# 1. Overall, individuals that move more roost with more unique others. For adult vultures only, individuals that move more feed with fewer unique others (there is no relationship between movement and feeding degree for juveniles). 
# 2. Overall, more exploratory individuals interact with more others in flight and while feeding than do less exploratory individuals. But more exploratory vultures co-roost with very slightly fewer others. 
# 3. Seasonal differences: [insert descriptions here]
 
# Strength ----------------------------------------------------------------
s <- mods[["s"]]
## with raw data
plot_model(s, type = "eff", terms = c("PC1", "situ"), show.data = T)+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Strength (scaled)")+
  xlab("Movement (PC1)")+
  ggtitle("")

## without data
plot_model(s, type = "eff", terms = c("PC1", "situ"))+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Strength (scaled)")+
  xlab("Movement (PC1)")+
  ggtitle("") # Okay, wow. Strong negative relationships here for roosting and feeding.

## Does this change with age?
plot_model(s, type = "eff", terms = c("PC1", "situ", "age_group"))+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Strength (scaled)")+
  xlab("Movement (PC1)")+
  ggtitle("") # Only slightly. Adults have a stronger negative relationship between movement and strength than juveniles do in both roosting and feeding situations. The direction of the flight relationship is also opposite, but it's a very weak relationship in both cases and I don't think it's significant. *[how do I check whether a given relationship is significant? The interaction term is indeed significant... which now has me wondering about effect sizes.]

## What about with season?
plot_model(s, type = "eff", terms = c("PC1", "situ", "season"))+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Strength (scaled)")+
  xlab("Movement (PC1)")+
  ggtitle("") # same general patterns, but stronger effect for roosting in the breeding season. Stronger effects in the breeding season in general, actually.

# PC2 (exploration)
plot_model(s, type = "eff", terms = c("PC2", "situ"))+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Strength (scaled)")+
  xlab("Exploration (PC2)")+
  ggtitle("") # again, another strong relationship.

## Does this change with age?
plot_model(s, type = "eff", terms = c("PC2", "situ", "age_group"))+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Strength (scaled)")+
  xlab("Exploration (PC2)")+
  ggtitle("") # directionality stays the same, but we see stronger relationships for juveniles than for adults.

## What about with season?
plot_model(s, type = "eff", terms = c("PC2", "situ", "season"))+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Strength (scaled)")+
  xlab("Exploration (PC2)")+
  ggtitle("") # Differences in magnitude but not direction. Once again, we see the weakest effects in fall, with breeding and summer patterning together. This is unexpected!

# Evenness ----------------------------------------------------------------
e <- mods[["e"]]
## with raw data
plot_model(e, type = "eff", terms = c("PC1", "situ"), show.data = T)+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Evenness (scaled)")+
  xlab("Movement (PC1)")+
  ggtitle("") # hmmm.

## without data
plot_model(e, type = "eff", terms = c("PC1", "situ"))+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Evenness (scaled)")+
  xlab("Movement (PC1)")+
  ggtitle("") # ah, this is much easier to read (though i'm not convinced it's really a good model). We see negative effects of movement on evenness in feeding and flight situations, but positive effects of movement on evenness in roosting situations.

## Does this differ by age?
plot_model(e, type = "eff", terms = c("PC1", "situ", "age_group"))+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Evenness (scaled)")+
  xlab("Movement (PC1)")+
  ggtitle("") # Nope, not really any differences by age!

## What about by season?
plot_model(e, type = "eff", terms = c("PC1", "situ", "season"))+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Evenness (scaled)")+
  xlab("Movement (PC1)")+
  ggtitle("") # Not really any differences by season either! Yay for ease of interpretation (for now).

## PC2 without data
plot_model(e, type = "eff", terms = c("PC2", "situ"))+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Evenness (scaled)")+
  xlab("Exploration (PC2)")+
  ggtitle("") # negative relationship in flight, but only very slight relationships for feeding and roosting.

## does this differ by age?
plot_model(e, type = "eff", terms = c("PC2", "situ", "age_group"))+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Evenness (scaled)")+
  xlab("Exploration (PC2)")+
  ggtitle("") # whoa, totally opposite! Might just be a really small effect size, though...

## What about by season?
plot_model(e, type = "eff", terms = c("PC2", "situ", "season"))+
  scale_color_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  scale_fill_manual(name = "Situation", values = c(feedingColor, flightColor, roostingColor))+
  ylab("Evenness (scaled)")+
  xlab("Exploration (PC2)")+
  ggtitle("") # interesting--in the other season plots, fall had the weakest effects, but here it has the strongest. 