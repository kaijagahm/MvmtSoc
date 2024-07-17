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
library(targets)
# THIS SCRIPT IS FOR TESTING OUT MODELS. FINAL MODEL CODE GOES OVER INTO FUNCTIONS.R TO BE INCORPORATED INTO THE TARGETS PIPELINE.
# Updated 2024-06-10: removing roost_div as a predictor

# Set ggplot theme to classic
theme_set(theme_classic())

# Load the data from the targets pipeline (wow this is so much easier!!!)
tar_load(linked)
tar_load(cc)
situcolors <- c(cc$feedingColor, cc$flightColor, cc$roostingColor)
seasoncolors <- c(cc$breedingColor, cc$summerColor, cc$fallColor)

# Examine response variable distributions ---------------------------------

# Normalized degree and strength ------------------------------------------
# linked %>%
#   ggplot(aes(x = degree, col = season, group = seasonUnique))+
#   geom_density()+
#   facet_wrap(~type, scales = "free") # very distinct distributions, but since we will have a random effect for seasonUnique, that should cover it... 
# 
# linked %>%
#   ggplot(aes(x = strength, col = season, group = seasonUnique))+
#   geom_density()+
#   facet_wrap(~type, scales = "free") 
# 
# # Degree and strength z-scores (non-normalized) ---------------------------
# linked %>%
#   ggplot(aes(x = z_deg, col = season, group = seasonUnique))+
#   geom_density()+
#   facet_wrap(~type, scales = "free") # these are weird distributions. I wonder what model family we should use here...
# 
# linked %>%
#   ggplot(aes(x = z_str, col = season, group = seasonUnique))+
#   geom_density()+
#   facet_wrap(~type, scales = "free")

# Let's examine zeroes for the social network measures. I know that when we calculate the social networks, we had a lot of zeroes for both degree and strength. But most of the individuals that didn't have network connections probably aren't our focal individuals for the movement measures.
# linked %>% filter(z_deg == 0 | z_str == 0) # nobody
# linked %>% filter(degree == 0, strength == 0) # just one individual in one season
# linked %>% filter(is.na(z_deg) | is.na(z_str)) # just one individual in one season
# linked %>% filter(is.na(z_deg) | is.na(z_str)) # just one individual in one season

# Let's remove that individual in case she poses a problem for modeling
linked <- linked %>%
  filter(!is.na(z_deg), !is.na(z_str))
nrow(linked)

linked %>% filter(is.infinite(z_deg) | is.infinite(z_str)) # likewise, removing the infinite individual
linked <- linked %>%
  filter(!is.infinite(z_deg), !is.infinite(z_str))

# Modeling: not corrected for space use -----------------------------------
tar_load(allMetrics)
# Degree -----------------------------------------------------------------
deg_base <- glmmTMB(normDegree ~ situ + movement + space_use + age_group + season + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
check_predictions(deg_base) # bleh
simulationOutput <- DHARMa::simulateResiduals(deg_base)
plot(simulationOutput, pch=".") # not great but not terrible.

deg_full <- glmmTMB(normDegree ~ situ*season*movement + situ*season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian()) # not converging, hmmm...
simulationOutput <- DHARMa::simulateResiduals(deg_full)
plot(simulationOutput, pch=".") # not great but not terrible.
check_collinearity(deg_full) # remove situ*season*movement first

deg_1 <- glmmTMB(normDegree ~ situ*season + situ*movement + season*movement + situ*season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_1) # now remove situ*season*space.

deg_2 <- glmmTMB(normDegree ~ situ*season + situ*movement + season*movement + situ*space_use + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_2) # now remove situ*season

deg_3 <- glmmTMB(normDegree ~ situ*movement + season*movement + situ*space_use + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_3) # this looks pretty good, let's look at the summary
summary(deg_3) # season*space can be removed

deg_4 <- glmmTMB(normDegree ~ situ*movement + season*movement + situ*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_4) # looks pretty good
summary(deg_4) # everything else is at least marginally significant, though movement*season is really borderline...

deg_mod <- deg_4

# Strength ---------------------------------------------------------------
str_base <- glmmTMB(normStrength ~ situ + movement + space_use + age_group + season + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
check_predictions(str_base) # not bad, actually!
simulationOutput <- DHARMa::simulateResiduals(str_base)
plot(simulationOutput, pch=".") # nice, pretty good!

str_full <- glmmTMB(normStrength ~ situ*season*movement + situ*season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
simulationOutput <- DHARMa::simulateResiduals(str_full)
dev.off()
plot(simulationOutput, pch=".") # huh, not the worst actually!
check_collinearity(str_full) # the worst is situ*season*space

str_1 <- glmmTMB(normStrength ~ situ*season*movement + situ*season + situ*space_use + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
check_collinearity(str_1) # now we can remove situ*season*movement

str_2 <- glmmTMB(normStrength ~ situ*season + situ*movement + season*movement + situ*space_use + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
check_collinearity(str_2) # now we can remove situ*season

str_3 <- glmmTMB(normStrength ~ situ*movement + season*movement + situ*space_use + season*space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
check_collinearity(str_3) # movement*season is technically now the highest, but they're very close so let's just look at the summary
summary(str_3) # space*season and situ*space and situ*movement and movement*season are all non-significant.

str_4 <- glmmTMB(normStrength ~ situ + season + movement + space_use + age_group + (1|seasonUnique) + (1|Nili_id), data = linked, family = beta_family())
summary(str_4) # done

str_mod <- str_4

# Modeling: z-scores (corrected for space use) ----------------------------
# This time we're using non-normalized degree and strength values, since using z-scores ends up normalizing each individual against itself.

# Visualize some of the z score deviations
tar_load(allMetrics)
vultures <- sample(unique(allMetrics$Nili_id), 10)
allMetrics %>% filter(Nili_id %in% vultures) %>% ggplot(aes(x = Nili_id, y = wrapped_degree, group = interaction(situ, Nili_id), col = situ))+geom_violin()+facet_wrap(~season, scales = "free")+geom_point(aes(x = Nili_id, y = degree)) # individuals generally *do* have degrees higher than expected by chance.

# Degree ---------------------------------------------------------------
deg_z_base <- glmmTMB(z_deg ~ situ + movement + space_use + age_group + season + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
check_predictions(deg_z_base) # ick

deg_z_full <- glmmTMB(z_deg ~ situ*season*movement + situ*season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_predictions(deg_z_full) # not any better
simulationOutput <- DHARMa::simulateResiduals(deg_z_full)
dev.off()
plot(simulationOutput, pch=".") # actually not as terrible as I'd feared
check_collinearity(deg_z_full) # first get rid of situ*season*movement

deg_z_1 <- glmmTMB(z_deg ~ situ*season + situ*movement + season*movement + situ*season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_z_1) # now remove situ*season*space

deg_z_2 <- glmmTMB(z_deg ~ situ*season + situ*movement + season*movement + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_z_2) # now remove situ*season

deg_z_3 <- glmmTMB(z_deg ~situ*movement + season*movement + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(deg_z_3) # now let's take a look at what's significant
summary(deg_z_3) # season*space is totally n.s., as well as movement*season

deg_z_4 <- glmmTMB(z_deg ~situ*movement + situ*space_use + age_group + season + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
summary(deg_z_4) # everything else is still at least marginally significant
check_collinearity(deg_z_4) # situ*space is collinear though... but I'm gonna leave it because it's still below 5.

# so let's go with deg_z_9a
deg_z_mod <- deg_z_4

# Strength ---------------------------------------------------------------
str_z_base <- glmmTMB(z_str ~ situ + movement + space_use + age_group + season + (1|seasonUnique)+(1|Nili_id), data = linked, family = gaussian())
check_predictions(str_z_base) # oh no...
linked %>% ggplot(aes(x = z_str, col = situ, group = interaction(situ, seasonUnique)))+geom_density()

str_z_full <- glmmTMB(z_str ~ situ*season*movement + situ*season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_predictions(str_z_full) # not any better
simulationOutput <- DHARMa::simulateResiduals(str_z_full)
dev.off()
plot(simulationOutput, pch=".") # also really bad. I wonder what I should do about this...
check_collinearity(str_z_full) # first remove situ*season*movement

str_z_1 <- glmmTMB(z_str ~ situ*season + situ*movement + season*movement + situ*season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(str_z_1) # now remove situ*season*space

str_z_2 <- glmmTMB(z_str ~ situ*season + situ*movement + season*movement + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(str_z_2) # now remove situ*season

str_z_3 <- glmmTMB(z_str ~ situ*movement + season*movement + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(str_z_3) # now can remove movement*season

str_z_4 <- glmmTMB(z_str ~ situ*movement + situ*space_use + season*space_use + age_group + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(str_z_4) # now let's look at significance
summary(str_z_4) # space*season is not significant

str_z_5 <- glmmTMB(z_str ~ situ*movement + situ*space_use + age_group + season + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
check_collinearity(str_z_5)
summary(str_z_5) # situ*movement is also n.s.

str_z_6 <- glmmTMB(z_str ~ movement + situ*space_use + age_group + season + (1|seasonUnique)+ (1|Nili_id), data = linked, family = gaussian())
summary(str_z_6) # remaining interaction is sig.

str_z_mod <- str_z_6

# Degree models to plot -----------------------------------------------------
### Without randomizations:
summary(deg_mod) # situ*space, movement*season, situ*movement
### With randomizations:
summary(deg_z_mod) # situ*space, situ*movement

# Strength models to plot ---------------------------------------------------
### Without randomizations:
summary(str_mod) # none
### With randomizations:
summary(str_z_mod) # situ*space

# Define plotting functions -----------------------------------------------
effplot <- function(model, terms, dataset, pointX, pointY, colvar, legendTitle, values, ylab, xlab){
  dat <- as.data.frame(ggeffect(model, terms = terms))
  
  if(length(terms) > 1){
    plt <- ggplot(dat, aes(x, predicted)) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, linewidth = 0.6, show.legend = F)+
      geom_point(data = dataset %>% mutate("group" = .data[[colvar]]), 
                 aes(.data[[pointX]], .data[[pointY]], col = group), alpha = 0.5)+
      geom_line(aes(col = group), linewidth = 1) +
      facet_wrap(~group, scales = "free_y")+
      scale_color_manual(legendTitle, values = values) + 
      scale_fill_manual(legendTitle, values = values) + 
      ylab(ylab) + xlab(xlab)
  }else{
    plt <- ggplot(dat, aes(x, predicted)) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, linewidth = 0.6, show.legend = F)+
      geom_point(data = dataset %>% mutate("group" = .data[[colvar]]), 
                 aes(.data[[pointX]], .data[[pointY]], col = group), alpha = 0.5)+
      geom_line(linewidth = 1) +
      facet_wrap(~group, scales = "free_y")+
      scale_color_manual(legendTitle, values = values) + 
      ylab(ylab) + xlab(xlab)
  }
  return(plt)
}

emtplot <- function(model, specs, var, values, ylab, xlab){
  dat <- as.data.frame(emmeans::emtrends(model, specs, var))
  lower <- ifelse("lower.CL" %in% names(dat), "lower.CL", "asymp.LCL")
  upper <- ifelse("upper.CL" %in% names(dat), "upper.CL", "asymp.UCL")
  plt <- dat %>% ggplot(aes(.data[[specs]], 
                            .data[[paste0(var, ".", "trend")]], 
                            col = .data[[specs]])) + 
    geom_point(size = 6) +
    geom_errorbar(aes(x = .data[[specs]], 
                      ymin = .data[[lower]], 
                      ymax = .data[[upper]]), 
                  width = 0, linewidth = 2)+geom_hline(aes(yintercept = 0), 
                                                       linetype = 2)+
    scale_color_manual(values = values)+ 
    theme(legend.position = "none") + ylab(ylab)+ xlab(xlab) + coord_flip()
  return(plt)
}


# Degree plots ------------------------------------------------------------
### Without randomizations:
#### situ*space
deg_situ_space_P <- effplot(model = deg_mod, terms = c("space_use", "situ"), 
                            dataset = linked, pointX = "space_use", 
                            pointY = "normDegree", col = "situ", 
                            legendTitle = "Situation", values = situcolors, 
                            ylab = "normDegree", xlab = "Space use (log)")

deg_situ_space_emt_P <- emtplot(model = deg_mod, specs = "situ", 
                                var = "space_use", values = situcolors, 
                                ylab = "Space use effect (normDegree)", 
                                xlab = "Situation")

#### movement*season
deg_season_movement_P <- effplot(model = deg_mod, terms = c("movement", "season"), 
                                 dataset = linked, pointX = "movement", 
                                 pointY = "normDegree", col = "season", 
                                 legendTitle = "Season", values = seasoncolors, 
                                 ylab = "normDegree", xlab = "Movement")

deg_season_movement_emt_P <- emtplot(model = deg_mod, specs = "season", 
                                     var = "movement", values = seasoncolors, 
                                     ylab = "Movement effect (normDegree)", 
                                     xlab = "Season")

#### movement*situ
deg_situ_movement_P <- effplot(model = deg_mod, terms = c("movement", "situ"), 
                               dataset = linked, pointX = "movement", 
                               pointY = "normDegree", col = "situ", 
                               legendTitle = "Situation", values = situcolors, 
                               ylab = "normDegree", xlab = "Movement")

deg_situ_movement_emt_P <- emtplot(model = deg_mod, specs = "situ", 
                                   var = "movement", values = situcolors, 
                                   ylab = "Movement effect (normDegree)", 
                                   xlab = "Situation")

### With randomizations:
#### situ*space
deg_z_situ_space_P <- effplot(model = deg_z_mod, terms = c("space_use", "situ"), 
                            dataset = linked, pointX = "space_use", 
                            pointY = "z_deg", col = "situ", 
                            legendTitle = "Situation", values = situcolors, 
                            ylab = "Degree (z-score)", xlab = "Space use (log)")

deg_z_situ_space_emt_P <- emtplot(model = deg_z_mod, specs = "situ", 
                                var = "space_use", values = situcolors, 
                                ylab = "Space use effect (Degree z-scores)", 
                                xlab = "Situation")

#### situ*movement
deg_z_situ_movement_P <- effplot(model = deg_z_mod, terms = c("movement", "situ"), 
                              dataset = linked, pointX = "movement", 
                              pointY = "z_deg", col = "situ", 
                              legendTitle = "Situation", values = situcolors, 
                              ylab = "Degree (z-score)", xlab = "Movement")

deg_z_situ_movement_emt_P <- emtplot(model = deg_z_mod, specs = "situ", 
                                  var = "movement", values = situcolors, 
                                  ylab = "Movement effect (Degree z-scores)", 
                                  xlab = "Situation")

# Strength plots ------------------------------------------------------------
### Without randomizations:
#### none

### With randomizations:
#### situ*space
str_z_situ_space_P <- effplot(model = str_z_mod, terms = c("space_use", "situ"), 
                            dataset = linked, pointX = "space_use", 
                            pointY = "z_str", col = "situ", 
                            legendTitle = "Situation", values = situcolors, 
                            ylab = "Strength (z-score)", xlab = "Space use (log)")

str_z_situ_space_emt_P <- emtplot(model = str_z_mod, specs = "situ", 
                                var = "space_use", values = situcolors, 
                                ylab = "Space use effect (strength z-score)", 
                                xlab = "Situation")
