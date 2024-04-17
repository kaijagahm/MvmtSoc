library(tidyverse)
library(performance) # for modeling and comparisons
library(emmeans) # estimated marginal means/trends
library(ggeffects)
library(here)

situcolors <- c(cc$feedingColor, cc$flightColor, cc$roostingColor)
seasoncolors <- c(cc$breedingColor, cc$summerColor, cc$fallColor)

# Set ggplot theme to classic
theme_set(theme_classic())

tar_load(linked)

load(here("data/deg_mod.Rda"))
load(here("data/deg_z_mod.Rda"))
load(here("data/str_mod.Rda"))
load(here("data/str_z_mod.Rda"))

# Degree models to plot -----------------------------------------------------
### Without randomizations:
summary(deg_mod) # situ*space, movement*season, situ*movement, roost_div
### With randomizations:
summary(deg_z_mod) # situ*space, roost_div

# Strength models to plot ---------------------------------------------------
### Without randomizations:
summary(str_mod) # situ*roost, roost_div
### With randomizations:
summary(str_z_mod) # situ*space, roost_div

# Define plotting functions -----------------------------------------------
effplot <- function(model, terms, dataset, pointX, pointY, colvar, legendTitle, values, ylab, xlab){
  dat <- as.data.frame(ggeffect(model, terms = terms))
  
  if(length(terms) > 1){
    plt <- ggplot(dat, aes(x, predicted)) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, linewidth = 0.6, show.legend = F)+
      geom_point(data = dataset, aes(.data[[pointX]], .data[[pointY]], col = .data[[colvar]]), alpha = 0.5)+
      geom_line(aes(col = group), linewidth = 1) +
      scale_color_manual(legendTitle, values = values) + 
      scale_fill_manual(legendTitle, values = values) + 
      ylab(ylab) + xlab(xlab)
  }else{
    plt <- ggplot(dat, aes(x, predicted)) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, linewidth = 0.6, show.legend = F)+
      geom_point(data = dataset, aes(.data[[pointX]], .data[[pointY]], col = .data[[colvar]]), alpha = 0.5)+
      geom_line(linewidth = 1) +
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

#### roost_div
deg_roost_P <- effplot(model = deg_mod, terms = c("roost_div"), 
                       dataset = linked, pointX = "roost_div", 
                       pointY = "normDegree", col = "situ", 
                       legendTitle = "Situation", values = situcolors, 
                       ylab = "normDegree", xlab = "Roost diversification")

### With randomizations:
#### situ*space
deg_z_situ_space_P <- effplot(model = deg_z_mod, terms = c("space_use", "situ"), 
                              dataset = linked, pointX = "space_use", 
                              pointY = "z_deg", col = "situ", 
                              legendTitle = "Situation", values = situcolors, 
                              ylab = "Degree (z-score)", xlab = "Space use (log)")

deg_z_situ_space_emt_P <- emtplot(model = deg_mod, specs = "situ", 
                                  var = "space_use", values = situcolors, 
                                  ylab = "Space use effect (Degree z-scores)", 
                                  xlab = "Situation")

#### roost_div
deg_z_roost_P <- effplot(model = deg_z_mod, terms = c("roost_div"), 
                         dataset = linked, pointX = "roost_div", 
                         pointY = "z_deg", col = "situ", 
                         legendTitle = "Situation", values = situcolors, 
                         ylab = "Degree (z-score)", xlab = "Roost diversification")

# Strength plots ------------------------------------------------------------
### Without randomizations:
#### situ*roost
str_situ_roost_P <- effplot(model = str_mod, terms = c("roost_div", "situ"), 
                            dataset = linked, pointX = "roost_div", 
                            pointY = "normStrength", col = "situ", 
                            legendTitle = "Situation", values = situcolors, 
                            ylab = "normStrength", xlab = "Roost diversification")

str_situ_roost_emt_P <- emtplot(model = str_mod, specs = "situ", 
                                var = "roost_div", values = situcolors, 
                                ylab = "Roost diversification effect (normStrength)", 
                                xlab = "Situation")

#### roost_div
str_roost_P <- effplot(model = str_mod, terms = c("roost_div"), 
                       dataset = linked, pointX = "roost_div", 
                       pointY = "normStrength", col = "situ", 
                       legendTitle = "Situation", values = situcolors, 
                       ylab = "normStrength", xlab = "Roost diversification")

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
