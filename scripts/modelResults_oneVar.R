library(tidyverse)
library(here)
library(targets)
library(patchwork)
library(ggeffects)
library(gtsummary)
library(extrafont)
library(ggthemes)
library(lemon)
library(see)
library(sf)
library(scales)
library(ggmap)
library(glmmTMB)

tar_load(linked)
tar_load(space_use)
tar_load(situcolors)

new <- linked %>%
  select(Nili_id, seasonUnique, degree, strength, z_deg, z_str, season, situ) %>%
  distinct() %>%
  left_join(select(space_use, Nili_id, seasonUnique, homeRange_log, coreAreaFidelity))

# Model results (Home range only) -------------------------------------------------
# Load models
load(here("data/created/hr_deg_obs.Rda"))
load(here("data/created/hr_deg_int.Rda"))
load(here("data/created/hr_str_obs.Rda"))
load(here("data/created/hr_str_int.Rda"))

# List models so we can operate on them
models <- list(hr_deg_obs, hr_deg_int, hr_str_obs, hr_str_int)

effs <- map(models, ~as.data.frame(emmeans::emtrends(.x, specs = "situ", var = "homeRange_log")))
preds <- map(models, ~as.data.frame(ggeffect(.x, terms = c("homeRange_log", "situ"))))
responses <- rep(c("degree", "strength"), each = 2)
mods <- rep(c("Observed", "Intentional"), 2)

effs <- pmap(.l = list(x = effs, y = responses, z = mods), 
             .f = function(x, y, z){x %>% mutate(response = y,
                                                 mod = z)}) %>%
  purrr::list_rbind() %>%
  mutate(situ = case_when(situ == "Fe" ~ "Feeding",
                          situ == "Fl" ~ "Flight",
                          situ == "Ro" ~ "Roosting"),
         mod = factor(mod, levels = c("Observed", "Intentional"))) %>%
  mutate(sig = case_when(sign(lower.CL) == sign(upper.CL) ~ T,
                         .default = F))

preds <- pmap(.l = list(x = preds, y = responses, z = mods), 
              .f = function(x, y, z){x %>% mutate(response = y,
                                                  mod = z)}) %>%
  purrr::list_rbind() %>%
  mutate(group = case_when(group == "Fe" ~ "Feeding",
                           group == "Fl" ~ "Flight",
                           group == "Ro" ~ "Roosting"),
         mod = factor(mod, levels = c("Observed", "Intentional")))

preds <- preds %>%
  left_join(effs %>%
              dplyr::select(response, mod, sig, situ) %>%
              distinct(), by = c("group" = "situ", "response", "mod"))

# Prepare data for forest plots (insets)
prepdata_forforestplots <- function(variable, mod, z_mod){
  dat <- as.data.frame(ggeffect(mod, terms = c(variable, "situ"))) %>%
    mutate(mod = "mod")
  z_dat <- as.data.frame(ggeffect(z_mod, terms = c(variable, "situ"))) %>%
    mutate(mod = "z_mod")
  eff1 <- as.data.frame(emmeans::emtrends(mod, specs = "situ", 
                                          var = variable)) %>%
    mutate(mod = "Observed")
  names(eff1)[5:6] <- c("lower", "upper")
  eff2 <- as.data.frame(emmeans::emtrends(z_mod, specs = "situ", 
                                          var = variable)) %>% 
    mutate(mod = "Intentional")
  names(eff2)[5:6] <- c("lower", "upper")
  eff <- bind_rows(eff1, eff2) %>%
    mutate(mod = factor(mod, levels = c("Observed", "Intentional"))) %>%
    mutate(situ = case_when(situ == "Fe" ~ "Feeding",
                            situ == "Fl" ~ "Flight",
                            situ == "Ro" ~ "Roosting"),
           var = variable)
  
  annot <- eff %>%
    mutate(sig = case_when(lower < 0 & upper < 0 ~ T,
                           lower > 0 & upper > 0 ~ T,
                           .default = F))
  return(annot)
}

deg_space <- prepdata_forforestplots(variable = "homeRange_log", mod = models[[1]],
                                     z_mod = models[[2]]) %>%
  mutate(measure = "degree") %>%
  group_by(mod) %>%
  group_split()
str_space <- prepdata_forforestplots(variable = "homeRange_log", mod = models[[3]],
                                     z_mod = models[[4]]) %>%
  mutate(measure = "strength") %>%
  group_by(mod) %>%
  group_split()

forforest <- c(deg_space, str_space)
length(forforest)
forforest <- map(forforest, ~.x %>% rename("trend" = 2))

forestplots <- map(forforest, ~{
  p <- .x %>%
    ggplot(aes(x = trend, y = situ, col = situ))+
    geom_vline(aes(xintercept = 0), linetype = 2)+
    geom_errorbar(aes(xmin = lower, xmax = upper),
                  width = 0, linewidth = 0.75)+
    geom_point(size = 3)+
    scale_color_manual(values = situcolors, guide = "none")+
    scale_y_discrete(limits = rev, position = "right")+
    labs(x = "Effect")+
    theme_classic()+
    theme(text = element_text(size = 10),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  return(p)
})

## Line plots
preds_list <- preds %>%
  group_by(mod, response) %>%
  group_split() %>%
  map(., ~.x %>% mutate(sig = factor(sig, levels = c(F, T))))

labs <- c("Degree", "Strength", "Degree z-score", "Strength z-score")
titles <- c("Observed", "Observed", "Intentional", "Intentional")

lineplots <- pmap(list(a = preds_list, b = labs), .f = function(a, b){
  p <- a %>%
    ggplot(aes(x = x, y = predicted, col = group))+
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group, col = NULL),
                alpha = 0.2)+
    geom_line(aes(linetype = sig), show.legend = T, linewidth = 1.5)+
    scale_linetype_manual(drop = FALSE, values = c(2, 1))+
    scale_color_manual(name = "Situation", values = situcolors)+
    scale_fill_manual(name = "Situation", values = situcolors)+
    labs(y = b, x = NULL)+
    #geom_point(data = e, pch = 1, alpha = 0.5)+
    theme_classic()+
    theme(legend.position = "none",
          text = element_text(size = 14))
  return(p)
})

a <- lineplots[[1]] + ggtitle("Observed") + theme(plot.margin = unit(c(1.4, 1.1, 0.1, 0.1), "cm"))
b <- lineplots[[3]] + ggtitle("Intentional") + theme(plot.margin = unit(c(1.4, 1.1, 0.1, 0.1), "cm"))
c <- lineplots[[2]] + theme(plot.margin = unit(c(1.4, 1.1, 0.1, 0.1), "cm"))
d <- lineplots[[4]] + theme(plot.margin = unit(c(1.4, 1.1, 0.1, 0.1), "cm"))
results <- (a + b)/(c + d)
results

ggsave(results, width =9, height = 6.5, file = here("fig/HRfig3_line.png"))

ggsave(forestplots[[1]], filename = here("fig/HRfig3_forest1.png"), width = 1.5, height = 1)
ggsave(forestplots[[2]], filename = here("fig/HRfig3_forest2.png"), width = 1.5, height = 1)
ggsave(forestplots[[3]], filename = here("fig/HRfig3_forest3.png"), width = 1.5, height = 1)
ggsave(forestplots[[4]], filename = here("fig/HRfig3_forest4.png"), width = 1.5, height = 1)

# Model results (Home range only) -------------------------------------------------
# Load models
load(here("data/created/caf_deg_obs.Rda"))
load(here("data/created/caf_deg_int.Rda"))
load(here("data/created/caf_str_obs.Rda"))
load(here("data/created/caf_str_int.Rda"))

# List models so we can operate on them
models <- list(caf_deg_obs, caf_deg_int, caf_str_obs, caf_str_int)

effs <- effs <- map(models, ~as.data.frame(emmeans::emtrends(.x, specs = "situ", var = "coreAreaFidelity")))
preds <- map(models, ~as.data.frame(ggeffect(.x, terms = c("coreAreaFidelity", "situ"))))
responses <- rep(c("degree", "strength"), each = 2)
mods <- rep(c("Observed", "Intentional"), 2)

effs <- pmap(.l = list(x = effs, y = responses, z = mods), 
             .f = function(x, y, z){x %>% mutate(response = y,
                                                 mod = z)}) %>%
  purrr::list_rbind() %>%
  mutate(situ = case_when(situ == "Fe" ~ "Feeding",
                          situ == "Fl" ~ "Flight",
                          situ == "Ro" ~ "Roosting"),
         mod = factor(mod, levels = c("Observed", "Intentional"))) %>%
  mutate(sig = case_when(sign(lower.CL) == sign(upper.CL) ~ T,
                         .default = F))

preds <- pmap(.l = list(x = preds, y = responses, z = mods), 
              .f = function(x, y, z){x %>% mutate(response = y,
                                                  mod = z)}) %>%
  purrr::list_rbind() %>%
  mutate(group = case_when(group == "Fe" ~ "Feeding",
                           group == "Fl" ~ "Flight",
                           group == "Ro" ~ "Roosting"),
         mod = factor(mod, levels = c("Observed", "Intentional")))

preds <- preds %>%
  left_join(effs %>%
              dplyr::select(response, mod, sig, situ) %>%
              distinct(), by = c("group" = "situ", "response", "mod"))

# Prepare data for forest plots (insets)
prepdata_forforestplots <- function(variable, mod, z_mod){
  dat <- as.data.frame(ggeffect(mod, terms = c(variable, "situ"))) %>%
    mutate(mod = "mod")
  z_dat <- as.data.frame(ggeffect(z_mod, terms = c(variable, "situ"))) %>%
    mutate(mod = "z_mod")
  eff1 <- as.data.frame(emmeans::emtrends(mod, specs = "situ", 
                                          var = variable)) %>%
    mutate(mod = "Observed")
  names(eff1)[5:6] <- c("lower", "upper")
  eff2 <- as.data.frame(emmeans::emtrends(z_mod, specs = "situ", 
                                          var = variable)) %>% 
    mutate(mod = "Intentional")
  names(eff2)[5:6] <- c("lower", "upper")
  eff <- bind_rows(eff1, eff2) %>%
    mutate(mod = factor(mod, levels = c("Observed", "Intentional"))) %>%
    mutate(situ = case_when(situ == "Fe" ~ "Feeding",
                            situ == "Fl" ~ "Flight",
                            situ == "Ro" ~ "Roosting"),
           var = variable)
  
  annot <- eff %>%
    mutate(sig = case_when(lower < 0 & upper < 0 ~ T,
                           lower > 0 & upper > 0 ~ T,
                           .default = F))
  return(annot)
}

deg_space <- prepdata_forforestplots(variable = "coreAreaFidelity", mod = models[[1]],
                                     z_mod = models[[2]]) %>%
  mutate(measure = "degree") %>%
  group_by(mod) %>%
  group_split()
str_space <- prepdata_forforestplots(variable = "coreAreaFidelity", mod = models[[3]],
                                     z_mod = models[[4]]) %>%
  mutate(measure = "strength") %>%
  group_by(mod) %>%
  group_split()

forforest <- c(deg_space, str_space)
length(forforest)
forforest <- map(forforest, ~.x %>% rename("trend" = 2))

forestplots <- map(forforest, ~{
  p <- .x %>%
    ggplot(aes(x = trend, y = situ, col = situ))+
    geom_vline(aes(xintercept = 0), linetype = 2)+
    geom_errorbar(aes(xmin = lower, xmax = upper),
                  width = 0, linewidth = 0.75)+
    geom_point(size = 3)+
    scale_color_manual(values = situcolors, guide = "none")+
    scale_y_discrete(limits = rev, position = "right")+
    labs(x = "Effect")+
    theme_classic()+
    theme(text = element_text(size = 10),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  return(p)
})

## Line plots
preds_list <- preds %>%
  group_by(mod, response) %>%
  group_split() %>%
  map(., ~.x %>% mutate(sig = factor(sig, levels = c(F, T))))

labs <- c("Degree", "Strength", "Degree z-score", "Strength z-score")
titles <- c("Observed", "Observed", "Intentional", "Intentional")

lineplots <- pmap(list(a = preds_list, b = labs), .f = function(a, b){
  p <- a %>%
    ggplot(aes(x = x, y = predicted, col = group))+
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group, col = NULL),
                alpha = 0.2)+
    geom_line(aes(linetype = sig), show.legend = T, linewidth = 1.5)+
    scale_linetype_manual(drop = FALSE, values = c(2, 1))+
    scale_color_manual(name = "Situation", values = situcolors)+
    scale_fill_manual(name = "Situation", values = situcolors)+
    labs(y = b, x = NULL)+
    #geom_point(data = e, pch = 1, alpha = 0.5)+
    theme_classic()+
    theme(legend.position = "none",
          text = element_text(size = 14))
  return(p)
})

a <- lineplots[[1]] + ggtitle("Observed") + theme(plot.margin = unit(c(1.4, 1.1, 0.1, 0.1), "cm"))
b <- lineplots[[3]] + ggtitle("Intentional") + theme(plot.margin = unit(c(1.4, 1.1, 0.1, 0.1), "cm"))
c <- lineplots[[2]] + theme(plot.margin = unit(c(1.4, 1.1, 0.1, 0.1), "cm"))
d <- lineplots[[4]] + theme(plot.margin = unit(c(1.4, 1.1, 0.1, 0.1), "cm"))
results <- (a + b)/(c + d)
results

ggsave(results, width =9, height = 6.5, file = here("fig/CAFfig3_line.png"))

ggsave(forestplots[[1]], filename = here("fig/CAFfig3_forest1.png"), width = 1.5, height = 1)
ggsave(forestplots[[2]], filename = here("fig/CAFfig3_forest2.png"), width = 1.5, height = 1)
ggsave(forestplots[[3]], filename = here("fig/CAFfig3_forest3.png"), width = 1.5, height = 1)
ggsave(forestplots[[4]], filename = here("fig/CAFfig3_forest4.png"), width = 1.5, height = 1)