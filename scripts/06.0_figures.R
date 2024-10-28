# Making the three figures we designed with Orr
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
tar_config_set(store = here::here('_targets'))
tar_load(cleaned_r)
tar_load(joined0_r)
tar_load(downsampled_10min_forSocial_r)
tar_load(downsampled_10min_r)
tar_load(cc)
tar_load(linked)
tar_load(deg_mod)
tar_load(deg_z_mod)
tar_load(str_mod)
tar_load(str_z_mod)
tar_load(situcolors)
tar_load(seasoncolors)
tar_load(movement_corr)
tar_load(space_corr)

# New models with only space use
load(here("data/sp_deg_obs_mod.Rda"))
load(here("data/sp_deg_int_mod.Rda"))
load(here("data/sp_str_obs_mod.Rda"))
load(here("data/sp_str_int_mod.Rda"))

mods <- list(sp_deg_obs_mod, sp_deg_int_mod, sp_str_obs_mod, sp_str_int_mod)

effs <- map(mods, ~as.data.frame(emmeans::emtrends(.x, specs = "situ", var = "space_use")))
preds <- map(mods, ~as.data.frame(ggeffect(.x, terms = c("space_use", "situ"))))
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
              select(response, mod, sig, situ) %>%
              distinct(), by = c("group" = "situ", "response", "mod"))


# Figure 1 ----------------------------------------------------------------

# Figure 2 ----------------------------------------------------------------
## A
fig_2a <- linked %>%
  filter(situ == "Fe") %>%
  ungroup() %>%
  ggplot(aes(x = normDegree))+
  geom_density(linewidth = 1.5, col = situcolors[1])+
  theme_classic()+
  theme(legend.position = "bottom",
        text = element_text(family = "Verdana", size = 14))+
  xlab("Observed degree\n(normalized)")+
  ylab("Density")
fig_2a
ggsave(filename = here("fig/2a.png"), plot = fig_2a, width = 2.75, height = 2.75)

## B
fig_2b <- linked %>%
  filter(situ == "Fe") %>%
  ungroup() %>%
  ggplot(aes(x = z_deg))+
  geom_density(linewidth = 1.5, col = situcolors[1])+
  theme_classic()+
  theme(legend.position = "bottom",
        text = element_text(family = "Verdana", size = 14))+
  xlab("Intentional degree\n(z-score)")+
  ylab("Density")
fig_2b
ggsave(filename = here("fig/2b.png"), plot = fig_2b, width = 2.75, height = 2.75)

## C. Correlation between measured and intentional sociality
fig_2c <- linked %>%
  ungroup() %>%
  filter(situ == "Fe") %>%
  ggplot(aes(x = normDegree, y = z_deg))+
  geom_point(pch = 1, size = 2, alpha = 0.6, col = situcolors[1])+
  theme_classic()+
  theme(text = element_text(family = "Verdana", size = 14))+
  geom_smooth(method = "lm", col = situcolors[1])+
  ylab("Intentional") + xlab("Observed")
fig_2c # we see that these are correlated, but not perfectly. In particular, individuals with low normalized degree
ggsave(filename = here("fig/2c.png"), fig_2c, width = 2.75, height = 2.75)

## D. Example scatter and forest plots
deg_space_situ_obs <- as.data.frame(ggeffect(sp_deg_obs_mod, 
                                            terms = c("space_use", "situ"))) %>%
  mutate(mod = "obs")
deg_space_situ_int <- as.data.frame(ggeffect(sp_deg_int_mod, 
                                              terms = c("space_use", "situ"))) %>%
  mutate(mod = "int")

eff1 <- as.data.frame(emmeans::emtrends(sp_deg_obs_mod, specs = "situ", var = "space_use")) %>% 
  mutate(situ = case_when(situ == "Fe" ~ "Feeding",
                          situ == "Fl" ~ "Flight",
                          situ == "Ro" ~ "Roosting")) %>%
  mutate(mod = "Observed")
eff2 <- as.data.frame(emmeans::emtrends(sp_deg_int_mod, specs = "situ", var = "space_use")) %>% 
  mutate(situ = case_when(situ == "Fe" ~ "Feeding",
                          situ == "Fl" ~ "Flight",
                          situ == "Ro" ~ "Roosting")) %>%
  mutate(mod = "Intentional")

eff <- bind_rows(eff1, eff2) %>%
  mutate(mod = factor(mod, levels = c("Observed", "Intentional")))


annot <- eff %>%
  mutate(sig = case_when(lower.CL < 0 & upper.CL < 0 ~ T,
                         lower.CL > 0 & upper.CL > 0 ~ T,
                         .default = F)) %>%
  select(situ, mod, sig)

# combine the data and scale it
dat <- bind_rows(deg_space_situ_obs, deg_space_situ_int)
mn_moddeg <- (deg_space_situ_obs %>% select(predicted, conf.low, conf.high) %>% rowSums() %>% sum())/(3*nrow(deg_space_situ_obs))
mn_moddeg_z <- (deg_space_situ_int %>% select(predicted, conf.low, conf.high) %>% rowSums() %>% sum())/(3*nrow(deg_space_situ_int))

dat <- dat %>%
  mutate(mod = case_when(mod == "obs" ~ "Observed",
                         mod == "int" ~ "Intentional")) %>%
  mutate(mn = ifelse(mod == "Observed", mn_moddeg, mn_moddeg_z)) %>%
  mutate(predicted_scl = predicted-mn,
         conf.low_scl = conf.low-mn,
         conf.high_scl = conf.high-mn) %>%
  mutate(group = case_when(group == "Fe" ~ "Feeding",
                           group == "Fl" ~ "Flight",
                           group == "Ro" ~ "Roosting")) %>%
  left_join(annot, by = c("group" = "situ",
                          "mod")) %>%
  mutate(mod = factor(mod, levels = c("Observed", "Intentional")))

fig_2d <- dat %>%
  ggplot(aes(x = x, y = predicted, group = group))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group, col = NULL), alpha = 0.1)+
  geom_line(linewidth = 1.5, aes(col = group, linetype = sig))+
  scale_linetype_manual(values = c(2, 1), guide = "none")+
  facet_wrap(~mod, scale = "free_y")+
  scale_color_manual(name = "Situation",
                     values = situcolors)+
  scale_fill_manual(name = "Situation", 
                    values = situcolors)+
  xlab("Space use")+
  ylab("Model prediction")+
  theme(text = element_text(family = "Verdana", size = 14), 
        legend.position = "right",
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA))
fig_2d
ggsave(filename = here("fig/2d.png"), fig_2d, width = 6, height = 4)

## E. Example forest plot with both types of lines
fig_2e <- eff %>%
  ggplot(aes(x = space_use.trend, y = situ, col = situ,
             group = interaction(mod, situ)))+
  geom_vline(aes(xintercept = 0), linetype = 2)+
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL),
                width = 0, linewidth = 0.5,
                position = position_dodge(width = 0.2))+
  geom_point(size = 3, 
             position = position_dodge(width = 0.2),
             fill = "white", aes(shape = mod))+
  scale_color_manual(values = situcolors, guide = "none")+
  scale_y_discrete(limits = rev, position = "right")+
  scale_shape_manual(name = "", values = c(16, 21))+
  facet_wrap(~mod, scales = "free_x",
             ncol = 2)+
  ylab("Space use effect")+ xlab("Effect size")+
  theme(text = element_text(family = "Verdana", size = 14),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 11),
        legend.position = "none")
fig_2e
ggsave(filename = here("fig/2e.png"), fig_2e, width = 4, height = 4)

# Figure 3 ----------------------------------------------------------------
# Main results
# Degree and strength, space use only.
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

deg_space <- prepdata_forforestplots(variable = "space_use", mod = sp_deg_obs_mod,
                                     z_mod = sp_deg_int_mod) %>%
  mutate(measure = "degree")
str_space <- prepdata_forforestplots(variable = "space_use", mod = sp_str_obs_mod,
                                     z_mod = sp_str_int_mod) %>%
  mutate(measure = "strength")
forforest <- list(deg_space, str_space)
forforest <- map(forforest, ~.x %>% rename("trend" = 2))
forforest_df <- purrr::list_rbind(forforest) %>%
  mutate(var = case_when(var == "movement" ~ "Movement",
                         var == "space_use" ~ "Space use"))

plots <- map(forforest, ~{
  var <- str_to_sentence(str_replace(.x$var[1], "_", " "))
  measure <- str_to_sentence(.x$measure[1])
  title <- paste0(var, " effect")
  p <- .x %>%
    ggplot(aes(x = trend, y = situ, col = situ,
               group = interaction(mod, situ)))+
    geom_vline(aes(xintercept = 0), linetype = 2)+
    geom_errorbar(aes(xmin = lower, xmax = upper),
                  width = 0, linewidth = 0.5,
                  position = position_dodge(width = 0.2))+
    geom_point(size = 3, 
               position = position_dodge(width = 0.2),
               fill = "white")+
    scale_color_manual(values = situcolors, guide = "none")+
    scale_y_discrete(limits = rev, position = "right")+
    scale_shape_manual(name = "", values = c(16, 21))+
    facet_wrap(~mod, scales = "free_x",
               nrow = 1)+
    xlab(title)+
    theme(text = element_text(size = 14),
          axis.title.y = element_blank())
  return(p)
})

patchwork <- ((plots[[1]]+
                 ggtitle("Degree")+
                 theme(axis.text.y = element_blank(),
                       axis.ticks.y = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       axis.title.x = element_blank())) + 
                (plots[[2]] + ggtitle("Strength")+
                   theme(plot.title = element_text(hjust = 0.5),
                         axis.title.x = element_blank())))
patchwork
ggsave(patchwork, filename = here("fig/3a.png"), width = 7, height = 3)

# Scatterplots ------------------------------------------------------------
test <- preds %>%
  filter(response == "degree")

points <- linked %>%
  select(z_deg, z_str, "group" = situ, normDegree, normStrength, "x" = space_use) %>%
  pivot_longer(cols = c("z_deg", "z_str", "normDegree", "normStrength"), names_to = "mod", values_to = "predicted") %>%
  mutate(response = case_when(str_detect(mod, "eg") ~ "degree",
                              str_detect(mod, "tr") ~ "strength"),
         mod = case_when(str_detect(mod, "z_") ~ "Intentional",
                         .default = "Observed"),
         mod = factor(mod, levels = c("Observed", "Intentional"))) %>%
  mutate(group = case_when(group == "Fe" ~ "Feeding",
                           group == "Fl" ~ "Flight",
                           group == "Ro" ~ "Roosting"))
  
pts <- points %>% 
  ungroup() %>%
  group_by(mod, response) %>%
  group_split()

preds_list <- preds %>%
  group_by(mod, response) %>%
  group_split() %>%
  map(., ~.x %>% mutate(sig = factor(sig, levels = c(F, T))))

map_dfr(pts, ~.x %>% select(response, mod) %>% distinct())
map_dfr(preds_list, ~.x %>% select(response, mod) %>% distinct()) # same; good

labs <- c("Degree (normalized)", "Strength (normalized)", "Degree (deviation from expected)", "Strength (deviation from expected)")
titles <- c("Observed", "Observed", "Intentional", "Intentional")

plts <- pmap(list(a = preds_list, b = pts, c = labs, d = titles, e = pts), .f = function(a, b, c, d, e){
  p <- a %>%
    ggplot(aes(x = x, y = predicted, col = group))+
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group, col = NULL),
                alpha = 0.2)+
    geom_line(aes(linetype = sig), show.legend = T)+
    scale_linetype_manual(drop = FALSE, values = c(2, 1))+
    scale_color_manual(name = "Situation", values = situcolors)+
    scale_fill_manual(name = "Situation", values = situcolors)+
    labs(x = "Space use", y = c, title = d)+
    geom_point(data = e, pch = 1, alpha = 0.5)+
    theme(legend.position = "none")
  return(p)
})
a <- plts[[1]]
b <- plts[[2]]
c <- plts[[3]]
d <- plts[[4]]


(a + b)/(c + d)
